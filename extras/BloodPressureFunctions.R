downloadBloodPressureData <- function(connectionDetails,
                                      indicationFolder,
                                      bpFolder,
                                      indicationId,
                                      cdmDatabaseSchema,
                                      cohortDatabaseSchema,
                                      tablePrefix,
                                      oracleTempSchema = NULL) {
    if (!file.exists(bpFolder)) {
        dir.create(bpFolder)
    }

    prepareForDataFetch <- function(conn, indicationFolder, indicationId, cohortDatabaseSchema, tablePrefix, oracleTempSchema) {
        pairedCohortTable <- paste(tablePrefix, tolower(indicationId), "pair_cohort", sep = "_")
        cohortsFolder <- file.path(indicationFolder, "allCohorts")
        exposureSummary <- read.csv(file.path(indicationFolder,
                                              "pairedExposureSummaryFilteredBySize.csv"))
        table <- exposureSummary[, c("targetId", "comparatorId")]
        colnames(table) <- SqlRender::camelCaseToSnakeCase(colnames(table))
        DatabaseConnector::insertTable(connection = conn,
                                       tableName = "#comparisons",
                                       data = table,
                                       dropTableIfExists = TRUE,
                                       createTable = TRUE,
                                       tempTable = TRUE,
                                       oracleTempSchema = oracleTempSchema)

        sql <- SqlRender::loadRenderTranslateSql("UnionExposureCohorts.sql",
                                                 "Legend",
                                                 dbms = connectionDetails$dbms,
                                                 oracleTempSchema = oracleTempSchema,
                                                 cohort_database_schema = cohortDatabaseSchema,
                                                 paired_cohort_table = pairedCohortTable)
        DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

        sql <- "TRUNCATE TABLE #comparisons; DROP TABLE #comparisons;"
        sql <- SqlRender::translateSql(sql = sql,
                                       targetDialect = connectionDetails$dbms,
                                       oracleTempSchema = oracleTempSchema)$sql
        DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

        # Just to make sure: check of rowIds are consistent with those generated before:
        sql <- "SELECT row_id, subject_id, cohort_start_date FROM #exposure_cohorts"
        sql <- SqlRender::translateSql(sql = sql,
                                       targetDialect = connectionDetails$dbms,
                                       oracleTempSchema = oracleTempSchema)$sql
        newCohorts <- DatabaseConnector::querySql(conn, sql)
        colnames(newCohorts) <- SqlRender::snakeCaseToCamelCase(colnames(newCohorts))
        newCohorts <- newCohorts[order(newCohorts$rowId,
                                       newCohorts$subjectId,
                                       newCohorts$cohortStartDate), ]
        row.names(newCohorts) <- NULL
        allCohorts <- readRDS(file.path(cohortsFolder, "allCohorts.rds"))
        allCohorts <- allCohorts[, colnames(newCohorts)]
        allCohorts <- unique(allCohorts)
        allCohorts <- allCohorts[order(allCohorts$rowId,
                                       allCohorts$subjectId,
                                       allCohorts$cohortStartDate), ]
        row.names(allCohorts) <- NULL
        if (!all.equal(allCohorts, newCohorts)) {
            stop("row IDs have changed. Hot swap failed")
        }
        ParallelLogger::logInfo("Verified that rowIds are the same")
    }

    conn <- DatabaseConnector::connect(connectionDetails)

    prepareForDataFetch(conn, indicationFolder, indicationId, cohortDatabaseSchema, tablePrefix, oracleTempSchema)

    sql <- "SELECT row_id,
    value_as_number,
    measurement_date,
    concept_id,
    concept_name
FROM @cdm_database_schema.measurement
INNER JOIN @cdm_database_schema.concept
ON measurement_concept_id = concept_id
INNER JOIN #exposure_cohorts
ON person_id = subject_id
AND measurement_date <= cohort_start_date
AND measurement_date > DATEADD(DAY, -365, cohort_start_date)
WHERE concept_id IN (3004249, 3012888)"
    sql <- SqlRender::renderSql(sql, cdm_database_schema = cdmDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    bps <- DatabaseConnector::querySql(conn, sql)
    colnames(bps) <- SqlRender::snakeCaseToCamelCase(colnames(bps))
    DatabaseConnector::disconnect(conn)

    bps <- bps[!is.na(bps$valueAsNumber), ]
    bps <- bps[order(bps$rowId, bps$measurementDate, decreasing = TRUE), ]
    bps <- bps[!duplicated(paste(bps$rowId, bps$conceptId)), ]
    saveRDS(bps, file.path(bpFolder, "bps.rds"))
}

plotBalance <- function(row, indicationFolder, bpFolder) {
    # row <- tcs[1, ]
    bpFolder <- file.path(indicationFolder, "bp")
    bps <- readRDS(file.path(bpFolder, "bps.rds"))
    bps <- bps[bps$valueAsNumber < 250, ]
    bps <- bps[bps$valueAsNumber > 25, ]
    outcomeModelReference <- readRDS(file.path(indicationFolder,
                                               "cmOutput",
                                               "outcomeModelReference1.rds"))
    outcomeModelReference <- outcomeModelReference[outcomeModelReference$targetId == row$targetId &
                                                       outcomeModelReference$comparatorId == row$comparatorId &
                                                       outcomeModelReference$analysisId == 1, ]
    outcomeModelReference <- outcomeModelReference[1, ]
    cmData <- CohortMethod::loadCohortMethodData(file.path(indicationFolder,
                                                           "cmOutput",
                                                           outcomeModelReference$cohortMethodDataFolder),
                                                 skipCovariates = TRUE)
    sharedPs <- readRDS(file.path(indicationFolder, "cmOutput", outcomeModelReference$sharedPsFile))
    strataPop <- CohortMethod::stratifyByPs(sharedPs, numberOfStrata = 10)
    if (nrow(strataPop) == 0) {
        return(NULL)
    }

    resultRow <- row[, c("targetId", "targetName" ,"comparatorId", "comparatorName")]
    resultRow$targetSubjects <- sum(strataPop$treatment == 1)
    resultRow$comparatorSubjects <- sum(strataPop$treatment == 0)

    # Restricting before cohort to just those in the after cohort:
    subset <- bps[bps$rowId %in% strataPop$rowId, ]
    strataPop <- strataPop[strataPop$rowId %in% subset$rowId, ]
    cmData$cohorts <- cmData$cohorts[cmData$cohorts$rowId %in% subset$rowId, ]

    resultRow$targetSubjectsWithBp <- sum(strataPop$treatment == 1)
    resultRow$comparatorSubjectsWithBp <- sum(strataPop$treatment == 0)

    # Density plot with custom smoothing:
    m <- merge(subset, strataPop[, c("rowId", "treatment", "stratumId")])
    m$group <- as.character(row$targetName)
    m$group[m$treatment == 0] <- as.character(row$comparatorName)
    m$group <- factor(m$group, levels = c(as.character(row$targetName),
                                          as.character(row$comparatorName)))
    m$conceptName[m$conceptName == "BP diastolic"] <- "Diastolic"
    m$conceptName[m$conceptName == "BP systolic"] <- "Systolic"

    # Save for Marc:
    fileName <- file.path(bpFolder, sprintf("BpData_%s_%s.rds", as.character(row$targetName), as.character(row$comparatorName)))
    saveRDS(m, fileName)

    before <- data.frame()
    after <- data.frame()
    for (conceptName in c("Diastolic", "Systolic")) {
        for (group in c(as.character(row$targetName), as.character(row$comparatorName))) {
            d <- density(m$valueAsNumber[m$conceptName == conceptName & m$group == group], bw = 5, from = 50, to = 250, n = 210)
            before <- rbind(before,
                            data.frame(x = d$x,
                                       y = d$y,
                                       conceptName = conceptName,
                                       group = group))
            for (stratumId in unique(m$stratumId)) {
                d <- density(m$valueAsNumber[m$conceptName == conceptName & m$group == group & m$stratumId == stratumId], bw = 5, from = 50, to = 250, n = 210)
                after <- rbind(after,
                               data.frame(x = d$x,
                                          y = d$y,
                                          conceptName = conceptName,
                                          group = group,
                                          stratumId = stratumId))
            }
        }
    }
    plotBp <- function(vizData, stratified, fileName) {
        plot <- ggplot2::ggplot(vizData, ggplot2::aes(x = x, y = y, group = group, color = group, fill = group)) +
            ggplot2::geom_area(position = "identity") +
            ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
            ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
            ggplot2::xlab("Blood pressure") +
            ggplot2::ylab("Density") +
            ggplot2::xlim(c(50,200)) +
            ggplot2::theme(legend.position = "top",
                           legend.title = ggplot2::element_blank())
        if (stratified) {
            plot <- plot + ggplot2::facet_grid(conceptName~stratumId)
        } else {
            plot <- plot + ggplot2::facet_grid(conceptName~.)
        }
        width <- if (stratified) {11} else {5}
        ggplot2::ggsave(fileName, plot, width = width, height = 4, dpi = 400)
    }
    plotBp(before, stratified = FALSE, fileName = file.path(bpFolder, sprintf("Before_%s_%s.png", as.character(row$targetName), as.character(row$comparatorName))))
    plotBp(after, stratified = TRUE, fileName = file.path(bpFolder, sprintf("After_%s_%s.png", as.character(row$targetName), as.character(row$comparatorName))))

    # Flip T and C:
    before$group <- factor(before$group, levels = c(as.character(row$comparatorName), as.character(row$targetName)))
    after$group <- factor(after$group, levels = c(as.character(row$comparatorName), as.character(row$targetName)))
    plotBp(before, stratified = FALSE, fileName = file.path(bpFolder, sprintf("Before_%s_%s.png", as.character(row$comparatorName), as.character(row$targetName))))
    plotBp(after, stratified = TRUE, fileName = file.path(bpFolder, sprintf("After_%s_%s.png", as.character(row$comparatorName), as.character(row$targetName))))

    # Compute balance
    cmData$covariates <- ff::as.ffdf(data.frame(rowId = subset$rowId,
                                                covariateId = subset$conceptId,
                                                covariateValue = subset$valueAsNumber))
    subsetRef <- unique(subset[, c("conceptId", "conceptName")])
    cmData$covariateRef <- ff::as.ffdf(data.frame(covariateId = subsetRef$conceptId,
                                                  covariateName = subsetRef$conceptName))
    bal <- CohortMethod::computeCovariateBalance(strataPop, cmData)
    resultRow <- merge(resultRow, bal)
    # balanceFile <- file.path(bpFolder, sprintf("Balance_%s_%s.csv", as.character(row$targetName), as.character(row$comparatorName)))
    # write.csv(bal, balanceFile, row.names = FALSE)
    return(resultRow)
}

refitPropensityModel <- function(row, indicationFolder, bpFolder) {
    # row <- tcs[3, ]
    bpFolder <- file.path(indicationFolder, "bp")
    bps <- readRDS(file.path(bpFolder, "bps.rds"))
    bps <- bps[bps$valueAsNumber < 250, ]
    bps <- bps[bps$valueAsNumber > 25, ]
    outcomeModelReference <- readRDS(file.path(indicationFolder,
                                               "cmOutput",
                                               "outcomeModelReference1.rds"))
    outcomeModelReference <- outcomeModelReference[outcomeModelReference$targetId == row$targetId &
                                                       outcomeModelReference$comparatorId == row$comparatorId &
                                                       outcomeModelReference$analysisId == 1, ]
    outcomeModelReference <- outcomeModelReference[1, ]

    if (!file.exists(file.path(bpFolder, outcomeModelReference$cohortMethodDataFolder))) {
        # Create new CohortMethodData object, restricting to people with BP data, and adding BP as splines
        cmData <- CohortMethod::loadCohortMethodData(file.path(indicationFolder,
                                                               "cmOutput",
                                                               outcomeModelReference$cohortMethodDataFolder))
        subset <- bps[bps$rowId %in% cmData$cohorts$rowId, ]
        # Convert to splines:
        newCovars <- data.frame()
        newCovarRef <- data.frame()
        for (conceptId in unique(subset$conceptId)) {
            # conceptId <- 3012888
            measurement <- subset[subset$conceptId == conceptId, ]
            designMatrix <- splines::bs(measurement$valueAsNumber, 5)
            sparse <- lapply(1:5, function(x) data.frame(rowId = measurement$rowId,
                                                         covariateId = conceptId * 10 + x,
                                                         covariateValue = designMatrix[, x]))

            sparse <- do.call("rbind", sparse)
            newCovars <- rbind(newCovars, sparse)
            ref <- data.frame(covariateId = conceptId * 10 + 1:5,
                              covariateName = paste(measurement$conceptName[1], 1:5),
                              analysisId = conceptId,
                              conceptId = conceptId)
            newCovarRef <- rbind(newCovarRef, ref)
        }
        covariates <- cmData$covariates
        covariates <- covariates[ffbase::`%in%`(covariates$rowId, subset$rowId), ]
        covariates <- ffbase::ffdfappend(covariates, newCovars)
        covariateRef <- cmData$covariateRef
        covariateRef <- ffbase::ffdfappend(covariateRef, newCovarRef)
        newCmData <- cmData
        newCmData$cohorts <- newCmData$cohorts[newCmData$cohorts$rowId %in% subset$rowId, ]
        attrition <- attr(newCmData$cohorts, "metaData")$attrition
        attrition <- rbind(attrition,
                           data.frame(description = "Having BP data",
                                      targetPersons =  sum(newCmData$cohort$treatment == 1),
                                      comparatorPersons =  sum(newCmData$cohort$treatment == 0),
                                      targetExposures =  sum(newCmData$cohort$treatment == 1),
                                      comparatorExposures =  sum(newCmData$cohort$treatment == 0)))
        attr(newCmData$cohorts, "metaData")$attrition <- attrition
        newCmData$outcomes <- newCmData$outcomes[newCmData$outcomes$rowId %in% subset$rowId, ]
        newCmData$covariates <- covariates
        newCmData$covariateRef <- covariateRef
        CohortMethod::saveCohortMethodData(cohortMethodData = newCmData,
                                           file = file.path(bpFolder,outcomeModelReference$cohortMethodDataFolder),
                                           compress = TRUE)
        rm(cmData)
        cmData <- newCmData
    } else {
        cmData <- CohortMethod::loadCohortMethodData(file.path(bpFolder, outcomeModelReference$cohortMethodDataFolder))
        subset <- bps[bps$rowId %in% cmData$cohorts$rowId, ]
    }

    if (!file.exists(file.path(bpFolder, outcomeModelReference$sharedPsFile))) {
        # Fit propensity model:
        subgroupCovariateIds <- c(1998, 2998, 3998, 4998, 5998, 6998, 7998, 8998)
        studyPop <- CohortMethod::createStudyPopulation(cohortMethodData = cmData,
                                                        removeDuplicateSubjects = "keep first",
                                                        removeSubjectsWithPriorOutcome = TRUE,
                                                        riskWindowStart = 1,
                                                        riskWindowEnd = 0,
                                                        addExposureDaysToEnd = TRUE,
                                                        minDaysAtRisk = 1,
                                                        censorAtNewRiskWindow = TRUE)
        ps <- CohortMethod::createPs(population = studyPop,
                                     cohortMethodData = cmData,
                                     control = Cyclops::createControl(noiseLevel = "quiet",
                                                                      cvType = "auto",
                                                                      tolerance = 2e-07,
                                                                      cvRepetitions = 1,
                                                                      fold = 10,
                                                                      startingVariance = 0.01,
                                                                      seed = 123,
                                                                      threads = 10),
                                     stopOnError = TRUE,
                                     excludeCovariateIds = subgroupCovariateIds,
                                     maxCohortSizeForFitting = 1e+05)
        saveRDS(ps, file.path(bpFolder, outcomeModelReference$sharedPsFile))
    } else {
        ps <- readRDS(file.path(bpFolder, outcomeModelReference$sharedPsFile))
    }
    if (!file.exists(file.path(bpFolder, outcomeModelReference$strataFile))) {
        studyPop <- CohortMethod::createStudyPopulation(population = ps,
                                                        removeDuplicateSubjects = "keep first",
                                                        removeSubjectsWithPriorOutcome = TRUE,
                                                        riskWindowStart = 1,
                                                        riskWindowEnd = 0,
                                                        addExposureDaysToEnd = TRUE,
                                                        minDaysAtRisk = 1,
                                                        censorAtNewRiskWindow = TRUE)
        strataPop <- CohortMethod::stratifyByPs(studyPop, numberOfStrata = 10, baseSelection = "all")
        saveRDS(strataPop, file.path(bpFolder, outcomeModelReference$strataFile))
    } else {
        strataPop <- readRDS(file.path(bpFolder, outcomeModelReference$strataFile))
    }

    if (!file.exists(file.path(bpFolder, sprintf("BalanceAfterStrataUsingBp_%s_%s.png", row$targetName, row$comparatorName)))) {
        # Overall balance:
        bal <- CohortMethod::computeCovariateBalance(strataPop, cmData)
        CohortMethod::plotCovariateBalanceScatterPlot(bal, fileName = file.path(bpFolder, sprintf("BalanceAfterStrataUsingBp_%s_%s.png", row$targetName, row$comparatorName)))
        CohortMethod::plotCovariateBalanceOfTopVariables(bal, fileName = file.path(bpFolder, sprintf("BalanceTopAfterStrataUsingBp_%s_%s.png", row$targetName, row$comparatorName)))
    }
    ff::close.ffdf(cmData$covariates)
    ff::close.ffdf(cmData$covariateRef)
    cmData$covariates <- ff::as.ffdf(data.frame(rowId = subset$rowId,
                                                covariateId = subset$conceptId,
                                                covariateValue = subset$valueAsNumber))
    subsetRef <- unique(subset[, c("conceptId", "conceptName")])
    cmData$covariateRef <- ff::as.ffdf(data.frame(covariateId = subsetRef$conceptId,
                                                  covariateName = subsetRef$conceptName))
    bal <- CohortMethod::computeCovariateBalance(strataPop, cmData)
    resultRow <- row[, c("targetId", "targetName" ,"comparatorId", "comparatorName")]
    resultRow <- merge(resultRow, bal)
    return(resultRow)
}

computeAdjustedHrs <- function(row, indicationFolder, bpFolder) {
    # row <- tcs[1,]
    bps <- readRDS(file.path(bpFolder, "bps.rds"))
    bps <- bps[bps$valueAsNumber < 250, ]
    bps <- bps[bps$valueAsNumber > 25, ]
    outcomeModelReference1 <- readRDS(file.path(indicationFolder,
                                                "cmOutput",
                                                "outcomeModelReference1.rds"))
    outcomeModelReference2 <- readRDS(file.path(indicationFolder,
                                                "cmOutput",
                                                "outcomeModelReference2.rds"))
    outcomeModelReference3 <- readRDS(file.path(indicationFolder,
                                                "cmOutput",
                                                "outcomeModelReference3.rds"))
    outcomeModelReference <- rbind(outcomeModelReference1, outcomeModelReference2, outcomeModelReference3)

    # outcomeModelReference <- readRDS(file.path(indicationFolder,
    #                                            "cmOutput",
    #                                            "outcomeModelReference1.rds"))
    onTreatment <- outcomeModelReference[outcomeModelReference$targetId == row$targetId &
                                                       outcomeModelReference$comparatorId == row$comparatorId &
                                                       outcomeModelReference$analysisId == 1, ]
    analysisFolder <- file.path(bpFolder, "Analysis_1")
    if (!file.exists(analysisFolder)) {
        dir.create(analysisFolder)
    }
    ps <- readRDS(file.path(bpFolder, onTreatment$sharedPsFile[1]))
    cmData <- CohortMethod::loadCohortMethodData(file.path(bpFolder, onTreatment$cohortMethodDataFolder[1]))


    # onTreatment <- onTreatment[onTreatment$outcomeId %in% outcomesOfInterest$cohortId, ]

    computeHr <- function(i) {
        # i = 1
        omFile <- file.path(bpFolder, onTreatment$outcomeModelFile[i])
        if (!file.exists(omFile)) {
            studyPop <- CohortMethod::createStudyPopulation(population = ps,
                                                            cohortMethodData = cmData,
                                                            outcomeId = onTreatment$outcomeId[i],
                                                            removeDuplicateSubjects = "keep first",
                                                            removeSubjectsWithPriorOutcome = TRUE,
                                                            riskWindowStart = 1,
                                                            riskWindowEnd = 0,
                                                            addExposureDaysToEnd = TRUE,
                                                            minDaysAtRisk = 1,
                                                            censorAtNewRiskWindow = TRUE)
            strataPop <- CohortMethod::stratifyByPs(studyPop, numberOfStrata = 10, baseSelection = "all")
            outcomeModel <- CohortMethod::fitOutcomeModel(population = strataPop,
                                                          stratified = TRUE,
                                                          modelType = "cox")
            saveRDS(outcomeModel, omFile)
        }
    }
    plyr::l_ply(1:nrow(onTreatment), computeHr, .progress = "text")


    originalSummary <- CohortMethod::summarizeAnalyses(onTreatment, file.path(indicationFolder, "cmOutput"))
    originalSummary$type <- "Original"
    bpAdjustedSummary <- CohortMethod::summarizeAnalyses(onTreatment, bpFolder)
    bpAdjustedSummary$type <- "Adjusting for\nblood pressure"
    estimates <- rbind(originalSummary, bpAdjustedSummary)

    # Calibration ------------------------------------------
    pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Legend")
    outcomesOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)
    outcomesOfInterest <- outcomesOfInterest[outcomesOfInterest$indicationId == indicationId, ]
    pathToCsv <- file.path(indicationFolder, "signalInjectionSummary.csv")
    siSummary <- read.csv(pathToCsv)
    siSummary <- siSummary[siSummary$newOutcomeId %in% estimates$outcomeId, ]
    pcs <- data.frame(outcomeId = siSummary$newOutcomeId, targetEffectSize = siSummary$trueEffectSize)
    pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Legend")
    negativeControls <- read.csv(pathToCsv)
    negativeControls <- negativeControls[negativeControls$indicationId == indicationId, ]

    estimates <- merge(estimates,
                       data.frame(outcomeId = siSummary$newOutcomeId,
                                  targetEffectSize = siSummary$targetEffectSize),
                       all.x = TRUE)
    estimates$targetEffectSize[estimates$outcomeId %in% negativeControls$cohortId] <- 1

    tcEstimates <- estimates[estimates$outcomeId %in% outcomesOfInterest$cohortId |
                                 estimates$outcomeId %in% negativeControls$cohortId |
                                 estimates$outcomeId  %in% siSummary$newOutcomeId[siSummary$exposureId == row$targetId], ]

    ctEstimates <- estimates[estimates$outcomeId %in% outcomesOfInterest$cohortId |
                                 estimates$outcomeId %in% negativeControls$cohortId |
                                 estimates$outcomeId  %in% siSummary$newOutcomeId[siSummary$exposureId == row$comparatorId], ]
    temp <- ctEstimates$targetId
    ctEstimates$targetId <- ctEstimates$comparatorId
    ctEstimates$comparatorId <- temp
    temp <- ctEstimates$target
    ctEstimates$target <- ctEstimates$comparator
    ctEstimates$comparator <- temp
    temp <- ctEstimates$targetDays
    ctEstimates$targetDays <- ctEstimates$comparatorDays
    ctEstimates$comparatorDays <- temp
    temp <- ctEstimates$eventsTarget
    ctEstimates$eventsTarget <- ctEstimates$eventsComparator
    ctEstimates$eventsComparator <- temp
    ctEstimates$logRr <- -ctEstimates$logRr
    ctEstimates$rr <- 1/ctEstimates$r
    temp <- 1/ctEstimates$ci95ub
    ctEstimates$ci95ub <- 1/ctEstimates$ci95lb
    ctEstimates$ci95lb <- temp

    calibrate <- function(estimates) {
        controlEstimates <- estimates[!is.na(estimates$targetEffectSize), ]
        controlEstimates <- controlEstimates[!is.na(controlEstimates$seLogRr), ]
        errorModel <- EmpiricalCalibration::fitSystematicErrorModel(logRr = controlEstimates$logRr,
                                                                    seLogRr = controlEstimates$seLogRr,
                                                                    trueLogRr = log(controlEstimates$targetEffectSize))
        # EmpiricalCalibration::plotCiCalibrationEffect(logRr = controlEstimates$logRr,
        #                                               seLogRr = controlEstimates$seLogRr,
        #                                               trueLogRr = log(controlEstimates$targetEffectSize))
        cal <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = estimates$logRr,
                                                                        seLogRr = estimates$seLogRr,
                                                                        model = errorModel)
        calibrated <- estimates
        calibrated$rr <- exp(cal$logRr)
        calibrated$ci95lb <- exp(cal$logLb95Rr)
        calibrated$ci95ub <- exp(cal$logUb95Rr)
        calibrated$logLb95Rr <- NULL
        calibrated$logUb95Rr <- NULL
        calibrated$outcomeId <- estimates$outcomeId
        calibrated$type <- estimates$type
        calibrated$estimate <- "Calibrated"
        estimates$estimate <- "Uncalibrated"
        estimates <- rbind(estimates[, colnames(calibrated)], calibrated)
    }
    estimates <- rbind(calibrate(tcEstimates[tcEstimates$type == "Original", ]),
                       calibrate(tcEstimates[tcEstimates$type == "Adjusting for\nblood pressure", ]),
                       calibrate(ctEstimates[tcEstimates$type == "Original", ]),
                       calibrate(ctEstimates[tcEstimates$type == "Adjusting for\nblood pressure", ]))

    vizData <- merge(estimates, data.frame(outcomeId = outcomesOfInterest$cohortId,
                                           outcomeName = outcomesOfInterest$name))
    vizData <- vizData[!is.na(vizData$seLogRr), ]
    outcomeNames <- unique(vizData$outcomeName)
    outcomeNames <- outcomeNames[order(outcomeNames, decreasing = TRUE)]
    vizData$y <- match(vizData$outcomeName, outcomeNames) - 0.1 + 0.2*(vizData$type == "Original")

    # Save for Marc:
    fileName <- file.path(bpFolder, sprintf("HrsData_%s_%s.rds", row$targetName, row$comparatorName))
    saveRDS(vizData, fileName)

    plotHrs <- function(vizData, fileName) {
        breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 8, 10)
        odd <- seq(1, length(outcomeNames), by = 2)
        bars <- data.frame(xmin = 0.01,
                           xmax = 100,
                           ymin = odd - 0.5,
                           ymax = odd + 0.5,
                           type = "Original",
                           rr = 1,
                           y = 1)
        vizData$estimate <- factor(vizData$estimate, levels = c("Uncalibrated", "Calibrated"))
        plot <- ggplot2::ggplot(vizData, ggplot2::aes(x = rr, y = y, color = type, shape = type, xmin = ci95lb, xmax = ci95ub)) +
            ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#EEEEEE", colour = "#EEEEEE", size = 0, data = bars) +
            ggplot2::geom_vline(xintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.25) +
            ggplot2::geom_vline(xintercept = 1, size = 0.5) +
            ggplot2::geom_errorbarh(alpha = 0.65, size = 0.6, height = 0.3) +
            ggplot2::geom_point(alpha = 0.65, size = 2) +
            ggplot2::scale_y_continuous(breaks = 1:length(outcomeNames), labels = outcomeNames) +
            ggplot2::coord_cartesian(xlim = c(0.1, 10)) +
            ggplot2::scale_x_log10(breaks = breaks, labels = breaks) +
            ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.65), rgb(0, 0, 0.8, alpha = 0.65))) +
            ggplot2::xlab("Hazard Ratio") +
            ggplot2::facet_grid(~estimate) +
            ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                           legend.title = ggplot2::element_blank(),
                           panel.grid.minor = ggplot2::element_blank(),
                           panel.background = ggplot2::element_blank(),
                           panel.grid.major = ggplot2::element_blank(),
                           axis.ticks = ggplot2::element_blank(),
                           strip.background = ggplot2::element_blank())
        ggplot2::ggsave(filename = fileName, plot = plot, width = 8, height = 10, dpi = 400)
        # ggplot2::ggsave(filename = fileName, plot = plot, width = 6.5, height = 10, dpi = 400)
    }
    plotHrs(vizData[vizData$targetId == row$targetId, ], file.path(bpFolder, sprintf("Hrs_%s_%s.png", row$targetName, row$comparatorName)))

    plotHrs(vizData[vizData$targetId == row$comparatorId, ], file.path(bpFolder, sprintf("Hrs_%s_%s.png", row$comparatorName, row$targetName)))
}

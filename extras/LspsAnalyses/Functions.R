downloadBloodPressureData <- function(connectionDetails,
                                      indicationFolder,
                                      lspsFolder,
                                      indicationId,
                                      cdmDatabaseSchema,
                                      cohortDatabaseSchema,
                                      tablePrefix,
                                      oracleTempSchema = NULL) {
    if (!file.exists(lspsFolder)) {
        dir.create(lspsFolder)
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
    saveRDS(bps, file.path(lspsFolder, "bps.rds"))
}

refitPropensityModel <- function(row, indicationFolder, lspsFolder, analysisId) {
    # row <- tcs[1, ]
    ParallelLogger::logInfo(paste("Refitting propensity model for", row$targetName, "and", row$comparatorName))
    bps <- readRDS(file.path(lspsFolder, "bps.rds"))
    bps <- bps[bps$valueAsNumber < 250, ]
    bps <- bps[bps$valueAsNumber > 25, ]
    outcomeModelReference <- readRDS(file.path(lspsFolder, "outcomeModelReference.rds"))
    outcomeModelReference <- outcomeModelReference[outcomeModelReference$targetId == row$targetId &
                                                       outcomeModelReference$comparatorId == row$comparatorId &
                                                       outcomeModelReference$analysisId == analysisId, ]
    outcomeModelReference <- outcomeModelReference[outcomeModelReference$strataFile != "", ][1, ]

    if (!file.exists(file.path(lspsFolder, outcomeModelReference$cohortMethodDataFolder))) {
        # Create new CohortMethodData object, restricting to people with BP data, and adding BP as splines
        cmData <- CohortMethod::loadCohortMethodData(file.path(indicationFolder,
                                                               "cmOutput",
                                                               outcomeModelReference$cohortMethodDataFolder))
        subset <- bps[bps$rowId %in% cmData$cohorts$rowId, ]
        # Convert to splines:
        newCovars <- data.frame()
        newCovarRef <- data.frame()
        for (conceptId in unique(subset$conceptId)) {
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
                                           file = file.path(lspsFolder,outcomeModelReference$cohortMethodDataFolder),
                                           compress = TRUE)
        rm(cmData)
        cmData <- newCmData
    } else {
        cmData <- CohortMethod::loadCohortMethodData(file.path(lspsFolder, outcomeModelReference$cohortMethodDataFolder))
        subset <- bps[bps$rowId %in% cmData$cohorts$rowId, ]
    }

    if (!file.exists(file.path(lspsFolder, outcomeModelReference$sharedPsFile))) {
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
                                     stopOnError = FALSE,
                                     excludeCovariateIds = subgroupCovariateIds,
                                     maxCohortSizeForFitting = 1e+05)
        saveRDS(ps, file.path(lspsFolder, outcomeModelReference$sharedPsFile))
    } else {
        ps <- readRDS(file.path(lspsFolder, outcomeModelReference$sharedPsFile))
    }
    if (!file.exists(file.path(lspsFolder, outcomeModelReference$strataFile))) {
        studyPop <- CohortMethod::createStudyPopulation(population = ps,
                                                        removeDuplicateSubjects = "keep first",
                                                        removeSubjectsWithPriorOutcome = TRUE,
                                                        riskWindowStart = 1,
                                                        riskWindowEnd = 0,
                                                        endAnchor = "cohort end",
                                                        minDaysAtRisk = 1,
                                                        censorAtNewRiskWindow = TRUE)
        if (analysisId == 1) {
            strataPop <- CohortMethod::stratifyByPs(studyPop, numberOfStrata = 10, baseSelection = "all")
        } else if (analysisId == 3) {
            strataPop <- CohortMethod::matchOnPs(studyPop, caliper = 0.2, caliperScale = "standardized logit", maxRatio = 100)
        } else {
            stop("Unknown analysis ID ", analysisId)
        }
        saveRDS(strataPop, file.path(lspsFolder, outcomeModelReference$strataFile))
    } else {
        strataPop <- readRDS(file.path(lspsFolder, outcomeModelReference$strataFile))
    }
    if (nrow(strataPop) != 0) {
        if (!file.exists(file.path(lspsFolder, sprintf("BalanceAfterMatchingUsingBp_%s_%s_%s.png", row$targetName, row$comparatorName, analysisId)))) {
            # Overall balance:
            bal <- CohortMethod::computeCovariateBalance(strataPop, cmData)
            CohortMethod::plotCovariateBalanceScatterPlot(bal, fileName = file.path(lspsFolder, sprintf("BalanceAfterStrataUsingBp_%s_%s.png", row$targetName, row$comparatorName)))
            CohortMethod::plotCovariateBalanceOfTopVariables(bal, fileName = file.path(lspsFolder, sprintf("BalanceTopAfterStrataUsingBp_%s_%s.png", row$targetName, row$comparatorName)))

            balanceFile <- file.path(lspsFolder, sprintf("BalanceAdjustBp_%s_%s.csv", as.character(row$targetName), as.character(row$comparatorName)))
            write.csv(bal, balanceFile, row.names = FALSE)
        }

        cmData$covariates <- ff::as.ffdf(data.frame(rowId = subset$rowId,
                                                    covariateId = subset$conceptId,
                                                    covariateValue = subset$valueAsNumber))
        subsetRef <- unique(subset[, c("conceptId", "conceptName")])
        cmData$covariateRef <- ff::as.ffdf(data.frame(covariateId = subsetRef$conceptId,
                                                      covariateName = subsetRef$conceptName))
        bal <- CohortMethod::computeCovariateBalance(strataPop, cmData)
        resultRow <- row[, c("targetId", "targetName" ,"comparatorId", "comparatorName")]
        resultRow <- merge(resultRow, bal)
        ff::close.ffdf(cmData$covariates)
        ff::close.ffdf(cmData$covariateRef)
        return(resultRow)
    } else {
        ff::close.ffdf(cmData$covariates)
        ff::close.ffdf(cmData$covariateRef)
        return(NULL)
    }
}

fitManualPropensityModel <- function(row, indicationFolder, lspsFolder, analysisId) {
    # row <- tcs[1, ]
    ParallelLogger::logInfo(paste("Fitting manual propensity model for", row$targetName, "and", row$comparatorName))
    bps <- readRDS(file.path(lspsFolder, "bps.rds"))
    bps <- bps[bps$valueAsNumber < 250, ]
    bps <- bps[bps$valueAsNumber > 25, ]
    manualFolder <- file.path(lspsFolder, "manual")
    if (!file.exists(manualFolder)) {
        dir.create(manualFolder)
    }
    outcomeModelReference <- readRDS(file.path(lspsFolder, "outcomeModelReference.rds"))
    outcomeModelReference <- outcomeModelReference[outcomeModelReference$targetId == row$targetId &
                                                       outcomeModelReference$comparatorId == row$comparatorId &
                                                       outcomeModelReference$analysisId == analysisId, ]
    outcomeModelReference <- outcomeModelReference[outcomeModelReference$strataFile != "", ][1, ]

    if (!file.exists(file.path(manualFolder, outcomeModelReference$cohortMethodDataFolder))) {
        cmData <- CohortMethod::loadCohortMethodData(file.path(indicationFolder,
                                                               "cmOutput",
                                                               outcomeModelReference$cohortMethodDataFolder))
        # covariateRef <- readRDS("c:/temp/covariateRef.rds")
        # covariateRef[grepl("plegia", covariateRef$covariateName, ignore.case = TRUE), c("covariateId", "covariateName")]

        selectedCovariateIds <- c(0:20*1000 + 3, #Age groups
                                  8532001, # Female,
                                  2000:2020*1000 + 6, # Index year
                                  44054006210, # T2DM
                                  53741008210, # CAD
                                  22298006210, # MI
                                  390798007210, # Asthma
                                  84114007210, # Heart failure
                                  709044004210, # Chronic kidney disease
                                  49436004210, # Atrial fibrillation,
                                  1901, # Charlson index - Romano adaptation
                                  21600985410, # Platelet aggregation inhibitors excl. heparin
                                  1310149410, # Warfarin
                                  21602722410, # Corticosteroids for systemic use
                                  1331270410, # Dipyridamole
                                  21603933410, # NSAIDS
                                  21600095410, # PPIs
                                  21601855410, # Statins
                                  21602514410, # Estrogens
                                  21602537410, # Progestogens
                                  21600712410, # Anti-glycemic agent
                                  4245997802, # BMI
                                  13645005210, # COPD
                                  235856003210, # Liver disease
                                  4159131210, # Dyslipidemia
                                  368009210, # Valvular heart disease
                                  4239381210, # Drug abuse
                                  443392210, # Cancer
                                  43972721) # HIV infection

        covariates <- cmData$covariates[ffbase::`%in%`(cmData$covariates$covariateId, selectedCovariateIds), ]

        smokingCovariateIds <- c(4058137802, 4282779802, 4216174802, 4132133802, 21494888702, 40486518802) # Smoking
        smokingRowIds <- cmData$covariates$rowId[ffbase::`%in%`(cmData$covariates$covariateId, smokingCovariateIds)]
        smokingRowIds <- ff::as.ram(unique(smokingRowIds))
        smokingCovariate <- data.frame(rowId = smokingRowIds,
                                       covariateId = rep(9999, length(smokingRowIds)),
                                       covariateValue = rep(1, length(smokingRowIds)))
        strokeCovariateIds <- c(230692004210, 195189003210, 195190007210, 288723005210) # Stroke
        strokeRowIds <- cmData$covariates$rowId[ffbase::`%in%`(cmData$covariates$covariateId, strokeCovariateIds)]
        strokeRowIds <- ff::as.ram(unique(strokeRowIds))
        strokeCovariate <- data.frame(rowId = strokeRowIds,
                                       covariateId = rep(9998, length(strokeRowIds)),
                                      covariateValue = rep(1, length(strokeRowIds)))
        if (nrow(smokingCovariate) > 0) {
            covariates <- ffbase::ffdfappend(covariates, smokingCovariate)
        }
        if (nrow(strokeCovariate) > 0) {
            covariates <- ffbase::ffdfappend(covariates, strokeCovariate)
        }

        covariateRef <- cmData$covariateRef[ffbase::`%in%`(cmData$covariateRef$covariateId, selectedCovariateIds), ]
        covariateRef <- ffbase::ffdfappend(covariateRef,
                                           data.frame(covariateId = c(9999, 9998),
                                                      covariateName = c("Smoking", "Stroke"),
                                                      analysisId = c(999, 999),
                                                      conceptId = c(0, 0)))



        newCmData <- cmData
        newCmData$covariates <- covariates
        newCmData$covariateRef <- covariateRef
        newCmData$analysisRef <- ff::clone(cmData$analysisRef)
        CohortMethod::saveCohortMethodData(cohortMethodData = newCmData,
                                           file = file.path(manualFolder, outcomeModelReference$cohortMethodDataFolder),
                                           compress = TRUE)
        rm(cmData)
        cmData <- newCmData
    } else {
        cmData <- CohortMethod::loadCohortMethodData(file.path(manualFolder, outcomeModelReference$cohortMethodDataFolder))
        subset <- bps[bps$rowId %in% cmData$cohorts$rowId, ]
    }

    if (!file.exists(file.path(manualFolder, outcomeModelReference$sharedPsFile))) {
        # Fit propensity model:
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
                                     stopOnError = FALSE,
                                     maxCohortSizeForFitting = 1e+05)
        saveRDS(ps, file.path(manualFolder, outcomeModelReference$sharedPsFile))

        model <- CohortMethod::getPsModel(ps, cmData)
        readr::write_csv(model, file.path(manualFolder, sprintf("PsModelManual_%s_%s.png", row$targetName, row$comparatorName)))

        CohortMethod::plotPs(data = ps,
                             targetLabel = row$targetName,
                             comparatorLabel = row$comparatorName,
                             showEquiposeLabel = TRUE,
                             fileName = file.path(manualFolder, sprintf("PsManual_%s_%s.png", row$targetName, row$comparatorName)))
    } else {
        ps <- readRDS(file.path(manualFolder, outcomeModelReference$sharedPsFile))
    }
    if (!file.exists(file.path(manualFolder, outcomeModelReference$strataFile))) {
        studyPop <- CohortMethod::createStudyPopulation(population = ps,
                                                        removeDuplicateSubjects = "keep first",
                                                        removeSubjectsWithPriorOutcome = TRUE,
                                                        riskWindowStart = 1,
                                                        riskWindowEnd = 0,
                                                        endAnchor = "cohort end",
                                                        minDaysAtRisk = 1,
                                                        censorAtNewRiskWindow = TRUE)
        if (analysisId == 1) {
            strataPop <- CohortMethod::stratifyByPs(studyPop, numberOfStrata = 10, baseSelection = "all")
        } else if (analysisId == 3) {
            strataPop <- CohortMethod::matchOnPs(studyPop, caliper = 0.2, caliperScale = "standardized logit", maxRatio = 100)
        } else {
            stop("Unknown analysis ID ", analysisId)
        }
        saveRDS(strataPop, file.path(manualFolder, outcomeModelReference$strataFile))
    } else {
        strataPop <- readRDS(file.path(manualFolder, outcomeModelReference$strataFile))
    }
    if (nrow(strataPop) != 0) {
        if (!file.exists(file.path(manualFolder, sprintf("BalanceAfterMatchingUsingManualPs_%s_%s_%s.png", row$targetName, row$comparatorName, analysisId)))) {
            # Overall balance:
            bal <- CohortMethod::computeCovariateBalance(strataPop, cmData)
            CohortMethod::plotCovariateBalanceScatterPlot(bal, fileName = file.path(manualFolder, sprintf("BalanceAfterMatchingUsingManualPs_%s_%s.png", row$targetName, row$comparatorName)))
            CohortMethod::plotCovariateBalanceOfTopVariables(bal, fileName = file.path(manualFolder, sprintf("BalanceTopAfterMatchingUsingManualPs_%s_%s.png", row$targetName, row$comparatorName)))

            balanceFile <- file.path(manualFolder, sprintf("BalanceAfterMatchingUsingManualPs_%s_%s.csv", as.character(row$targetName), as.character(row$comparatorName)))
            write.csv(bal, balanceFile, row.names = FALSE)

            cmDataAll <-  CohortMethod::loadCohortMethodData(file.path(indicationFolder,
                                                                       "cmOutput",
                                                                       outcomeModelReference$cohortMethodDataFolder))
            bal <- CohortMethod::computeCovariateBalance(strataPop, cmDataAll)
            CohortMethod::plotCovariateBalanceScatterPlot(bal, fileName = file.path(manualFolder, sprintf("AllBalanceAfterMatchingUsingManualPs_%s_%s.png", row$targetName, row$comparatorName)))
            CohortMethod::plotCovariateBalanceOfTopVariables(bal, fileName = file.path(manualFolder, sprintf("AllBalanceTopAfterMatchingUsingManualPs_%s_%s.png", row$targetName, row$comparatorName)))
            balanceFile <- file.path(manualFolder, sprintf("AllBalanceAfterMatchingUsingManualPs_%s_%s.csv", as.character(row$targetName), as.character(row$comparatorName)))
            write.csv(bal, balanceFile, row.names = FALSE)
        }

        bps <- readRDS(file.path(lspsFolder, "bps.rds"))
        bps <- bps[bps$valueAsNumber < 250, ]
        bps <- bps[bps$valueAsNumber > 25, ]
        subsetStrataPop <- strataPop[strataPop$rowId %in% bps$rowId, ]

        cmData$cohorts <- cmData$cohorts[cmData$cohorts$rowId %in% bps$rowId, ]
        subsetBps <- bps[bps$rowId %in% cmData$cohorts$rowId, ]
        cmData$covariates <- ff::as.ffdf(data.frame(rowId = subsetBps$rowId,
                                                    covariateId = subsetBps$conceptId,
                                                    covariateValue = subsetBps$valueAsNumber))
        subsetRef <- unique(subsetBps[, c("conceptId", "conceptName")])
        cmData$covariateRef <- ff::as.ffdf(data.frame(covariateId = subsetRef$conceptId,
                                                      covariateName = subsetRef$conceptName))
        bal <- CohortMethod::computeCovariateBalance(subsetStrataPop, cmData)
        balanceFile <- file.path(manualFolder, sprintf("BpBalanceAfterMatchingUsingManualPs_%s_%s.csv", as.character(row$targetName), as.character(row$comparatorName)))
        write.csv(bal, balanceFile, row.names = FALSE)
        ff::close.ffdf(cmData$covariates)
        ff::close.ffdf(cmData$covariateRef)
    } else {
        ff::close.ffdf(cmData$covariates)
        ff::close.ffdf(cmData$covariateRef)
    }
}

computeAdjustedHrs <- function(row, indicationFolder, lspsFolder, newPsFolder = lspsFolder, indicationId, analysisId, type = "Adjusting for\nblood pressure") {
    # row <- tcs[1,]
    ParallelLogger::logInfo(paste("Computing adjusted HR for", row$targetName, "and", row$comparatorName))
    outcomeModelReference <- readRDS(file.path(lspsFolder, "outcomeModelReference.rds"))
    onTreatment <- outcomeModelReference[outcomeModelReference$targetId == row$targetId &
                                             outcomeModelReference$comparatorId == row$comparatorId &
                                             outcomeModelReference$analysisId == analysisId, ]
    analysisFolder <- file.path(newPsFolder, sprintf("Analysis_%s", analysisId))
    if (!file.exists(analysisFolder)) {
        dir.create(analysisFolder)
    }
    ps <- readRDS(file.path(newPsFolder, onTreatment$sharedPsFile[1]))
    cmData <- CohortMethod::loadCohortMethodData(file.path(newPsFolder, onTreatment$cohortMethodDataFolder[1]))

    computeHr <- function(i) {
        # i = 1
        omFile <- file.path(newPsFolder, onTreatment$outcomeModelFile[i])
        if (!file.exists(omFile)) {
            studyPop <- CohortMethod::createStudyPopulation(population = ps,
                                                            cohortMethodData = cmData,
                                                            outcomeId = onTreatment$outcomeId[i],
                                                            removeDuplicateSubjects = "keep first",
                                                            removeSubjectsWithPriorOutcome = TRUE,
                                                            riskWindowStart = 1,
                                                            riskWindowEnd = 0,
                                                            endAnchor = "cohort end",
                                                            minDaysAtRisk = 1,
                                                            censorAtNewRiskWindow = TRUE)
            strataPop <- CohortMethod::matchOnPs(studyPop, caliper = 0.2, caliperScale = "standardized logit", maxRatio = 100)
            outcomeModel <- CohortMethod::fitOutcomeModel(population = strataPop,
                                                          stratified = TRUE,
                                                          modelType = "cox")
            saveRDS(outcomeModel, omFile)
        }
    }
    plyr::l_ply(1:nrow(onTreatment), computeHr, .progress = "text")
    originalSummary <- CohortMethod::summarizeAnalyses(onTreatment, file.path(indicationFolder, "cmOutput"))
    originalSummary$type <- "Original"
    bpAdjustedSummary <- CohortMethod::summarizeAnalyses(onTreatment, newPsFolder)
    bpAdjustedSummary$type <- type
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

    if (all(is.na(tcEstimates$seLogRr))) {
        warning("All estimates are NA. Skipping calibration")
        return()
    }
    calibrate <- function(estimates) {
        controlEstimates <- estimates[!is.na(estimates$targetEffectSize), ]
        controlEstimates <- controlEstimates[!is.na(controlEstimates$seLogRr), ]
        errorModel <- EmpiricalCalibration::fitSystematicErrorModel(logRr = controlEstimates$logRr,
                                                                    seLogRr = controlEstimates$seLogRr,
                                                                    trueLogRr = log(controlEstimates$targetEffectSize),
                                                                    estimateCovarianceMatrix = FALSE)
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
                       calibrate(tcEstimates[tcEstimates$type == type, ]),
                       calibrate(ctEstimates[ctEstimates$type == "Original", ]),
                       calibrate(ctEstimates[ctEstimates$type == type, ]))

    vizData <- merge(estimates, data.frame(outcomeId = outcomesOfInterest$cohortId,
                                           outcomeName = outcomesOfInterest$name))
    vizData <- vizData[!is.na(vizData$seLogRr), ]
    outcomeNames <- unique(vizData$outcomeName)
    outcomeNames <- outcomeNames[order(outcomeNames, decreasing = TRUE)]
    vizData$y <- match(vizData$outcomeName, outcomeNames) - 0.1 + 0.2*(vizData$type == "Original")

    fileName <- file.path(newPsFolder, sprintf("HrsDataBpAdj_%s_%s_%s.csv", row$targetName, row$comparatorName, analysisId))
    write.csv(estimates, fileName, row.names = FALSE)

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
    }
    plotHrs(vizData[vizData$targetId == row$targetId, ], file.path(newPsFolder, sprintf("Hrs_%s_%s_%s.png", row$targetName, row$comparatorName, analysisId)))
    plotHrs(vizData[vizData$targetId == row$comparatorId, ], file.path(newPsFolder, sprintf("Hrs_%s_%s_%s.png", row$comparatorName, row$targetName, analysisId)))
}


computeUnadjustedHrs <- function(row, indicationFolder, lspsFolder, indicationId, analysisId) {
    # row <- tcs[1,]
    ParallelLogger::logInfo(paste("Computing unadjusted HR for", row$targetName, "and", row$comparatorName))
    outcomeModelReference <- readRDS(file.path(lspsFolder, "outcomeModelReference.rds"))
    onTreatment <- outcomeModelReference[outcomeModelReference$targetId == row$targetId &
                                             outcomeModelReference$comparatorId == row$comparatorId &
                                             outcomeModelReference$analysisId == analysisId, ]
    analysisFolder <- file.path(lspsFolder, sprintf("Analysis_%s", analysisId + 4))
    if (!file.exists(analysisFolder)) {
        dir.create(analysisFolder)
    }
    cmData <- CohortMethod::loadCohortMethodData(file.path(indicationFolder, "cmOutput", onTreatment$cohortMethodDataFolder[1]))

    computeHr <- function(i) {
        #i = 1
        omFile <- file.path(lspsFolder, gsub(sprintf("Analysis_%s", analysisId), sprintf("Analysis_%s", analysisId + 4), onTreatment$outcomeModelFile[i]))
        if (!file.exists(omFile)) {
            studyPop <- CohortMethod::createStudyPopulation(cohortMethodData = cmData,
                                                            outcomeId = onTreatment$outcomeId[i],
                                                            removeDuplicateSubjects = "keep first",
                                                            removeSubjectsWithPriorOutcome = TRUE,
                                                            riskWindowStart = 1,
                                                            riskWindowEnd = 0,
                                                            endAnchor = "cohort end",
                                                            minDaysAtRisk = 1,
                                                            censorAtNewRiskWindow = TRUE)
            outcomeModel <- CohortMethod::fitOutcomeModel(population = studyPop,
                                                          stratified = FALSE,
                                                          modelType = "cox")
            saveRDS(outcomeModel, omFile)
        }
    }
    plyr::l_ply(1:nrow(onTreatment), computeHr, .progress = "text")
    originalSummary <- CohortMethod::summarizeAnalyses(onTreatment, file.path(indicationFolder, "cmOutput"))
    originalSummary$type <- "Original"
    onTreatment$analysisId <- onTreatment$analysisId + 4
    onTreatment$outcomeModelFile <- gsub(sprintf("Analysis_%s", analysisId), sprintf("Analysis_%s", analysisId + 4), onTreatment$outcomeModelFile)
    unadjustedSummary <- CohortMethod::summarizeAnalyses(onTreatment, lspsFolder)
    unadjustedSummary$type <- "Unadjusted"
    estimates <- rbind(originalSummary, unadjustedSummary)

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

    if (all(is.na(tcEstimates$seLogRr))) {
        warning("All estimates are NA. Skipping calibration")
        return()
    }
    calibrate <- function(estimates) {
        controlEstimates <- estimates[!is.na(estimates$targetEffectSize), ]
        controlEstimates <- controlEstimates[!is.na(controlEstimates$seLogRr), ]
        errorModel <- EmpiricalCalibration::fitSystematicErrorModel(logRr = controlEstimates$logRr,
                                                                    seLogRr = controlEstimates$seLogRr,
                                                                    trueLogRr = log(controlEstimates$targetEffectSize),
                                                                    estimateCovarianceMatrix = FALSE)
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
                       calibrate(tcEstimates[tcEstimates$type == "Unadjusted", ]),
                       calibrate(ctEstimates[ctEstimates$type == "Original", ]),
                       calibrate(ctEstimates[ctEstimates$type == "Unadjusted", ]))

    vizData <- merge(estimates, data.frame(outcomeId = outcomesOfInterest$cohortId,
                                           outcomeName = outcomesOfInterest$name))
    vizData <- vizData[!is.na(vizData$seLogRr), ]
    outcomeNames <- unique(vizData$outcomeName)
    outcomeNames <- outcomeNames[order(outcomeNames, decreasing = TRUE)]
    vizData$y <- match(vizData$outcomeName, outcomeNames) - 0.1 + 0.2*(vizData$type == "Original")

    # Save for George:
    fileName <- file.path(lspsFolder, sprintf("HrsDataNoAdj_%s_%s_%s.csv", row$targetName, row$comparatorName, analysisId + 4))
    write.csv(estimates, fileName, row.names = FALSE)

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
    plotHrs(vizData[vizData$targetId == row$targetId, ], file.path(lspsFolder, sprintf("Hrs_%s_%s_%s.png", row$targetName, row$comparatorName, analysisId + 4)))

    plotHrs(vizData[vizData$targetId == row$comparatorId, ], file.path(lspsFolder, sprintf("Hrs_%s_%s_%s.png", row$comparatorName, row$targetName, analysisId + 4)))
}

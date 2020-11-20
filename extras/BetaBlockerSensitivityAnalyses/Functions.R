subsetOmr <- function(sourceFolder, targetFolder) {
    outcomeModelReference1 <- readRDS(file.path(sourceFolder,
                                                "cmOutput",
                                                "outcomeModelReference1.rds"))
    outcomeModelReference1$part <- 1
    outcomeModelReference2 <- readRDS(file.path(sourceFolder,
                                                "cmOutput",
                                                "outcomeModelReference2.rds"))
    outcomeModelReference2$part <- 2
    outcomeModelReference4 <- readRDS(file.path(sourceFolder,
                                                "cmOutput",
                                                "outcomeModelReference4.rds"))
    outcomeModelReference4$part <- 4
    outcomeModelReference <- rbind(outcomeModelReference1, outcomeModelReference2, outcomeModelReference4)

    subset <- read.csv("extras/BetaBlockerSensitivityAnalyses/ComparisonsBb.csv")
    subset <- rbind(data.frame(targetId = subset$targetId,
                               comparatorId = subset$comparatorId),
                    data.frame(targetId = subset$comparatorId,
                               comparatorId = subset$targetId))
    outcomeModelReference <- merge(outcomeModelReference, subset)
    outcomeModelReference <- outcomeModelReference[outcomeModelReference$analysisId == 3, ]
    saveRDS(outcomeModelReference, file.path(targetFolder, "toGenerate.rds"))
}

restrictCmDatas <- function(sourceFolder, targetFolder) {
    toGenerate <- readRDS(file.path(targetFolder, "toGenerate.rds"))
    cmDataFolders <- unique(toGenerate$cohortMethodDataFolder[toGenerate$part %in% c(1, 2)])

    # toDelete <- list.files(targetFolder, "CmData_", include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
    # toDelete <- toDelete[!basename(toDelete) %in% cmDataFolders]
    # unlink(toDelete, recursive = TRUE)
    #
    #     for (cmDataFolder in cmDataFolders) {
    #         print(cmDataFolder)
    #         cmData <- CohortMethod::loadCohortMethodData(file.path(targetFolder, "cmOutput", cmDataFolder))
    #         if (nrow(cmData$covariates) < 3) {
    #             stop("No covariates found")
    #         }
    #     }

    cmDataFolder = cmDataFolders[1]
    for (cmDataFolder in cmDataFolders) {
        outputFile <- file.path(targetFolder, "cmOutput", cmDataFolder)
        if (!file.exists(outputFile)) {
            writeLines(paste("Restricting population in", cmDataFolder, "to those without CV diseases."))
            cmData <- CohortMethod::loadCohortMethodData(file.path(sourceFolder, "cmOutput", cmDataFolder))
            if (nrow(cmData$covariates) < 3) {
                stop("No covariates found")
            }
            # Subset to "Those without cardiovascular diseases, which are tightly related with HF (heart failure, ischemic heart disease, and atrial fibrillation)."
            covariateIds <- c(316139210, # Heart failure
                              4185932210, # Ischemic heart disease
                              49436004210) # Atrial fibrillation)
            # covariateRef <- as.data.frame(ff::as.ram(cmData$covariateRef))
            # covariateRef[grepl(": atrial fibrillation$", covariateRef$covariateName, ignore.case = TRUE), ]
            rowIdsToExclude <- cmData$covariates$rowId[ffbase::`%in%`(cmData$covariates$covariateId, covariateIds)]
            rowIdsToInclude <-  cmData$cohorts$rowId[!cmData$cohorts$rowId %in% rowIdsToExclude]
            writeLines(paste("Excluding", length(rowIdsToExclude), "of", nrow(cmData$cohorts), "subjects"))
            newCmData <- cmData
            newCmData$cohorts <- cmData$cohorts[cmData$cohorts$rowId %in% rowIdsToInclude, ]
            newCmData$outcomes <- cmData$outcomes[cmData$outcomes$rowId %in% rowIdsToInclude, ]
            newCmData$covariates <- cmData$covariates[ffbase::`%in%`(cmData$covariates$rowId, rowIdsToInclude), ]
            newCmData$covariateRef <- ff::clone(cmData$covariateRef)
            newCmData$analysisRef <- ff::clone(cmData$analysisRef)
            newCmData$metaData$populationSize <- length(rowIdsToExclude)
            CohortMethod::saveCohortMethodData(newCmData, outputFile, compress = TRUE)
        }
    }
}

addOtherHalf <- function(i, cmFolder, reference) {
    sourceFolder <- file.path(cmFolder, reference$cohortMethodDataFolder[i])
    targetFolder <- gsub(paste0("_t", reference$targetId[i]),
                         paste0("_t", reference$comparatorId[i]),
                         gsub(paste0("_c", reference$comparatorId[i]),
                              paste0("_c", reference$targetId[i]),
                              sourceFolder))
    if (!file.exists(targetFolder)) {
        cohortMethodData <- CohortMethod::loadCohortMethodData(sourceFolder)
        idx <- ff::as.ff(1:2)
        cohortMethodData$covariates <- cohortMethodData$covariates[idx, ]
        cohortMethodData$covariateRef <- cohortMethodData$covariateRef[idx, ]
        cohortMethodData$analysisRef <- cohortMethodData$analysisRef[idx, ]
        cohortMethodData$cohorts$treatment <- 1 - cohortMethodData$cohorts$treatment
        metaData <- attr(cohortMethodData$cohorts, "metaData")
        temp <- metaData$attrition$targetPersons
        metaData$attrition$targetPersons <- metaData$attrition$comparatorPersons
        metaData$attrition$comparatorPersons <- temp
        metaData$targetId <- reference$comparatorId[i]
        metaData$comparatorId <- reference$targetId[i]
        attr(cohortMethodData$cohorts, "metaData") <- metaData
        CohortMethod::saveCohortMethodData(cohortMethodData, targetFolder)
    }
    sourceFile <- file.path(cmFolder, reference$sharedPsFile[i])
    targetFile <- gsub(paste0("_t", reference$targetId[i]),
                       paste0("_t", reference$comparatorId[i]),
                       gsub(paste0("_c", reference$comparatorId[i]),
                            paste0("_c", reference$targetId[i]),
                            sourceFile))
    if (!file.exists(targetFile)) {
        ps <- readRDS(sourceFile)
        ps$propensityScore <- 1 - ps$propensityScore
        ps$preferenceScore <- 1 - ps$preferenceScore
        ps$treatment <- 1 - ps$treatment
        metaData <- attr(ps, "metaData")
        temp <- metaData$attrition$targetPersons
        metaData$attrition$targetPersons <- metaData$attrition$comparatorPersons
        metaData$attrition$comparatorPersons <- temp
        metaData$targetId <- reference$comparatorId[i]
        metaData$comparatorId <- reference$targetId[i]
        if (!is.null(metaData$psModelCoef)) {
            metaData$psModelCoef <- -metaData$psModelCoef
        }
        attr(ps, "metaData") <- metaData
        saveRDS(ps, targetFile)
    }
    return(NULL)
}

runCohortMethod <- function(targetFolder) {
    cmAnalysisList <- CohortMethod::loadCmAnalysisList("inst/settings/cmAnalysisListAsymHypertension.json")
    cmAnalysisList <- list(cmAnalysisList[[1]]) # Analysis ID 3: matching, on-treatment
    toGenerate <- readRDS(file.path(targetFolder, "toGenerate.rds"))

    createTcos <- function(subset) {
        tcos <- CohortMethod::createTargetComparatorOutcomes(targetId = subset$targetId[1],
                                                             comparatorId = subset$comparatorId[1],
                                                             outcomeIds = subset$outcomeId)
        return(tcos)
    }

    # # Delete strataPop and outcome models from previous run:
    # toDelete <- list.files(targetFolder, "StratPop.*.rds", recursive = TRUE, full.names = TRUE)
    # unlink(toDelete)
    # toDelete <- list.files(targetFolder, "Analysis_", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
    # unlink(toDelete, recursive = TRUE)

    # First run all forward analyses:
    toGenerateForward <- toGenerate[toGenerate$part %in% c(1, 2), ]
    tcosList <- lapply(split(toGenerateForward, paste(toGenerateForward$targetId, toGenerateForward$comparatorId)), createTcos)
    outcomesOfInterest <- read.csv("inst/settings/OutcomesOfInterest.csv")
    outcomesOfInterest <- outcomesOfInterest[outcomesOfInterest$indicationId == "Hypertension", ]
    CohortMethod::runCmAnalyses(outputFolder = file.path(targetFolder, "cmOutput"),
                                cmAnalysisList = cmAnalysisList,
                                targetComparatorOutcomesList = tcosList,
                                refitPsForEveryStudyPopulation = FALSE,
                                createPsThreads = 3,
                                psCvThreads = 10,
                                createStudyPopThreads = 4,
                                trimMatchStratifyThreads = 4,
                                fitOutcomeModelThreads = 4,
                                outcomeIdsOfInterest = outcomesOfInterest$cohortId)
    omr <- readRDS(file.path(targetFolder, "cmOutput", "outcomeModelReference.rds"))
    saveRDS(omr, file.path(targetFolder, "outcomeModelReference12.rds"))
    analysisSummary <- CohortMethod::summarizeAnalyses(omr, file.path(targetFolder, "cmOutput"))
    saveRDS(analysisSummary, file.path(targetFolder, "analysisSummary12.rds"))

    # Then run the reverse analyses:
    writeLines("Adding reverse cmData and sharedPs objects")
    toReverse <- toGenerateForward[!duplicated(toGenerateForward$cohortMethodDataFolder), ]
    plyr::l_ply(1:nrow(toReverse), addOtherHalf, cmFolder = file.path(targetFolder, "cmOutput"), reference = toReverse, .progress = "text")
    toGenerateBackward <- toGenerate[toGenerate$part %in% c(4), ]
    tcosList <- lapply(split(toGenerateBackward, paste(toGenerateBackward$targetId, toGenerateBackward$comparatorId)), createTcos)
    outcomesOfInterest <- read.csv("inst/settings/OutcomesOfInterest.csv")
    outcomesOfInterest <- outcomesOfInterest[outcomesOfInterest$indicationId == "Hypertension", ]
    CohortMethod::runCmAnalyses(outputFolder = file.path(targetFolder, "cmOutput"),
                                cmAnalysisList = cmAnalysisList,
                                targetComparatorOutcomesList = tcosList,
                                refitPsForEveryStudyPopulation = FALSE,
                                createPsThreads = 3,
                                psCvThreads = 10,
                                createStudyPopThreads = 4,
                                trimMatchStratifyThreads = 4,
                                fitOutcomeModelThreads = 4,
                                outcomeIdsOfInterest = outcomesOfInterest$cohortId)
    omr <- readRDS(file.path(targetFolder, "cmOutput", "outcomeModelReference.rds"))
    saveRDS(omr, file.path(targetFolder, "outcomeModelReference4.rds"))
    analysisSummary <- CohortMethod::summarizeAnalyses(omr, file.path(targetFolder, "cmOutput"))
    saveRDS(analysisSummary, file.path(targetFolder, "analysisSummary4.rds"))
}

calibrateResults <- function(sourceFolder, targetFolder) {
    tcEstimates12 <- readRDS(file.path(targetFolder, "analysisSummary12.rds"))
    tcEstimates4 <- readRDS(file.path(targetFolder, "analysisSummary4.rds"))
    tcEstimates <- rbind(tcEstimates12, tcEstimates4)

    pathToCsv <- "inst/settings/OutcomesOfInterest.csv"
    outcomesOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)
    outcomesOfInterest <- outcomesOfInterest[outcomesOfInterest$indicationId == "Hypertension", ]
    pathToCsv <- file.path(sourceFolder, "signalInjectionSummary.csv")
    siSummary <- read.csv(pathToCsv)
    siSummary <- siSummary[siSummary$newOutcomeId %in% tcEstimates$outcomeId, ]
    pcs <- data.frame(outcomeId = siSummary$newOutcomeId,
                      trueEffectSize = siSummary$trueEffectSize,
                      targetEffectSize = siSummary$targetEffectSize)
    pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Legend")
    negativeControls <- read.csv(pathToCsv)
    negativeControls <- negativeControls[negativeControls$indicationId == "Hypertension", ]

    tcPcEstimates <- merge(tcEstimates, pcs)
    tcEstimates$trueEffectSize[tcEstimates$outcomeId %in% negativeControls$cohortId] <- 1
    tcEstimates$targetEffectSize[tcEstimates$outcomeId %in% negativeControls$cohortId] <- 1

    tcEstimates <- rbind(tcEstimates[tcEstimates$outcomeId %in% outcomesOfInterest$cohortId |
                                         tcEstimates$outcomeId %in% negativeControls$cohortId, ],
                         tcPcEstimates)

    # ctEstimates <- tcEstimates
    # temp <- ctEstimates$targetId
    # ctEstimates$targetId <- ctEstimates$comparatorId
    # ctEstimates$comparatorId <- temp
    # temp <- ctEstimates$target
    # ctEstimates$target <- ctEstimates$comparator
    # ctEstimates$comparator <- temp
    # temp <- ctEstimates$targetDays
    # ctEstimates$targetDays <- ctEstimates$comparatorDays
    # ctEstimates$comparatorDays <- temp
    # temp <- ctEstimates$eventsTarget
    # ctEstimates$eventsTarget <- ctEstimates$eventsComparator
    # ctEstimates$eventsComparator <- temp
    # ctEstimates$logRr <- -ctEstimates$logRr
    # ctEstimates$rr <- 1/ctEstimates$r
    # temp <- 1/ctEstimates$ci95ub
    # ctEstimates$ci95ub <- 1/ctEstimates$ci95lb
    # ctEstimates$ci95lb <- temp
    # tcPcEstimates <- merge(tcEstimates, pcs)
    # ctPcEstimates <- merge(ctEstimates, pcs)
    # tcEstimates$trueEffectSize[tcEstimates$outcomeId  %in% negativeControls$cohortId] <- 1
    # ctEstimates$trueEffectSize[ctEstimates$outcomeId  %in% negativeControls$cohortId] <- 1
    # tcEstimates <- rbind(tcEstimates[tcEstimates$outcomeId %in% outcomesOfInterest$cohortId |
    #                                      tcEstimates$outcomeId %in% negativeControls$cohortId, ],
    #                      tcPcEstimates)
    # ctEstimates <- rbind(ctEstimates[ctEstimates$outcomeId %in% outcomesOfInterest$cohortId |
    #                                      ctEstimates$outcomeId %in% negativeControls$cohortId, ],
    #                      ctPcEstimates)
    # estimates <- rbind(tcEstimates, ctEstimates)
    estimates <- tcEstimates

    if (all(is.na(estimates$seLogRr))) {
        warning("All estimates are NA. Skipping calibration")
        return()
    }
    # subset <- split(estimates, paste(estimates$targetId, estimates$comparatorId))[[3]]
    # subset <- estimates[estimates$targetId == 1314002 & estimates$comparatorId == 1318853, ]
    # subset <- tcEstimates12[tcEstimates12$targetId == 1314002 & tcEstimates12$comparatorId == 1318853, ]
    calibrate <- function(subset) {
        controlEstimates <- subset[!is.na(subset$trueEffectSize), ]
        controlEstimates <- controlEstimates[!is.na(controlEstimates$seLogRr), ]
        if (nrow(controlEstimates) > 0) {
            errorModel <- EmpiricalCalibration::fitSystematicErrorModel(logRr = controlEstimates$logRr,
                                                                        seLogRr = controlEstimates$seLogRr,
                                                                        trueLogRr = log(controlEstimates$trueEffectSize),
                                                                        estimateCovarianceMatrix = FALSE)
            # EmpiricalCalibration::plotCiCalibrationEffect(logRr = controlEstimates$logRr,
            #                                               seLogRr = controlEstimates$seLogRr,
            #                                               trueLogRr = log(controlEstimates$targetEffectSize),)
            cal <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = subset$logRr,
                                                                     seLogRr = subset$seLogRr,
                                                                     model = errorModel)
            subset$calibratedRr <- exp(cal$logRr)
            subset$calibratedCi95Lb <- exp(cal$logLb95Rr)
            subset$calibratedCi95Ub <- exp(cal$logUb95Rr)
            subset$calibratedLogRr <- cal$logRr
            subset$calibratedSeLogRr <- cal$seLogRr
        } else {
            subset$calibratedRr <- NA
            subset$calibratedCi95Lb <- NA
            subset$calibratedCi95Ub <- NA
            subset$calibratedLogRr <- NA
            subset$calibratedSeLogRr <- NA
        }
        negativeControlEstimates <- controlEstimates[controlEstimates$trueEffectSize == 1, ]
        if (nrow(controlEstimates) > 0) {
            null <- EmpiricalCalibration::fitMcmcNull(logRr = negativeControlEstimates$logRr,
                                                      seLogRr = negativeControlEstimates$seLogRr)
            # EmpiricalCalibration::plotCalibrationEffect(logRrNegatives = negativeControlEstimates$logRr,
            #                                             seLogRrNegatives = negativeControlEstimates$seLogRr,
            #                                             null = null,
            #                                             showCis = TRUE)
            calP <- EmpiricalCalibration::calibrateP(logRr = subset$logRr,
                                                     seLogRr = subset$seLogRr,
                                                     null = null)
            subset$calibratedP <- calP$p
        } else {
            subset$calibratedP <- NA
        }
        return(subset)
    }
    cluster <- ParallelLogger::makeCluster(5)
    calibratedEstimates <- ParallelLogger::clusterApply(cluster, split(estimates, paste(estimates$targetId, estimates$comparatorId)), calibrate)
    ParallelLogger::stopCluster(cluster)
    calibratedEstimates <- do.call(rbind, calibratedEstimates)
    saveRDS(calibratedEstimates, file.path(targetFolder, "calibratedEstimates.rds"))
}

compareNewToOldEstimates <- function(sourceFolder, targetFolder, fileName) {
 newEstimates <- readRDS(file.path(targetFolder, "calibratedEstimates.rds"))
 oldEstimates <- readr::read_csv(file.path(sourceFolder, "export", "cohort_method_result.csv"))
 colnames(oldEstimates) <- SqlRender::snakeCaseToCamelCase(colnames(oldEstimates))
 estimates <- merge(oldEstimates,
                    data.frame(analysisId = newEstimates$analysisId,
                               targetId = newEstimates$targetId,
                               comparatorId = newEstimates$comparatorId,
                               outcomeId = newEstimates$outcomeId,
                               newLogRr = newEstimates$logRr,
                               newSeLogRr = newEstimates$seLogRr,
                               newCalibratedLogRr = newEstimates$calibratedLogRr,
                               newCalibratedSeLogRr = newEstimates$calibratedSeLogRr))
 plotEstimates <- estimates[!is.na(estimates$newSeLogRr) & abs(estimates$logRr) < 10 & abs(estimates$newLogRr) < 10, ]
 plot(plotEstimates$logRr, plotEstimates$newLogRr)

 plotEstimates <- estimates[!is.na(estimates$newCalibratedSeLogRr) & abs(estimates$calibratedLogRr) < 10 & abs(estimates$newCalibratedLogRr) < 10, ]
 plot(plotEstimates$calibratedLogRr, plotEstimates$newCalibratedLogRr)
}

createPrettyTable <- function(sourceFolder, targetFolder, fileName) {
    calibratedEstimates <- readRDS(file.path(targetFolder, "calibratedEstimates.rds"))

    pathToCsv <- "inst/settings/OutcomesOfInterest.csv"
    outcomesOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)
    outcomesOfInterest <- outcomesOfInterest[outcomesOfInterest$indicationId == "Hypertension", ]
    pathToCsv <- file.path(sourceFolder, "signalInjectionSummary.csv")
    siSummary <- read.csv(pathToCsv)
    pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Legend")
    negativeControls <- read.csv(pathToCsv, stringsAsFactors = FALSE)
    negativeControls <- negativeControls[negativeControls$indicationId == "Hypertension", ]

    siSummary <- merge(siSummary,
                       data.frame(outcomeId = negativeControls$cohortId,
                                  name = negativeControls$name))
    siSummary$outcomeName <- sprintf("%s (HR = %0.1f)", siSummary$name, siSummary$trueEffectSize)

    outcomeNames <- rbind(data.frame(outcomeId = outcomesOfInterest$cohortId,
                                     outcomeName = outcomesOfInterest$name),
                          data.frame(outcomeId = negativeControls$cohortId,
                                     outcomeName = negativeControls$name),
                          data.frame(outcomeId = siSummary$newOutcomeId,
                                     outcomeName = siSummary$outcomeName))
    exposures <- read.csv(file.path(sourceFolder, "pairedExposureSummaryFilteredBySize.csv"), stringsAsFactors = FALSE)
    exposureNames <- rbind(data.frame(exposureId = exposures$targetId,
                                      exposureName = exposures$targetName),
                           data.frame(exposureId = exposures$comparatorId,
                                      exposureName = exposures$comparatorName))
    exposureNames <- unique(exposureNames)

    calibratedEstimates <- merge(calibratedEstimates, outcomeNames)
    calibratedEstimates <- merge(calibratedEstimates,
                                 data.frame(targetId = exposureNames$exposureId,
                                            targetName = exposureNames$exposureName))
    calibratedEstimates <- merge(calibratedEstimates,
                                 data.frame(comparatorId = exposureNames$exposureId,
                                            comparatorName = exposureNames$exposureName))
    readr::write_csv(calibratedEstimates, fileName)
}

addBpToCmDatas <- function(sourceFolder, targetFolder) {
    bpFolder <- file.path(sourceFolder, "bp")
    bps <- readRDS(file.path(bpFolder, "bps.rds"))
    bps <- bps[bps$valueAsNumber < 250, ]
    bps <- bps[bps$valueAsNumber > 25, ]

    toGenerate <- readRDS(file.path(targetFolder, "toGenerate.rds"))
    cmDataFolders <- unique(toGenerate$cohortMethodDataFolder[toGenerate$part %in% c(1, 2)])

    cmDataFolder = cmDataFolders[1]
    for (cmDataFolder in cmDataFolders) {
        outputFile <- file.path(targetFolder, "cmOutput", cmDataFolder)
        if (!file.exists(outputFile)) {
            writeLines(paste("Adding blood pressure covariates to ", cmDataFolder, "."))
            cmData <- CohortMethod::loadCohortMethodData(file.path(sourceFolder, "cmOutput", cmDataFolder))
            if (nrow(cmData$covariates) < 3) {
                stop("No covariates found")
            }
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
            newCmData$metaData$populationSize <- nrow(newCmData$cohorts)
            CohortMethod::saveCohortMethodData(newCmData, outputFile, compress = TRUE)
        }
    }
}

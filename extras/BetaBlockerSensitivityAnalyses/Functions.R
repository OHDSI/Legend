subsetOmr <- function(sourceFolder, targetFolder) {
    outcomeModelReference1 <- readRDS(file.path(sourceFolder,
                                                "cmOutput",
                                                "outcomeModelReference1.rds"))
    outcomeModelReference2 <- readRDS(file.path(sourceFolder,
                                                "cmOutput",
                                                "outcomeModelReference2.rds"))
    outcomeModelReference3 <- readRDS(file.path(sourceFolder,
                                                "cmOutput",
                                                "outcomeModelReference3.rds"))
    outcomeModelReference <- rbind(outcomeModelReference1, outcomeModelReference2, outcomeModelReference3)

    subset <- read.csv("extras/BetaBlockerSensitivityAnalyses/ComparisonsBb.csv")

    outcomeModelReference <- merge(outcomeModelReference, subset[, c("targetId", "comparatorId")])
    outcomeModelReference <- outcomeModelReference[outcomeModelReference$analysisId == 1, ]
    saveRDS(outcomeModelReference, file.path(targetFolder, "toGenerate.rds"))
}

restrictCmDatas <- function(sourceFolder, targetFolder) {
    outcomeModelReference <- readRDS(file.path(targetFolder, "toGenerate.rds"))
    cmDataFolders<- unique(outcomeModelReference$cohortMethodDataFolder)
    cmDataFolder = cmDataFolders[1]
    for (cmDataFolder in cmDataFolders) {
        writeLines(paste("Restricting population in", cmDataFolder, "to those without CV diseases."))
        cmData <- CohortMethod::loadCohortMethodData(file.path(sourceFolder, "cmOutput", cmDataFolder))
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
        CohortMethod::saveCohortMethodData(newCmData, file.path(targetFolder, "cmOutput", cmDataFolder), compress = TRUE)
    }
}

runCohortMethod <- function(targetFolder) {
    cmAnalysisList <- CohortMethod::loadCmAnalysisList("inst/settings/cmAnalysisListHypertension.json")
    cmAnalysisList <- list(cmAnalysisList[[1]]) # Analysis ID 1
    outcomeModelReference <- readRDS(file.path(targetFolder, "toGenerate.rds"))
    createTcos <- function(subset) {
        tcos <- CohortMethod::createTargetComparatorOutcomes(targetId = subset$targetId[1],
                                                             comparatorId = subset$comparatorId[1],
                                                             outcomeIds = subset$outcomeId)
        return(tcos)
    }
    tcosList <- lapply(split(outcomeModelReference, paste(outcomeModelReference$targetId, outcomeModelReference$comparatorId)), createTcos)
    outcomesOfInterest <- read.csv("inst/settings/OutcomesOfInterest.csv")
    outcomesOfInterest <- outcomesOfInterest[outcomesOfInterest$indicationId == "Hypertension", ]
    CohortMethod::runCmAnalyses(outputFolder = file.path(targetFolder, "cmOutput"),
                                cmAnalysisList = cmAnalysisList,
                                targetComparatorOutcomesList = tcosList,
                                createPsThreads = 3,
                                psCvThreads = 10,
                                createStudyPopThreads = 4,
                                trimMatchStratifyThreads = 4,
                                fitOutcomeModelThreads = 4,
                                outcomeIdsOfInterest = outcomesOfInterest$cohortId)
    omr <- readRDS(file.path(targetFolder, "cmOutput", "outcomeModelReference.rds"))
    analysisSummary <- CohortMethod::summarizeAnalyses(omr, targetFolder)
    saveRDS(analysisSummary, file.path(targetFolder, "analysisSummary.rds"))
}

calibrateResults <- function(sourceFolder, targetFolder) {
    tcEstimates <- readRDS(file.path(targetFolder, "analysisSummary.rds"))

    pathToCsv <- "inst/settings/OutcomesOfInterest.csv"
    outcomesOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)
    outcomesOfInterest <- outcomesOfInterest[outcomesOfInterest$indicationId == "Hypertension", ]
    pathToCsv <- file.path(sourceFolder, "signalInjectionSummary.csv")
    siSummary <- read.csv(pathToCsv)
    siSummary <- siSummary[siSummary$newOutcomeId %in% estimates$outcomeId, ]
    pcs <- data.frame(outcomeId = siSummary$newOutcomeId, trueEffectSize = siSummary$trueEffectSize)
    pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Legend")
    negativeControls <- read.csv(pathToCsv)
    negativeControls <- negativeControls[negativeControls$indicationId == "Hypertension", ]

    ctEstimates <- tcEstimates
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
    tcPcEstimates <- merge(tcEstimates, pcs)
    ctPcEstimates <- merge(ctEstimates, pcs)
    tcEstimates$trueEffectSize[tcEstimates$outcomeId  %in% negativeControls$cohortId] <- 1
    ctEstimates$trueEffectSize[ctEstimates$outcomeId  %in% negativeControls$cohortId] <- 1
    tcEstimates <- rbind(tcEstimates[tcEstimates$outcomeId %in% outcomesOfInterest$cohortId |
                                         tcEstimates$outcomeId %in% negativeControls$cohortId, ],
                         tcPcEstimates)
    ctEstimates <- rbind(ctEstimates[ctEstimates$outcomeId %in% outcomesOfInterest$cohortId |
                                         ctEstimates$outcomeId %in% negativeControls$cohortId, ],
                         ctPcEstimates)
    estimates <- rbind(tcEstimates, ctEstimates)


    if (all(is.na(estimates$seLogRr))) {
        warning("All estimates are NA. Skipping calibration")
        return()
    }
    # estimates <- split(estimates, paste(estimates$targetId, estimates$comparatorId))[[4]]
    calibrate <- function(estimates) {
        controlEstimates <- estimates[!is.na(estimates$trueEffectSize), ]
        controlEstimates <- controlEstimates[!is.na(controlEstimates$seLogRr), ]
        negativeControlEstimates <- controlEstimates[controlEstimates$trueEffectSize == 1, ]

        null <- EmpiricalCalibration::fitMcmcNull(logRr = negativeControlEstimates$logRr,
                                                  seLogRr = negativeControlEstimates$seLogRr)

        # EmpiricalCalibration::plotCalibrationEffect(logRrNegatives = negativeControlEstimates$logRr,
        #                                             seLogRrNegatives = negativeControlEstimates$seLogRr,
        #                                             null = null,
        #                                             showCis = TRUE)

        calP <- EmpiricalCalibration::calibrateP(logRr = estimates$logRr,
                                                 seLogRr = estimates$seLogRr,
                                                 null = null)
        errorModel <- EmpiricalCalibration::fitSystematicErrorModel(logRr = controlEstimates$logRr,
                                                                    seLogRr = controlEstimates$seLogRr,
                                                                    trueLogRr = log(controlEstimates$trueEffectSize),
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
        calibrated$p <- calP$p
        calibrated$estimate <- "Calibrated"
        estimates$estimate <- "Uncalibrated"
        estimates <- rbind(estimates[, colnames(calibrated)], calibrated)
    }
    calibratedEstimates <- plyr::llply(split(estimates, paste(estimates$targetId, estimates$comparatorId)), calibrate, .progress = "text")
    calibratedEstimates <- do.call(rbind, calibratedEstimates)
    saveRDS(calibratedEstimates, file.path(targetFolder, "calibratedEstimates.rds"))
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

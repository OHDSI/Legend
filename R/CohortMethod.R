# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of Legend
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Run the cohort method package
#'
#' @details
#' Runs the cohort method package to produce propensity scores and outcome models.
#'
#' @param outputFolder   Name of local folder to place results; make sure to use forward slashes (/).
#'                       Do not use a folder on a network drive since this greatly impacts performance.
#' @param indicationId   A string denoting the indicationId.
#' @param maxCores       How many parallel cores should be used? If more cores are made available this
#'                       can speed up the analyses.
#'
#' @export
runCohortMethod <- function(outputFolder, indicationId = "Depression", maxCores = 4) {
    # Note: we don't want to run all analyses on all TCO pairs. Specifically, analyses that are
    # symmetrical (e.g. PS stratification) we only want to do one way, and the interaction analyses we
    # don't want to run on the positive controls.  To do this, we split up the analyses across several
    # CohortMethod runs. We must be careful not to have intermediary files in the different runs with the
    # same name but different content.

    indicationFolder <- file.path(outputFolder, indicationId)
    cmFolder <- file.path(indicationFolder, "cmOutput")
    exposureSummary <- read.csv(file.path(indicationFolder,
                                          "pairedExposureSummaryFilteredBySize.csv"))
    pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Legend")
    hois <- read.csv(pathToCsv)
    hois <- hois[hois$indicationId == indicationId, ]
    pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Legend")
    negativeControls <- read.csv(pathToCsv)
    negativeControls <- negativeControls[negativeControls$indicationId == indicationId, ]
    injectionSummary <- read.csv(file.path(indicationFolder, "signalInjectionSummary.csv"))

    createTcos <- function(i, positiveControls = "exclude", reverse = FALSE) {
        if (reverse) {
            comparatorId <- exposureSummary$targetId[i]
            targetId <- exposureSummary$comparatorId[i]
        } else {
            targetId <- exposureSummary$targetId[i]
            comparatorId <- exposureSummary$comparatorId[i]
        }
        fileName <- file.path(cmFolder,
                              paste0("CmData_l1_t", targetId, "_c", comparatorId),
                              "outcomes.rds")
        outcomes <- readRDS(fileName)
        outcomeIds <- unique(outcomes$outcomeId)
        if (positiveControls == "exclude") {
            outcomeIds <- outcomeIds[outcomeIds %in% c(hois$cohortId, negativeControls$cohortId)]
        } else if (positiveControls == "onlyTarget") {
            outcomeIds <- outcomeIds[outcomeIds %in% injectionSummary$newOutcomeId[injectionSummary$exposureId ==
                                                                                       targetId]]
        } else if (positiveControls == "onlyComparator") {
            outcomeIds <- outcomeIds[outcomeIds %in% injectionSummary$newOutcomeId[injectionSummary$exposureId ==
                                                                                       comparatorId]]
        } else if (positiveControls == "excludeComparator") {
            outcomeIds <- outcomeIds[!(outcomeIds %in% injectionSummary$newOutcomeId[injectionSummary$exposureId ==
                                                                                         comparatorId])]
        } else {
            stop("Unknown positive control setting: ", positiveControls)
        }
        tco <- CohortMethod::createTargetComparatorOutcomes(targetId = targetId,
                                                            comparatorId = comparatorId,
                                                            outcomeIds = outcomeIds)
        return(tco)
    }

    cmAnalysisListFile <- system.file("settings",
                                      sprintf("cmAnalysisList%s.json", indicationId),
                                      package = "Legend")
    cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)

    cmAnalysisListFile <- system.file("settings",
                                      sprintf("cmAnalysisListAsym%s.json", indicationId),
                                      package = "Legend")
    cmAnalysisListAsym <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)

    cmAnalysisListFile <- system.file("settings",
                                      sprintf("cmAnalysisListInteractions%s.json", indicationId),
                                      package = "Legend")
    cmAnalysisListInteractions <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)

    # First run: Forward pairs only, no positive controls ---------------------------------

    tcos <- lapply(1:nrow(exposureSummary), createTcos, positiveControls = "exclude", reverse = FALSE)
    cmAnalyses <- c(cmAnalysisList, cmAnalysisListAsym, cmAnalysisListInteractions)
    CohortMethod::runCmAnalyses(connectionDetails = NULL,
                                cdmDatabaseSchema = NULL,
                                exposureDatabaseSchema = NULL,
                                exposureTable = NULL,
                                outcomeDatabaseSchema = NULL,
                                outcomeTable = NULL,
                                outputFolder = cmFolder,
                                oracleTempSchema = NULL,
                                cmAnalysisList = cmAnalyses,
                                cdmVersion = 5,
                                targetComparatorOutcomesList = tcos,
                                getDbCohortMethodDataThreads = 1,
                                createStudyPopThreads = min(4, maxCores),
                                createPsThreads = max(1, round(maxCores/10)),
                                psCvThreads = min(10, maxCores),
                                trimMatchStratifyThreads = min(4, maxCores),
                                prefilterCovariatesThreads = min(5, maxCores),
                                fitOutcomeModelThreads = min(10, maxCores),
                                outcomeCvThreads = min(10, maxCores),
                                refitPsForEveryOutcome = FALSE,
                                refitPsForEveryStudyPopulation = FALSE,
                                prefilterCovariates = TRUE,
                                outcomeIdsOfInterest = hois$cohortId)
    file.rename(from = file.path(indicationFolder, "cmOutput", "outcomeModelReference.rds"),
                to = file.path(indicationFolder, "cmOutput", "outcomeModelReference1.rds"))

    # Second run: Forward paris only, only target positive controls ---------------------------------

    tcos <- lapply(1:nrow(exposureSummary),
                   createTcos,
                   positiveControls = "onlyTarget",
                   reverse = FALSE)
    cmAnalyses <- c(cmAnalysisList, cmAnalysisListAsym)
    CohortMethod::runCmAnalyses(connectionDetails = NULL,
                                cdmDatabaseSchema = NULL,
                                exposureDatabaseSchema = NULL,
                                exposureTable = NULL,
                                outcomeDatabaseSchema = NULL,
                                outcomeTable = NULL,
                                outputFolder = cmFolder,
                                oracleTempSchema = NULL,
                                cmAnalysisList = cmAnalyses,
                                cdmVersion = 5,
                                targetComparatorOutcomesList = tcos,
                                getDbCohortMethodDataThreads = 1,
                                createStudyPopThreads = min(4, maxCores),
                                createPsThreads = max(1, round(maxCores/10)),
                                psCvThreads = min(10, maxCores),
                                trimMatchStratifyThreads = min(4, maxCores),
                                prefilterCovariatesThreads = min(5, maxCores),
                                fitOutcomeModelThreads = min(10, maxCores),
                                outcomeCvThreads = min(2, maxCores),
                                refitPsForEveryOutcome = FALSE,
                                refitPsForEveryStudyPopulation = FALSE,
                                prefilterCovariates = TRUE,
                                outcomeIdsOfInterest = hois$cohortId)
    file.rename(from = file.path(indicationFolder, "cmOutput", "outcomeModelReference.rds"),
                to = file.path(indicationFolder, "cmOutput", "outcomeModelReference2.rds"))

    # Third run: Forward pairs only, only comparator positive controls ----------------

    tcos <- lapply(1:nrow(exposureSummary),
                   createTcos,
                   positiveControls = "onlyComparator",
                   reverse = FALSE)
    cmAnalyses <- c(cmAnalysisList)
    CohortMethod::runCmAnalyses(connectionDetails = NULL,
                                cdmDatabaseSchema = NULL,
                                exposureDatabaseSchema = NULL,
                                exposureTable = NULL,
                                outcomeDatabaseSchema = NULL,
                                outcomeTable = NULL,
                                outputFolder = cmFolder,
                                oracleTempSchema = NULL,
                                cmAnalysisList = cmAnalyses,
                                cdmVersion = 5,
                                targetComparatorOutcomesList = tcos,
                                getDbCohortMethodDataThreads = 1,
                                createStudyPopThreads = min(4, maxCores),
                                createPsThreads = max(1, round(maxCores/10)),
                                psCvThreads = min(10, maxCores),
                                trimMatchStratifyThreads = min(4, maxCores),
                                prefilterCovariatesThreads = min(5, maxCores),
                                fitOutcomeModelThreads = min(10, maxCores),
                                outcomeCvThreads = min(2, maxCores),
                                refitPsForEveryOutcome = FALSE,
                                refitPsForEveryStudyPopulation = FALSE,
                                prefilterCovariates = TRUE,
                                outcomeIdsOfInterest = hois$cohortId)
    file.rename(from = file.path(indicationFolder, "cmOutput", "outcomeModelReference.rds"),
                to = file.path(indicationFolder, "cmOutput", "outcomeModelReference3.rds"))

    # Fourth run: Reverse pairs, exclude comparator controls ------------------------------------

    # Create reverse (comparator-target) cohortMethodData and ps objects: Warning: cohortMethodData will
    # not have covariate and covariateRef data, since we won't need them (PS is already computed)
    pathToRds <- file.path(indicationFolder, "cmOutput", "outcomeModelReference1.rds")
    reference <- readRDS(pathToRds)
    reference <- reference[order(reference$cohortMethodDataFolder), ]
    reference <- reference[!duplicated(reference$cohortMethodDataFolder), ]
    ParallelLogger::logInfo("Making cohortMethodData and ps objects symmetrical")
    addOtherHalf <- function(i) {
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
    plyr::llply(1:nrow(reference), addOtherHalf, .progress = "text")

    tcos <- lapply(1:nrow(exposureSummary),
                   createTcos,
                   positiveControl = "excludeComparator",
                   reverse = TRUE)
    cmAnalyses <- c(cmAnalysisListAsym)
    CohortMethod::runCmAnalyses(connectionDetails = NULL,
                                cdmDatabaseSchema = NULL,
                                exposureDatabaseSchema = NULL,
                                exposureTable = NULL,
                                outcomeDatabaseSchema = NULL,
                                outcomeTable = NULL,
                                outputFolder = cmFolder,
                                oracleTempSchema = NULL,
                                cmAnalysisList = cmAnalyses,
                                cdmVersion = 5,
                                targetComparatorOutcomesList = tcos,
                                getDbCohortMethodDataThreads = 1,
                                createStudyPopThreads = min(4, maxCores),
                                createPsThreads = max(1, round(maxCores/10)),
                                psCvThreads = min(10, maxCores),
                                trimMatchStratifyThreads = min(4, maxCores),
                                fitOutcomeModelThreads = min(8, maxCores),
                                outcomeCvThreads = min(2, maxCores),
                                refitPsForEveryOutcome = FALSE,
                                refitPsForEveryStudyPopulation = FALSE,
                                prefilterCovariates = TRUE,
                                outcomeIdsOfInterest = hois$cohortId)
    file.rename(from = file.path(indicationFolder, "cmOutput", "outcomeModelReference.rds"),
                to = file.path(indicationFolder, "cmOutput", "outcomeModelReference4.rds"))

    # Create analysis summaries -------------------------------------------------------------------
    outcomeModelReference1 <- readRDS(file.path(indicationFolder,
                                                "cmOutput",
                                                "outcomeModelReference1.rds"))
    outcomeModelReference2 <- readRDS(file.path(indicationFolder,
                                                "cmOutput",
                                                "outcomeModelReference2.rds"))
    outcomeModelReference3 <- readRDS(file.path(indicationFolder,
                                                "cmOutput",
                                                "outcomeModelReference3.rds"))
    outcomeModelReference4 <- readRDS(file.path(indicationFolder,
                                                "cmOutput",
                                                "outcomeModelReference4.rds"))

    # Check to make sure no file names were used twice:
    if (any(outcomeModelReference1$studyPopFile != "" & outcomeModelReference1$studyPopFile %in% outcomeModelReference2$studyPopFile)) {
        stop("Overlapping studyPop files detected between run 1 and 2")
    }
    if (any(outcomeModelReference1$studyPopFile != "" & outcomeModelReference1$studyPopFile %in% outcomeModelReference3$studyPopFile)) {
        stop("Overlapping studyPop files detected between run 1 and 3")
    }
    if (any(outcomeModelReference1$studyPopFile != "" & outcomeModelReference1$studyPopFile %in% outcomeModelReference4$studyPopFile)) {
        stop("Overlapping studyPop files detected between run 1 and 4")
    }
    if (any(outcomeModelReference2$studyPopFile != "" & outcomeModelReference2$studyPopFile %in% outcomeModelReference3$studyPopFile)) {
        stop("Overlapping studyPop files detected between run 2 and 3")
    }
    if (any(outcomeModelReference2$studyPopFile != "" & outcomeModelReference2$studyPopFile %in% outcomeModelReference4$studyPopFile)) {
        stop("Overlapping studyPop files detected between run 2 and 4")
    }
    if (any(outcomeModelReference3$studyPopFile != "" & outcomeModelReference3$studyPopFile %in% outcomeModelReference4$studyPopFile)) {
        stop("Overlapping studyPop files detected between run 3 and 4")
    }
    if (any(outcomeModelReference1$strataFile != "" & outcomeModelReference1$strataFile %in% outcomeModelReference2$strataFile)) {
        stop("Overlapping strataFile files detected between run 1 and 2")
    }
    if (any(outcomeModelReference1$strataFile != "" & outcomeModelReference1$strataFile %in% outcomeModelReference3$strataFile)) {
        stop("Overlapping strata files detected between run 1 and 3")
    }
    if (any(outcomeModelReference1$strataFile != "" & outcomeModelReference1$strataFile %in% outcomeModelReference4$strataFile)) {
        stop("Overlapping strata files detected between run 1 and 4")
    }
    if (any(outcomeModelReference2$strataFile != "" & outcomeModelReference2$strataFile %in% outcomeModelReference3$strataFile)) {
        stop("Overlapping strata files detected between run 2 and 3")
    }
    if (any(outcomeModelReference2$strataFile != "" & outcomeModelReference2$strataFile %in% outcomeModelReference4$strataFile)) {
        stop("Overlapping strata files detected between run 2 and 4")
    }
    if (any(outcomeModelReference3$strataFile != "" & outcomeModelReference3$strataFile %in% outcomeModelReference4$strataFile)) {
        stop("Overlapping strata files detected between run 3 and 4")
    }

    ParallelLogger::logInfo("Summarizing results")
    analysesSum <- CohortMethod::summarizeAnalyses(referenceTable = outcomeModelReference1,
                                                   outputFolder = cmFolder)
    write.csv(analysesSum, file.path(indicationFolder, "analysisSummary1.csv"), row.names = FALSE)

    analysesSum <- CohortMethod::summarizeAnalyses(referenceTable = outcomeModelReference2,
                                                   outputFolder = cmFolder)
    write.csv(analysesSum, file.path(indicationFolder, "analysisSummary2.csv"), row.names = FALSE)

    analysesSum <- CohortMethod::summarizeAnalyses(referenceTable = outcomeModelReference3,
                                                   outputFolder = cmFolder)
    write.csv(analysesSum, file.path(indicationFolder, "analysisSummary3.csv"), row.names = FALSE)

    analysesSum <- CohortMethod::summarizeAnalyses(referenceTable = outcomeModelReference4,
                                                   outputFolder = cmFolder)
    write.csv(analysesSum, file.path(indicationFolder, "analysisSummary4.csv"), row.names = FALSE)
}

#' Create the analyses details
#'
#' @details
#' This function creates files specifying the analyses that will be performed.
#'
#' @param outputFolder   Name of local folder to place results; make sure to use forward slashes (/)
#'
#' @export
createAnalysesDetails <- function(outputFolder) {

    # Depression --------------------------------------------------------------

    # dummy args, will never be used because data objects have already been created:
    getDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(covariateSettings = FeatureExtraction::createCovariateSettings())

    createStudyPopArgsOnTreatment <- CohortMethod::createCreateStudyPopulationArgs(removeDuplicateSubjects = "keep first",
                                                                                   removeSubjectsWithPriorOutcome = TRUE,
                                                                                   riskWindowStart = 0,
                                                                                   riskWindowEnd = 0,
                                                                                   addExposureDaysToEnd = TRUE,
                                                                                   minDaysAtRisk = 1)

    createStudyPopArgsItt <- CohortMethod::createCreateStudyPopulationArgs(removeDuplicateSubjects = "keep first",
                                                                           removeSubjectsWithPriorOutcome = TRUE,
                                                                           riskWindowStart = 0,
                                                                           riskWindowEnd = 9999,
                                                                           addExposureDaysToEnd = FALSE,
                                                                           minDaysAtRisk = 1)

    subgroupCovariateIds <- c(1998, 2998, 3998, 4998, 5998, 6998, 7998)

    createPsArgs <- CohortMethod::createCreatePsArgs(control = Cyclops::createControl(noiseLevel = "silent",
                                                                                      cvType = "auto",
                                                                                      tolerance = 2e-07,
                                                                                      cvRepetitions = 1,
                                                                                      startingVariance = 0.01,
                                                                                      seed = 123),
                                                     stopOnError = FALSE,
                                                     excludeCovariateIds = subgroupCovariateIds,
                                                     maxCohortSizeForFitting = 1e+05)

    stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(numberOfStrata = 10,
                                                             baseSelection = "all")

    fitOutcomeModelArgs1 <- CohortMethod::createFitOutcomeModelArgs(stratified = TRUE,
                                                                    modelType = "cox")

    cmAnalysis1 <- CohortMethod::createCmAnalysis(analysisId = 1,
                                                  description = "PS stratification, on-treatment",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsOnTreatment,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  stratifyByPs = TRUE,
                                                  stratifyByPsArgs = stratifyByPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

    cmAnalysis2 <- CohortMethod::createCmAnalysis(analysisId = 2,
                                                  description = "PS stratification, intent-to-treat",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsItt,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  stratifyByPs = TRUE,
                                                  stratifyByPsArgs = stratifyByPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

    cmAnalysisList <- list(cmAnalysis1, cmAnalysis2)

    CohortMethod::saveCmAnalysisList(cmAnalysisList,
                                     file.path(outputFolder, "cmAnalysisListDepression.json"))

    matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(caliper = 0.2,
                                                       caliperScale = "standardized logit",
                                                       maxRatio = 100)

    cmAnalysis3 <- CohortMethod::createCmAnalysis(analysisId = 3,
                                                  description = "PS matching, on-treatment",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsOnTreatment,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  matchOnPs = TRUE,
                                                  matchOnPsArgs = matchOnPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

    cmAnalysis4 <- CohortMethod::createCmAnalysis(analysisId = 4,
                                                  description = "PS matching, intent-to-treat",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsItt,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  matchOnPs = TRUE,
                                                  matchOnPsArgs = matchOnPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

    cmAnalysisListAsym <- list(cmAnalysis3, cmAnalysis4)

    CohortMethod::saveCmAnalysisList(cmAnalysisListAsym,
                                     file.path(outputFolder, "cmAnalysisListAsymDepression.json"))

    fitOutcomeModelArgsI1998 <- CohortMethod::createFitOutcomeModelArgs(stratified = TRUE,
                                                                        modelType = "cox",
                                                                        interactionCovariateIds = 1998)

    cmAnalysis5 <- CohortMethod::createCmAnalysis(analysisId = 5,
                                                  description = "PS stratification, on-treatment, renal impairment interactions",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsOnTreatment,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  stratifyByPs = TRUE,
                                                  stratifyByPsArgs = stratifyByPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgsI1998)

    cmAnalysis6 <- CohortMethod::createCmAnalysis(analysisId = 6,
                                                  description = "PS stratification, intent-to-treat, renal impairment interactions",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsItt,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  stratifyByPs = TRUE,
                                                  stratifyByPsArgs = stratifyByPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgsI1998)


    fitOutcomeModelArgsI2998 <- CohortMethod::createFitOutcomeModelArgs(stratified = TRUE,
                                                                        modelType = "cox",
                                                                        interactionCovariateIds = 2998)

    cmAnalysis7 <- CohortMethod::createCmAnalysis(analysisId = 7,
                                                  description = "PS stratification, on-treatment, hepatic impairment interactions",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsOnTreatment,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  stratifyByPs = TRUE,
                                                  stratifyByPsArgs = stratifyByPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgsI2998)

    cmAnalysis8 <- CohortMethod::createCmAnalysis(analysisId = 8,
                                                  description = "PS stratification, intent-to-treat, hepatic impairment interactions",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsItt,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  stratifyByPs = TRUE,
                                                  stratifyByPsArgs = stratifyByPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgsI2998)
    fitOutcomeModelArgsI3998 <- CohortMethod::createFitOutcomeModelArgs(stratified = TRUE,
                                                                        modelType = "cox",
                                                                        interactionCovariateIds = 3998)

    cmAnalysis9 <- CohortMethod::createCmAnalysis(analysisId = 9,
                                                  description = "PS stratification, on-treatment, pregnancy interactions",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsOnTreatment,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  stratifyByPs = TRUE,
                                                  stratifyByPsArgs = stratifyByPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgsI3998)

    cmAnalysis10 <- CohortMethod::createCmAnalysis(analysisId = 10,
                                                   description = "PS stratification, intent-to-treat, pregnancy interactions",
                                                   getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                   createStudyPopArgs = createStudyPopArgsItt,
                                                   createPs = TRUE,
                                                   createPsArgs = createPsArgs,
                                                   stratifyByPs = TRUE,
                                                   stratifyByPsArgs = stratifyByPsArgs,
                                                   fitOutcomeModel = TRUE,
                                                   fitOutcomeModelArgs = fitOutcomeModelArgsI3998)

    fitOutcomeModelArgsI4998 <- CohortMethod::createFitOutcomeModelArgs(stratified = TRUE,
                                                                        modelType = "cox",
                                                                        interactionCovariateIds = 4998)

    cmAnalysis11 <- CohortMethod::createCmAnalysis(analysisId = 11,
                                                   description = "PS stratification, on-treatment, children interactions",
                                                   getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                   createStudyPopArgs = createStudyPopArgsOnTreatment,
                                                   createPs = TRUE,
                                                   createPsArgs = createPsArgs,
                                                   stratifyByPs = TRUE,
                                                   stratifyByPsArgs = stratifyByPsArgs,
                                                   fitOutcomeModel = TRUE,
                                                   fitOutcomeModelArgs = fitOutcomeModelArgsI4998)

    cmAnalysis12 <- CohortMethod::createCmAnalysis(analysisId = 12,
                                                   description = "PS stratification, intent-to-treat, children interactions",
                                                   getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                   createStudyPopArgs = createStudyPopArgsItt,
                                                   createPs = TRUE,
                                                   createPsArgs = createPsArgs,
                                                   stratifyByPs = TRUE,
                                                   stratifyByPsArgs = stratifyByPsArgs,
                                                   fitOutcomeModel = TRUE,
                                                   fitOutcomeModelArgs = fitOutcomeModelArgsI4998)

    fitOutcomeModelArgsI5998 <- CohortMethod::createFitOutcomeModelArgs(stratified = TRUE,
                                                                        modelType = "cox",
                                                                        interactionCovariateIds = 5998)

    cmAnalysis13 <- CohortMethod::createCmAnalysis(analysisId = 13,
                                                   description = "PS stratification, on-treatment, elderly interactions",
                                                   getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                   createStudyPopArgs = createStudyPopArgsOnTreatment,
                                                   createPs = TRUE,
                                                   createPsArgs = createPsArgs,
                                                   stratifyByPs = TRUE,
                                                   stratifyByPsArgs = stratifyByPsArgs,
                                                   fitOutcomeModel = TRUE,
                                                   fitOutcomeModelArgs = fitOutcomeModelArgsI5998)

    cmAnalysis14 <- CohortMethod::createCmAnalysis(analysisId = 14,
                                                   description = "PS stratification, intent-to-treat, elderly interactions",
                                                   getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                   createStudyPopArgs = createStudyPopArgsItt,
                                                   createPs = TRUE,
                                                   createPsArgs = createPsArgs,
                                                   stratifyByPs = TRUE,
                                                   stratifyByPsArgs = stratifyByPsArgs,
                                                   fitOutcomeModel = TRUE,
                                                   fitOutcomeModelArgs = fitOutcomeModelArgsI5998)

    fitOutcomeModelArgsI6998 <- CohortMethod::createFitOutcomeModelArgs(stratified = TRUE,
                                                                        modelType = "cox",
                                                                        interactionCovariateIds = 6998)

    cmAnalysis15 <- CohortMethod::createCmAnalysis(analysisId = 15,
                                                   description = "PS stratification, on-treatment, female interactions",
                                                   getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                   createStudyPopArgs = createStudyPopArgsOnTreatment,
                                                   createPs = TRUE,
                                                   createPsArgs = createPsArgs,
                                                   stratifyByPs = TRUE,
                                                   stratifyByPsArgs = stratifyByPsArgs,
                                                   fitOutcomeModel = TRUE,
                                                   fitOutcomeModelArgs = fitOutcomeModelArgsI6998)

    cmAnalysis16 <- CohortMethod::createCmAnalysis(analysisId = 16,
                                                   description = "PS stratification, intent-to-treat, female interactions",
                                                   getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                   createStudyPopArgs = createStudyPopArgsItt,
                                                   createPs = TRUE,
                                                   createPsArgs = createPsArgs,
                                                   stratifyByPs = TRUE,
                                                   stratifyByPsArgs = stratifyByPsArgs,
                                                   fitOutcomeModel = TRUE,
                                                   fitOutcomeModelArgs = fitOutcomeModelArgsI6998)

    cmAnalysisListInteractions <- list(cmAnalysis5,
                                       cmAnalysis6,
                                       cmAnalysis7,
                                       cmAnalysis8,
                                       cmAnalysis9,
                                       cmAnalysis10,
                                       cmAnalysis11,
                                       cmAnalysis12,
                                       cmAnalysis13,
                                       cmAnalysis14,
                                       cmAnalysis15,
                                       cmAnalysis16)

    CohortMethod::saveCmAnalysisList(cmAnalysisListInteractions,
                                     file.path(outputFolder,
                                               "cmAnalysisListInteractionsDepression.json"))

    # Hypertension --------------------------------------------------------------

    # Main analyses: just use same as depression

    CohortMethod::saveCmAnalysisList(cmAnalysisList,
                                     file.path(outputFolder, "cmAnalysisListHypertension.json"))

    CohortMethod::saveCmAnalysisList(cmAnalysisListAsym,
                                     file.path(outputFolder, "cmAnalysisListAsymHypertension.json"))

    # Interactions: add type 2 diabetes mellitus subgroup

    fitOutcomeModelArgsI7998 <- CohortMethod::createFitOutcomeModelArgs(stratified = TRUE,
                                                                        modelType = "cox",
                                                                        interactionCovariateIds = 7998)

    cmAnalysis17 <- CohortMethod::createCmAnalysis(analysisId = 17,
                                                   description = "PS stratification, on-treatment, T2DM interactions",
                                                   getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                   createStudyPopArgs = createStudyPopArgsOnTreatment,
                                                   createPs = TRUE,
                                                   createPsArgs = createPsArgs,
                                                   stratifyByPs = TRUE,
                                                   stratifyByPsArgs = stratifyByPsArgs,
                                                   fitOutcomeModel = TRUE,
                                                   fitOutcomeModelArgs = fitOutcomeModelArgsI7998)

    cmAnalysis18 <- CohortMethod::createCmAnalysis(analysisId = 18,
                                                   description = "PS stratification, intent-to-treat, T2DM interactions",
                                                   getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                   createStudyPopArgs = createStudyPopArgsItt,
                                                   createPs = TRUE,
                                                   createPsArgs = createPsArgs,
                                                   stratifyByPs = TRUE,
                                                   stratifyByPsArgs = stratifyByPsArgs,
                                                   fitOutcomeModel = TRUE,
                                                   fitOutcomeModelArgs = fitOutcomeModelArgsI7998)

    cmAnalysisListInteractions <- list(cmAnalysis5,
                                       cmAnalysis6,
                                       cmAnalysis7,
                                       cmAnalysis8,
                                       cmAnalysis9,
                                       cmAnalysis10,
                                       cmAnalysis11,
                                       cmAnalysis12,
                                       cmAnalysis13,
                                       cmAnalysis14,
                                       cmAnalysis15,
                                       cmAnalysis16,
                                       cmAnalysis17,
                                       cmAnalysis18)

    CohortMethod::saveCmAnalysisList(cmAnalysisListInteractions,
                                     file.path(outputFolder,
                                               "cmAnalysisListInteractionsHypertension.json"))
}

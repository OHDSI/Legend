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
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param indicationId         A string denoting the indicationId.
#' @param maxCores             How many parallel cores should be used? If more cores are made available
#'                             this can speed up the analyses.
#'
#' @export
runCohortMethod <- function(outputFolder, indicationId = "Depression", maxCores = 4) {
    indicationFolder <- file.path(outputFolder, indicationId)
    cmFolder <- file.path(indicationFolder, "cmOutput")
    exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
    pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Legend")
    hois <- read.csv(pathToCsv)
    hois <- hois[hois$indicationId == indicationId, ]
    pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Legend")
    negativeControls <- read.csv(pathToCsv)
    negativeControls <- negativeControls[negativeControls$indicationId == indicationId, ]

    createTcos <- function(i, excludePositiveControls = FALSE) {
        targetId <- exposureSummary$targetId[i]
        comparatorId <- exposureSummary$comparatorId[i]
        folderName <- file.path(cmFolder, paste0("CmData_l1_t", targetId, "_c", comparatorId))
        cmData <- CohortMethod::loadCohortMethodData(folderName, readOnly = TRUE)
        outcomeIds <-   attr(cmData$outcomes, "metaData")$outcomeIds
        if (excludePositiveControls) {
            outcomeIds <- outcomeIds[outcomeIds %in% c(hois$cohortId, negativeControls$cohortId)]
        }
        tco <- CohortMethod::createTargetComparatorOutcomes(targetId = targetId,
                                                            comparatorId = comparatorId,
                                                            outcomeIds = outcomeIds)
        return(tco)
    }

    # First run: no interaction terms, include positive controls ---------------------------------

    tcos <- lapply(1:nrow(exposureSummary), createTcos)
    cmAnalysisListFile <- system.file("settings",
                                      "cmAnalysisList.json",
                                      package = "Legend")
    cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)
    CohortMethod::runCmAnalyses(connectionDetails = NULL,
                                cdmDatabaseSchema = NULL,
                                exposureDatabaseSchema = NULL,
                                exposureTable = NULL,
                                outcomeDatabaseSchema = NULL,
                                outcomeTable = NULL,
                                outputFolder = cmFolder,
                                oracleTempSchema = NULL,
                                cmAnalysisList = cmAnalysisList,
                                cdmVersion = 5,
                                targetComparatorOutcomesList = tcos,
                                getDbCohortMethodDataThreads = 1,
                                createStudyPopThreads = min(4, maxCores),
                                createPsThreads = max(1, round(maxCores/10)),
                                psCvThreads = min(10, maxCores),
                                trimMatchStratifyThreads = min(4, maxCores),
                                prefilterCovariatesThreads = min(5, maxCores),
                                fitOutcomeModelThreads = min(8, maxCores),
                                outcomeCvThreads = min(2, maxCores),
                                refitPsForEveryOutcome = FALSE,
                                refitPsForEveryStudyPopulation = FALSE,
                                prefilterCovariates = TRUE,
                                outcomeIdsOfInterest = hois$cohortId)
    file.rename(from = file.path(indicationFolder, "cmOutput", "outcomeModelReference.rds"),
                to = file.path(indicationFolder, "cmOutput", "outcomeModelReference1.rds"))

    # Second run: only interaction terms, exclude positive controls ---------------------------------

    tcos <- lapply(1:nrow(exposureSummary), createTcos, excludePositiveControls = TRUE)
    cmAnalysisListFile <- system.file("settings",
                                      "cmAnalysisListInteractions.json",
                                      package = "Legend")
    cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)
    CohortMethod::runCmAnalyses(connectionDetails = NULL,
                                cdmDatabaseSchema = NULL,
                                exposureDatabaseSchema = NULL,
                                exposureTable = NULL,
                                outcomeDatabaseSchema = NULL,
                                outcomeTable = NULL,
                                outputFolder = cmFolder,
                                oracleTempSchema = NULL,
                                cmAnalysisList = cmAnalysisList,
                                cdmVersion = 5,
                                targetComparatorOutcomesList = tcos,
                                getDbCohortMethodDataThreads = 1,
                                createStudyPopThreads = min(4, maxCores),
                                createPsThreads = max(1, round(maxCores/10)),
                                psCvThreads = min(10, maxCores),
                                trimMatchStratifyThreads = min(4, maxCores),
                                prefilterCovariatesThreads = min(5, maxCores),
                                fitOutcomeModelThreads = max(1, round(maxCores/5)),
                                outcomeCvThreads = min(10, maxCores),
                                refitPsForEveryOutcome = FALSE,
                                refitPsForEveryStudyPopulation = FALSE,
                                prefilterCovariates = TRUE,
                                outcomeIdsOfInterest = hois$cohortId)
    file.rename(from = file.path(indicationFolder, "cmOutput", "outcomeModelReference.rds"),
                to = file.path(indicationFolder, "cmOutput", "outcomeModelReference2.rds"))

    # Third run: no interaction terms, asymmetrical (matching) ------------------------------------

    # Make cohortMethodData and ps objects symmetrical:
    pathToRds <- file.path(indicationFolder, "cmOutput", "outcomeModelReference1.rds")
    reference <- readRDS(pathToRds)
    reference <- reference[order(reference$cohortMethodDataFolder), ]
    reference <- reference[!duplicated(reference$cohortMethodDataFolder), ]
    OhdsiRTools::logInfo("Making cohortMethodData and ps objects symmetrical")
    addOtherHalf <- function(i) {
        sourceFolder <- reference$cohortMethodDataFolder[i]
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
        sourceFile <- reference$sharedPsFile[i]
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
    dummy <- plyr::llply(1:nrow(reference), addOtherHalf, .progress = "text")

    tcos <- lapply(1:nrow(exposureSummary), createTcos)
    switchTc <- function(tco) {
       temp <- tco$targetId
       tco$targetId <- tco$comparatorId
       tco$comparatorId <- temp
       return(tco)
    }
    otherTcos <- lapply(tcos, switchTc)
    tcos <- c(tcos, otherTcos)
    cmAnalysisListFile <- system.file("settings",
                                      "cmAnalysisListAsym.json",
                                      package = "Legend")
    cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)
    CohortMethod::runCmAnalyses(connectionDetails = NULL,
                                cdmDatabaseSchema = NULL,
                                exposureDatabaseSchema = NULL,
                                exposureTable = NULL,
                                outcomeDatabaseSchema = NULL,
                                outcomeTable = NULL,
                                outputFolder = cmFolder,
                                oracleTempSchema = NULL,
                                cmAnalysisList = cmAnalysisList,
                                cdmVersion = 5,
                                targetComparatorOutcomesList = tcos,
                                getDbCohortMethodDataThreads = 1,
                                createStudyPopThreads = min(4, maxCores),
                                createPsThreads = max(1, round(maxCores/10)),
                                psCvThreads = min(10, maxCores),
                                trimMatchStratifyThreads = min(4, maxCores),
                                prefilterCovariatesThreads = min(5, maxCores),
                                fitOutcomeModelThreads = max(1, round(maxCores/5)),
                                outcomeCvThreads = min(10, maxCores),
                                refitPsForEveryOutcome = FALSE,
                                refitPsForEveryStudyPopulation = FALSE,
                                prefilterCovariates = TRUE,
                                outcomeIdsOfInterest = hois$cohortId)
    file.rename(from = file.path(indicationFolder, "cmOutput", "outcomeModelReference.rds"),
                to = file.path(indicationFolder, "cmOutput", "outcomeModelReference3.rds"))


    # Create analysis summaries -------------------------------------------------------------------
    outcomeModelReference <- readRDS(file.path(indicationFolder, "cmOutput", "outcomeModelReference1.rds"))
    analysesSum <- CohortMethod::summarizeAnalyses(outcomeModelReference)
    write.csv(analysesSum, file.path(indicationFolder, "analysisSummary.csv"), row.names = FALSE)

    outcomeModelReference <- readRDS(file.path(indicationFolder, "cmOutput", "outcomeModelReference2.rds"))
    analysesSum <- CohortMethod::summarizeAnalyses(outcomeModelReference)
    write.csv(analysesSum, file.path(indicationFolder, "analysisSummaryInteractions.csv"), row.names = FALSE)

    outcomeModelReference <- readRDS(file.path(indicationFolder, "cmOutput", "outcomeModelReference3.rds"))
    analysesSum <- CohortMethod::summarizeAnalyses(outcomeModelReference)
    write.csv(analysesSum, file.path(indicationFolder, "analysisSummaryAsym.csv"), row.names = FALSE)
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

    subgroupCovariateIds <- c(1998, 2998, 3998, 4998, 5998, 6998)

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

    createPsArgs <- CohortMethod::createCreatePsArgs(control = Cyclops::createControl(noiseLevel = "silent",
                                                                                      cvType = "auto",
                                                                                      tolerance = 2e-07,
                                                                                      cvRepetitions = 1,
                                                                                      startingVariance = 0.01,
                                                                                      seed = 123),
                                                     stopOnError = FALSE,
                                                     excludeCovariateIds = subgroupCovariateIds,
                                                     maxCohortSizeForFitting = 100000)

    stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(numberOfStrata = 10, baseSelection = "all")

    matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(caliper = 0.2,
                                                       caliperScale = "standardized logit",
                                                       maxRatio = 100)

    fitOutcomeModelArgs1 <- CohortMethod::createFitOutcomeModelArgs(stratified = TRUE,
                                                                    modelType = "cox")

    fitOutcomeModelArgs2 <- CohortMethod::createFitOutcomeModelArgs(stratified = TRUE,
                                                                    modelType = "cox",
                                                                    interactionCovariateIds = subgroupCovariateIds)

    cmAnalysis1 <- CohortMethod::createCmAnalysis(analysisId = 1,
                                                  description = "PS stratification, on-treatment",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsOnTreatment,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  stratifyByPs =  TRUE,
                                                  stratifyByPsArgs = stratifyByPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

    cmAnalysis2 <- CohortMethod::createCmAnalysis(analysisId = 2,
                                                  description = "PS stratification, intent-to-treat",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsItt,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  stratifyByPs =  TRUE,
                                                  stratifyByPsArgs = stratifyByPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

    cmAnalysisList <- list(cmAnalysis1, cmAnalysis2)

    CohortMethod::saveCmAnalysisList(cmAnalysisList, file.path(outputFolder, "cmAnalysisList.json"))

    cmAnalysis3 <- CohortMethod::createCmAnalysis(analysisId = 3,
                                                  description = "PS stratification, on-treatment, subgroup interactions",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsOnTreatment,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  stratifyByPs =  TRUE,
                                                  stratifyByPsArgs = stratifyByPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgs2)


    cmAnalysis4 <- CohortMethod::createCmAnalysis(analysisId = 4,
                                                  description = "PS stratification, intent-to-treat, subgroup interactions",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsItt,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  stratifyByPs =  TRUE,
                                                  stratifyByPsArgs = stratifyByPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgs2)

    cmAnalysisListInteractions <- list(cmAnalysis3, cmAnalysis4)

    CohortMethod::saveCmAnalysisList(cmAnalysisListInteractions, file.path(outputFolder, "cmAnalysisListInteractions.json"))

    cmAnalysis5 <- CohortMethod::createCmAnalysis(analysisId = 5,
                                                  description = "PS matching, on-treatment",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsOnTreatment,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  matchOnPs =  TRUE,
                                                  matchOnPsArgs = matchOnPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

    cmAnalysis6 <- CohortMethod::createCmAnalysis(analysisId = 6,
                                                  description = "PS matching, intent-to-treat",
                                                  getDbCohortMethodDataArgs = getDbCmDataArgs,
                                                  createStudyPopArgs = createStudyPopArgsItt,
                                                  createPs = TRUE,
                                                  createPsArgs = createPsArgs,
                                                  matchOnPs =  TRUE,
                                                  matchOnPsArgs = matchOnPsArgs,
                                                  fitOutcomeModel = TRUE,
                                                  fitOutcomeModelArgs = fitOutcomeModelArgs1)

    cmAnalysisList <- list(cmAnalysis5, cmAnalysis6)

    CohortMethod::saveCmAnalysisList(cmAnalysisList, file.path(outputFolder, "cmAnalysisListAsym.json"))

}

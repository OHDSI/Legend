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
#' @param indicationFolder           Name of local folder to place results; make sure to use forward slashes
#'                             (/)
#' @param maxCores             How many parallel cores should be used? If more cores are made available
#'                             this can speed up the analyses.
#'
#' @export
runCohortMethod <- function(outputFolder, indication = "Depression", maxCores = 4) {
    indicationFolder <- file.path(outputFolder, indication)
    cmFolder <- file.path(indicationFolder, "cmOutput")
    exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
    pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Legend")
    hois <- read.csv(pathToCsv)
    hois <- hois[hois$indication == indication, ]
    pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Legend")
    negativeControls <- read.csv(pathToCsv)
    negativeControls <- negativeControls[negativeControls$indication == indication, ]

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


    # Create analysis summaries
    outcomeModelReference <- readRDS(file.path(indicationFolder, "cmOutput", "outcomeModelReference1.rds"))
    analysesSum <- CohortMethod::summarizeAnalyses(outcomeModelReference)
    write.csv(analysesSum, file.path(indicationFolder, "analysisSummary.csv"), row.names = FALSE)

    outcomeModelReference <- readRDS(file.path(indicationFolder, "cmOutput", "outcomeModelReference2.rds"))
    analysesSum <- CohortMethod::summarizeAnalyses(outcomeModelReference)
    write.csv(analysesSum, file.path(indicationFolder, "analysisSummaryInteractions.csv"), row.names = FALSE)
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
}

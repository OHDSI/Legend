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

#' Compute covariate balance for each comparison
#'
#' @details
#' Compute covariate alance for each comparison. The balance is computed considering the entire target and comparator cohort,
#' so note removing subjects with prior outcomes. Stores the results in a subfolder called 'balance'.
#'
#' @param indicationId           A string denoting the indicationId for which the exposure cohorts should be created.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/)
#' @param maxCores             How many parallel cores should be used? If more cores are made available
#'                             this can speed up the analyses.
#'
#' @export
computeCovariateBalance <- function(indicationId = "Depression",
                                    outputFolder,
                                    maxCores) {
    ParallelLogger::logInfo("Computing covariate balance")
    indicationFolder <- file.path(outputFolder, indicationId)
    exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
    balanceFolder <- file.path(indicationFolder, "balance")
    if (!file.exists(balanceFolder)) {
        dir.create(balanceFolder, recursive = TRUE)
    }
    # Using ITT study population, stratification, and matching arguments:
    cmAnalysisListFile <- system.file("settings",
                                      sprintf("cmAnalysisList%s.json", indicationId),
                                      package = "Legend")
    cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)
    studyPopArgs <- cmAnalysisList[[2]]$createStudyPopArgs
    stratifyByPsArgs <- cmAnalysisList[[2]]$stratifyByPsArgs
    cmAnalysisListFile <- system.file("settings",
                                      sprintf("cmAnalysisListAsym%s.json", indicationId),
                                      package = "Legend")
    cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)
    matchOnPsArgs <- cmAnalysisList[[2]]$matchOnPsArgs

    # Load restricted set of covariates to use for per-outcome balance:
    csvFile <- system.file("settings",
                           "Table1Specs.csv",
                           package = "Legend")
    table1Specs <- read.csv(csvFile)
    analysisIds <- table1Specs$analysisId[table1Specs$covariateIds == ""]
    covariateIds <- table1Specs$covariateIds[table1Specs$covariateIds != ""]
    covariateIds <- strsplit(as.character(covariateIds), ";")
    covariateIds <- do.call("c", covariateIds)
    covariateIds <- as.numeric(covariateIds)
    covarSubsetIds <- list(analysisIds = analysisIds,
                           covariateIds = covariateIds)

    # Load outcomeModelReference:
    pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds")
    outcomeModelReference1 <- readRDS(pathToRds)
    outcomeModelReference1 <- outcomeModelReference1[outcomeModelReference1$analysisId %in% 1:4, ]
    pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference4.rds")
    outcomeModelReference4 <- readRDS(pathToRds)
    outcomeModelReference4 <- outcomeModelReference4[outcomeModelReference4$analysisId %in% 1:4, ]
    outcomeModelReference <- rbind(outcomeModelReference1, outcomeModelReference4)
    pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Legend")
    outcomesOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)
    outcomesOfInterest <- outcomesOfInterest[outcomesOfInterest$indicationId == indicationId, ]
    outcomeModelReference <- outcomeModelReference[outcomeModelReference$outcomeId %in% outcomesOfInterest$cohortId, ]
    computeBalance <- function(exposureSummaryRow, studyPopArgs, stratifyByPsArgs, matchOnPsArgs, indicationFolder, balanceFolder, covarSubsetIds, outcomeModelReference) {
        ParallelLogger::logTrace("Computing balance for target ", exposureSummaryRow$targetId, " and comparator ",  exposureSummaryRow$comparatorId)
        referenceSubset <- outcomeModelReference[outcomeModelReference$targetId == exposureSummaryRow$targetId &
                                                     outcomeModelReference$comparatorId == exposureSummaryRow$comparatorId, ]
        cmDataFolder <-file.path(indicationFolder, "cmOutput", referenceSubset$cohortMethodDataFolder[1])
        cmData <- CohortMethod::loadCohortMethodData(cmDataFolder)
        referenceSubsetCt <- outcomeModelReference[outcomeModelReference$targetId == exposureSummaryRow$comparatorId &
                                                       outcomeModelReference$comparatorId == exposureSummaryRow$targetId, ]
        cmDataCtFolder <-file.path(indicationFolder, "cmOutput", referenceSubsetCt$cohortMethodDataFolder[1])
        cmDataCt <- CohortMethod::loadCohortMethodData(cmDataCtFolder)
        # Reverse cohortMethodData objects have no covariate data. Add back in:
        cmDataCt$covariates <- cmData$covariates
        cmDataCt$covariateRef <- cmData$covariateRef
        psFile <- file.path(indicationFolder, "cmOutput", referenceSubset$sharedPsFile[1])
        ps <- readRDS(psFile)
        # Compute balance when stratifying. Not specific to one outcome, so create hypothetical study population --------
        studyPop <- CohortMethod::createStudyPopulation(cohortMethodData = cmData,
                                                        population = ps,
                                                        firstExposureOnly = studyPopArgs$firstExposureOnly,
                                                        restrictToCommonPeriod = studyPopArgs$restrictToCommonPeriod,
                                                        washoutPeriod = studyPopArgs$washoutPeriod,
                                                        removeDuplicateSubjects = studyPopArgs$removeDuplicateSubjects,
                                                        removeSubjectsWithPriorOutcome = studyPopArgs$removeSubjectsWithPriorOutcome,
                                                        priorOutcomeLookback = studyPopArgs$priorOutcomeLookback,
                                                        minDaysAtRisk = studyPopArgs$minDaysAtRisk,
                                                        riskWindowStart = studyPopArgs$riskWindowStart,
                                                        addExposureDaysToStart = studyPopArgs$addExposureDaysToStart,
                                                        riskWindowEnd = studyPopArgs$riskWindowEnd,
                                                        addExposureDaysToEnd = studyPopArgs$addExposureDaysToEnd,
                                                        censorAtNewRiskWindow = studyPopArgs$censorAtNewRiskWindow)
        stratifiedPop <- CohortMethod::stratifyByPs(population = studyPop,
                                                    numberOfStrata = stratifyByPsArgs$numberOfStrata,
                                                    baseSelection = stratifyByPsArgs$baseSelection)
        fileName <- file.path(balanceFolder, paste0("Bal_t",
                                                    exposureSummaryRow$targetId,
                                                    "_c",
                                                    exposureSummaryRow$comparatorId,
                                                    "_a2.rds"))
        if (!file.exists(fileName)) {
            balance <- CohortMethod::computeCovariateBalance(population = stratifiedPop,
                                                             cohortMethodData = cmData)
            saveRDS(balance, fileName)
        }

        # Compute balance when matching. Not specific to one outcome, so use hypothetical study population -----------------
        matchedPop <- CohortMethod::matchOnPs(population = studyPop,
                                              caliper = matchOnPsArgs$caliper,
                                              caliperScale = matchOnPsArgs$caliperScale,
                                              maxRatio = matchOnPsArgs$maxRatio)
        fileName <- file.path(balanceFolder, paste0("Bal_t",
                                                    exposureSummaryRow$targetId,
                                                    "_c",
                                                    exposureSummaryRow$comparatorId,
                                                    "_a4.rds"))
        if (nrow(matchedPop) != 0 && !file.exists(fileName)) {
            balance <- CohortMethod::computeCovariateBalance(population = matchedPop,
                                                             cohortMethodData = cmData)
            saveRDS(balance, fileName)
        }

        # Matching is asymmetrical, so flip. Not specific to one outcome, so use hypothetical study population -----------------
        studyPopCt <- studyPop
        studyPopCt$treatment <-  1 - studyPopCt$treatment
        matchedPopCt <- CohortMethod::matchOnPs(population = studyPopCt,
                                                caliper = matchOnPsArgs$caliper,
                                                caliperScale = matchOnPsArgs$caliperScale,
                                                maxRatio = matchOnPsArgs$maxRatio)
        fileName <- file.path(balanceFolder, paste0("Bal_t",
                                                    exposureSummaryRow$comparatorId,
                                                    "_c",
                                                    exposureSummaryRow$targetId,
                                                    "_a4.rds"))
        if (nrow(matchedPopCt) != 0 && !file.exists(fileName)) {
            balance <- CohortMethod::computeCovariateBalance(population = matchedPopCt,
                                                             cohortMethodData = cmDataCt)
            saveRDS(balance, fileName)
        }
        # Compute balance within subgroups, both for stratification and matching. Not specific to one outcome --------------
        subgroupCovariateIds <- c(1998, 2998, 3998, 4998, 5998, 6998)
        for (subgroupCovariateId in subgroupCovariateIds) {
            fileName <- file.path(balanceFolder, paste0("Bal_t",
                                                        exposureSummaryRow$targetId,
                                                        "_c",
                                                        exposureSummaryRow$comparatorId,
                                                        "_s",
                                                        subgroupCovariateId,
                                                        "_a2.rds"))
            if (!file.exists(fileName)) {
                subgroupSize <- ffbase::sum.ff(cmData$covariates$covariateId == subgroupCovariateId)
                if (subgroupSize > 1000) {
                    balance <- CohortMethod::computeCovariateBalance(population = stratifiedPop,
                                                                     cohortMethodData = cmData,
                                                                     subgroupCovariateId = subgroupCovariateId)


                    saveRDS(balance, fileName)
                }
            }
            fileName <- file.path(balanceFolder, paste0("Bal_t",
                                                        exposureSummaryRow$targetId,
                                                        "_c",
                                                        exposureSummaryRow$comparatorId,
                                                        "_s",
                                                        subgroupCovariateId,
                                                        "_a4.rds"))
            if (nrow(matchedPop) != 0 && !file.exists(fileName)) {
                subgroupSize <- ffbase::sum.ff(cmData$covariates$covariateId == subgroupCovariateId)
                if (subgroupSize > 1000) {
                    balance <- CohortMethod::computeCovariateBalance(population = matchedPop,
                                                                     cohortMethodData = cmData,
                                                                     subgroupCovariateId = subgroupCovariateId)


                    saveRDS(balance, fileName)
                }
            }

        }
        # Compute balance per outcome. Restrict to subset of covariates to limit space ----------------------------------------------
        covariateIds <- cmData$covariateRef$covariateId[ffbase::`%in%`(cmData$covariateRef$analysisId, covarSubsetIds$analysisIds) |
                                                            ffbase::`%in%`(cmData$covariateRef$covariateId, covarSubsetIds$covariateIds)]
        cmDataSubset <- cmData
        cmDataSubset$covariates <- cmData$covariates[ffbase::`%in%`(cmData$covariates$covariateId, covariateIds), ]
        cmDataCtSubset <- cmDataCt
        cmDataCtSubset$covariates  <- cmDataSubset$covariates
        outcomeIds <- unique(referenceSubset$outcomeId)
        for (outcomeId in outcomeIds) {
            for (analysisId in unique(outcomeModelReference$analysisId)) {
                fileName <- file.path(balanceFolder, paste0("Bal_t",
                                                            exposureSummaryRow$targetId,
                                                            "_c",
                                                            exposureSummaryRow$comparatorId,
                                                            "_o",
                                                            outcomeId,
                                                            "_a",
                                                            analysisId,
                                                            ".rds"))
                if (!file.exists(fileName)) {
                    strataPopFile <- referenceSubset$strataFile[referenceSubset$outcomeId == outcomeId & referenceSubset$analysisId == analysisId]
                    strataPop <- readRDS(file.path(indicationFolder, "cmOutput", strataPopFile))
                    if (nrow(strataPop) != 0) {
                        balance <- CohortMethod::computeCovariateBalance(population = strataPop,
                                                                         cohortMethodData = cmDataSubset)


                        saveRDS(balance, fileName)
                    }
                }
                # See if reverse strata pop also exists (= matching)
                strataPopFile <- referenceSubsetCt$strataFile[referenceSubsetCt$outcomeId == outcomeId &
                                                                  referenceSubsetCt$analysisId == analysisId]
                if (length(strataPopFile) == 1) {
                    fileName <- file.path(balanceFolder, paste0("Bal_t",
                                                                exposureSummaryRow$comparatorId,
                                                                "_c",
                                                                exposureSummaryRow$targetId,
                                                                "_o",
                                                                outcomeId,
                                                                "_a",
                                                                analysisId,
                                                                ".rds"))
                    if (!file.exists(fileName)) {
                        strataPop <- readRDS(file.path(indicationFolder, "cmOutput", strataPopFile))
                        if (nrow(strataPop) != 0) {
                            balance <- CohortMethod::computeCovariateBalance(population = strataPop,
                                                                             cohortMethodData = cmDataCtSubset)


                            saveRDS(balance, fileName)
                        }
                    }
                }
            }
        }
    }

    cluster <- ParallelLogger::makeCluster(numberOfThreads = min(4, maxCores))
    exposureSummary$comparison <- paste(exposureSummary$targetId, exposureSummary$comparatorId, sep = "-")
    ParallelLogger::clusterApply(cluster = cluster,
                                 x = split(exposureSummary, exposureSummary$comparison),
                                 fun = computeBalance,
                                 studyPopArgs = studyPopArgs,
                                 stratifyByPsArgs = stratifyByPsArgs,
                                 matchOnPsArgs = matchOnPsArgs,
                                 indicationFolder = indicationFolder,
                                 balanceFolder = balanceFolder,
                                 covarSubsetIds = covarSubsetIds,
                                 outcomeModelReference = outcomeModelReference)
    ParallelLogger::stopCluster(cluster)
}


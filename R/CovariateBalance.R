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
    cmAnalysisListFile <- system.file("settings",
                                      "cmAnalysisList.json",
                                      package = "Legend")
    cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)

    # Using ITT study population and stratification arguments:
    studyPopArgs <- cmAnalysisList[[2]]$createStudyPopArgs
    stratifyByPsArgs <- cmAnalysisList[[2]]$stratifyByPsArgs
    computeBalance <- function(exposureSummaryRow, studyPopArgs, stratifyByPsArgs, indicationFolder, balanceFolder) {
        ParallelLogger::logTrace("Computing balance for target ", exposureSummaryRow$targetId, " and comparator ",  exposureSummaryRow$comparatorId)
        cmDataFolder <- file.path(indicationFolder, "cmOutput", paste0("CmData_l1_t", exposureSummaryRow$targetId, "_c", exposureSummaryRow$comparatorId))
        cmData <- CohortMethod::loadCohortMethodData(cmDataFolder)
        psFile <- file.path(indicationFolder, "cmOutput", paste0("Ps_l1_p1_t", exposureSummaryRow$targetId, "_c", exposureSummaryRow$comparatorId, ".rds"))
        ps <- readRDS(psFile)

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
        strataPop <- CohortMethod::stratifyByPs(population = studyPop,
                                                numberOfStrata = stratifyByPsArgs$numberOfStrata,
                                                baseSelection = stratifyByPsArgs$baseSelection)
        fileName <- file.path(balanceFolder, paste0("Bal_t",
                                                    exposureSummaryRow$targetId,
                                                    "_c",
                                                    exposureSummaryRow$comparatorId,
                                                    ".rds"))
        if (!file.exists(fileName)) {
            balance <- CohortMethod::computeCovariateBalance(population = strataPop,
                                                             cohortMethodData = cmData)
            saveRDS(balance, fileName)
        }
        subgroupCovariateIds <- c(1998, 2998, 3998, 4998, 5998, 6998)
        for (subgroupCovariateId in subgroupCovariateIds) {
            fileName <- file.path(balanceFolder, paste0("Bal_t",
                                                        exposureSummaryRow$targetId,
                                                        "_c",
                                                        exposureSummaryRow$comparatorId,
                                                        "_s",
                                                        subgroupCovariateId,
                                                        ".rds"))
            if (!file.exists(fileName)) {
                subgroupSize <- ffbase::sum.ff(cmData$covariates$covariateId == subgroupCovariateId)
                if (subgroupSize > 1000) {
                    balance <- CohortMethod::computeCovariateBalance(population = strataPop,
                                                                     cohortMethodData = cmData,
                                                                     subgroupCovariateId = subgroupCovariateId)


                    saveRDS(balance, fileName)
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
                              indicationFolder = indicationFolder,
                              balanceFolder = balanceFolder)
    ParallelLogger::stopCluster(cluster)
}


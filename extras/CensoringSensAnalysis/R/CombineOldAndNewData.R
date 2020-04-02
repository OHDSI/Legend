# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of CensoringSensAnalysis
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


#' @export
fetchNewCohorts <- function(connectionDetails,
                            cohortDatabaseSchema,
                            cohortTable,
                            newTargetId,
                            newComparatorId,
                            sensAnalysisFolder){
  connection <- DatabaseConnector::connect(connectionDetails)

  sql <- "SELECT *
  FROM @cohort_database_schema.@cohort_table
  WHERE cohort_definition_id IN (@cohort_ids);"

  cohorts <- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                                        sql = sql,
                                                        cohort_database_schema = cohortDatabaseSchema,
                                                        cohort_table = cohortTable,
                                                        cohort_ids = c(newTargetId, newComparatorId),
                                                        snakeCaseToCamelCase = TRUE)
  DatabaseConnector::disconnect(connection)
  saveRDS(cohorts, file.path(sensAnalysisFolder, "cohorts.rds"))

}

#' @export
combineOldAndNewData <- function(targetId,
                                 comparatorId,
                                 newTargetId,
                                 newComparatorId,
                                 indicationFolder,
                                 sensAnalysisFolder) {
  oldCmOutputFolder <- file.path(indicationFolder, "cmOutput")
  newCmOutputFolder <- file.path(sensAnalysisFolder, "cmOutput")
  if (!file.exists(newCmOutputFolder)) {
    dir.create(newCmOutputFolder)
  }
  omrFileNames <- file.path(oldCmOutputFolder, sprintf("outcomeModelReference%s.rds", 1:4))
  getOmrRows <- function(omrFileName) {
    omr <- readRDS(omrFileName)
    return(omr[omr$targetId == comparatorId & omr$comparatorId == targetId, ])
  }
  omr <- lapply(omrFileNames, getOmrRows)
  omr <- do.call(rbind, omr)

  # Analysis IDs 1 and 3 incidate on-treatment analyses:
  omr <- omr[omr$analysisId %in% c(1, 3), ]
  omr <- unique(omr)
  saveRDS(omr, file.path(newCmOutputFolder, "outcomeModelReference.rds"))


  newCmOutputFolder  <- file.path(sensAnalysisFolder, "cmOutput")

  newCohorts <- readRDS(file.path(sensAnalysisFolder, "cohorts.rds"))
  newCohorts$treatment <- as.numeric(newCohorts$cohortDefinitionId == newComparatorId)
  newCohorts$cohortDefinitionId <- NULL
  newCohorts$newDaysToCohortEnd <- newCohorts$cohortEndDate - newCohorts$cohortStartDate
  newCohorts$cohortEndDate <- NULL

  pathToCsv <- system.file("settings", "CohortsToCreate.csv", package = "CensoringSensAnalysis")
  cohortsToCreate <- read.csv(pathToCsv, stringsAsFactors = FALSE)
  targetName <- cohortsToCreate$name[cohortsToCreate$cohortId == newTargetId]
  comparatorName <- cohortsToCreate$name[cohortsToCreate$cohortId == newComparatorId]

  cohortMethodDataFolder <- unique(omr$cohortMethodDataFolder)
  if (length(cohortMethodDataFolder) != 1) {
    stop("Script not designed for multiple cohortMethodDataFolders")
  }
  cmData <- CohortMethod::loadCohortMethodData(file.path(oldCmOutputFolder, cohortMethodDataFolder))
  mergedCohorts <- dplyr::inner_join(cmData$cohorts, newCohorts, by = c("treatment", "subjectId", "cohortStartDate"))
  if (nrow(mergedCohorts) != nrow(cmData$cohorts)) {
    stop("Cohort mismatch in CohortMethodData")
  }
  cmData$cohorts <- mergedCohorts
  cmData$cohorts$daysToCohortEnd <- cmData$cohorts$newDaysToCohortEnd
  cmData$cohorts$newDaysToCohortEnd <- NULL
  CohortMethod::saveCohortMethodData(cmData, file.path(newCmOutputFolder, cohortMethodDataFolder))

  # Store change in follow up
  followUp <- rbind(quantile(mergedCohorts$daysToCohortEnd[mergedCohorts$treatment == 1], c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)),
                    quantile(mergedCohorts$daysToCohortEnd[mergedCohorts$treatment == 0], c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)),
                    quantile(mergedCohorts$newDaysToCohortEnd[mergedCohorts$treatment == 1], c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)),
                    quantile(mergedCohorts$newDaysToCohortEnd[mergedCohorts$treatment == 0], c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)))
  followUp <- as.data.frame(followUp)
  followUp$censor <- c(FALSE, FALSE, TRUE, TRUE)
  followUp$treatment <- c(comparatorName, targetName, comparatorName, targetName)
  write.csv(followUp, file.path(sensAnalysisFolder, "FollowUp.csv"), row.names = FALSE)

  writeLines(sprintf("%0.1f%% of %s exposures were censored because of switching",
                     100*mean(mergedCohorts$daysToCohortEnd[mergedCohorts$treatment == 1] != mergedCohorts$newDaysToCohortEnd[mergedCohorts$treatment == 1]),
                     comparatorName))
  writeLines(sprintf("%0.1f%% of %s exposures were censored because of switching",
                     100*mean(mergedCohorts$daysToCohortEnd[mergedCohorts$treatment == 0] != mergedCohorts$newDaysToCohortEnd[mergedCohorts$treatment == 0]),
                     targetName))

  sharedPsFile <- unique(omr$sharedPsFile)
  if (length(cohortMethodDataFolder) != 1) {
    stop("Script not designed for multiple sharedPsFiles")
  }
  ps <- readRDS(file.path(oldCmOutputFolder, sharedPsFile))
  mergedPs <- dplyr::inner_join(ps, newCohorts, by = c("treatment", "subjectId", "cohortStartDate"))
  if (nrow(mergedPs) != nrow(ps)) {
    stop("Cohort mismatch in shared PS file")
  }
  mergedPs$daysToCohortEnd <- cmData$cohorts$newDaysToCohortEnd
  mergedPs$newDaysToCohortEnd <- NULL
  saveRDS(mergedPs, file.path(newCmOutputFolder, sharedPsFile))
}

#' @export
computeNewEstimates <- function(sensAnalysisFolder) {

  indicationId <- "Hypertension"
  cmAnalysisListFile <- system.file("settings",
                                    sprintf("cmAnalysisList%s.json", indicationId),
                                    package = "Legend")
  cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)

  cmAnalysisListFile <- system.file("settings",
                                    sprintf("cmAnalysisListAsym%s.json", indicationId),
                                    package = "Legend")
  cmAnalysisListAsym <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)

  cmAnalysisList <- append(cmAnalysisList, cmAnalysisListAsym)
  if (!all(sapply(1:length(cmAnalysisList), function(i) cmAnalysisList[[i]]$analysisId == i))) {
    stop("Mismatch in analysis IDs")
  }

  newCmOutputFolder <- file.path(sensAnalysisFolder, "cmOutput")
  omr <- readRDS(file.path(newCmOutputFolder, "outcomeModelReference.rds"))

  analysisFolders <- file.path(newCmOutputFolder, unique(gsub("/.*", "", omr$outcomeModelFile)))
  createFolder <- function(analysisFolder) {
    if (!file.exists(analysisFolder)) {
      dir.create(analysisFolder)
    }
  }
  lapply(analysisFolders, createFolder)

  nNodes <- 8
  cluster <- ParallelLogger::makeCluster(nNodes)

  ParallelLogger::clusterApply(cluster = cluster,
                               fun = initVars,
                               1:nNodes,
                               cmAnalysisList = cmAnalysisList,
                               newCmOutputFolder = newCmOutputFolder,
                               sharedPsFile = omr$sharedPsFile[1],
                               cohortMethodDataFolder = omr$cohortMethodDataFolder[1])

  ParallelLogger::clusterApply(cluster = cluster, x = split(omr, 1:nrow(omr)), fun = computeOutcomeModel)
  # plyr::l_ply(split(omr, 1:nrow(omr)), computeOutcomeModel, .progress = "text")
  ParallelLogger::stopCluster(cluster)
  analysisSummary <- CohortMethod::summarizeAnalyses(referenceTable = omr, outputFolder = newCmOutputFolder)
  saveRDS(analysisSummary, file.path(sensAnalysisFolder, "analysisSummary.rds"))
}

#' @export
initVars <- function(dummy, cmAnalysisList, newCmOutputFolder, sharedPsFile, cohortMethodDataFolder) {
  .GlobalEnv$cmAnalysisList <- cmAnalysisList
  .GlobalEnv$newCmOutputFolder <- newCmOutputFolder
  .GlobalEnv$sharedPs <- readRDS(file.path(newCmOutputFolder, sharedPsFile))
  .GlobalEnv$cmData <- CohortMethod::loadCohortMethodData(file.path(newCmOutputFolder, cohortMethodDataFolder))
  return(NULL)
}

#' @export
computeOutcomeModel <- function(omrRow) {
  cmAnalysisList <- get("cmAnalysisList", envir = globalenv())
  newCmOutputFolder <- get("newCmOutputFolder", envir = globalenv())
  sharedPs <- get("sharedPs", envir = globalenv())
  cmData <- get("cmData", envir = globalenv())

  analysisId <- omrRow$analysisId
  args <- cmAnalysisList[[analysisId]]$createStudyPopArgs
  args$cohortMethodData <- cmData
  args$outcomeId <- omrRow$outcomeId
  studyPop <- do.call(CohortMethod::createStudyPopulation, args)
  if (omrRow$studyPopFile != "") {
    saveRDS(studyPop, file.path(newCmOutputFolder, omrRow$studyPopFile))
  }

  ps <- dplyr::inner_join(studyPop, sharedPs, by = c("rowId", "treatment", "subjectId", "cohortStartDate", "daysFromObsStart", "daysToObsEnd"))
  if (omrRow$psFile != "") {
    saveRDS(ps, file.path(newCmOutputFolder, omrRow$psFile))
  }
  # Switching treatment
  ps$treatment <- 1 - ps$treatment

  if (cmAnalysisList[[analysisId]]$stratifyByPs) {
    args <- cmAnalysisList[[analysisId]]$stratifyByPsArgs
    args$population <- ps
    strataPop <- do.call(CohortMethod::stratifyByPs, args)
  } else {
    args <- cmAnalysisList[[analysisId]]$matchOnPsArgs
    args$population <- ps
    strataPop <- do.call(CohortMethod::matchOnPs, args)
  }
  # Switching treatment
  strataPop$treatment <- 1 - strataPop$treatment

  if (omrRow$strataFile != "") {
    saveRDS(strataPop, file.path(newCmOutputFolder, omrRow$strataFile))
  }

  args <- cmAnalysisList[[analysisId]]$fitOutcomeModelArgs
  args$population <- strataPop
  outcomeModel <- do.call(CohortMethod::fitOutcomeModel, args)
  saveRDS(outcomeModel, file.path(newCmOutputFolder, omrRow$outcomeModelFile))
  return(NULL)
}

#' @export
calibrateResults <- function(indicationFolder, sensAnalysisFolder) {
  negativeControls <- read.csv(file.path(indicationFolder, "export", "negative_control_outcome.csv"))
  colnames(negativeControls) <- SqlRender::snakeCaseToCamelCase(colnames(negativeControls))
  postiveControls <- read.csv(file.path(indicationFolder, "export", "positive_control_outcome.csv"))
  colnames(postiveControls) <- SqlRender::snakeCaseToCamelCase(colnames(postiveControls))
  postiveControls <- postiveControls[postiveControls$exposureId == targetId, ]
  negativeControls$effectSize <- 1
  allControls <- rbind(negativeControls[, c("outcomeId", "effectSize")],
                       postiveControls[, c("outcomeId", "effectSize")])

  analysisSummary <- readRDS(file.path(sensAnalysisFolder, "analysisSummary.rds"))

  calibrateAnalysis <- function(subset) {
    subsetCt <- swapColumnContents(subset, "targetId", "comparatorId")
    subsetCt <- swapColumnContents(subset, "target", "comparator")
    subsetCt <- swapColumnContents(subset, "targetDays", "comparatorDays")
    subsetCt <- swapColumnContents(subset, "eventsTarget", "eventsComparator")
    subsetCt$rr <- 1/subsetCt$rr
    temp <- 1/subsetCt$ci95lb
    subsetCt$ci95lb <- 1/subsetCt$ci95ub
    subsetCt$ci95ub <- temp
    subsetCt$logRr <- -subsetCt$logRr


    ncs <- subsetCt[subsetCt$outcomeId %in% negativeControls$outcomeId, ]
    pcs <- merge(subsetCt, allControls)

    null <- EmpiricalCalibration::fitMcmcNull(logRr = ncs$logRr, seLogRr = ncs$seLogRr)
    calibratedP <- EmpiricalCalibration::calibrateP(null, logRr = subsetCt$logRr, seLogRr = subsetCt$seLogRr)
    subsetCt$calibratedP <- calibratedP$p
    # EmpiricalCalibration::plotCiCalibrationEffect(logRr = pcs$logRr, seLogRr = pcs$seLogRr, trueLogRr = log(pcs$effectSize))
    model <- EmpiricalCalibration::fitSystematicErrorModel(logRr = pcs$logRr, seLogRr = pcs$seLogRr, trueLogRr = log(pcs$effectSize), estimateCovarianceMatrix = FALSE)
    calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = subsetCt$logRr,
                                                                      seLogRr = subsetCt$seLogRr,
                                                                      model = model)
    subsetCt$calibratedRr <- exp(calibratedCi$logRr)
    subsetCt$calibratedCi95Lb <- exp(calibratedCi$logLb95Rr)
    subsetCt$calibratedCi95Ub <- exp(calibratedCi$logUb95Rr)
    subsetCt$calibratedLogRr <- calibratedCi$logRr
    subsetCt$calibratedSeLogRr <- calibratedCi$seLogRr
    return(subsetCt)

  }
  calibrated <- lapply(split(analysisSummary, analysisSummary$analysisId), calibrateAnalysis)
  calibrated <- do.call("rbind", calibrated)
  write.csv(calibrated, file.path(sensAnalysisFolder, "calibratedEstimates.csv"), row.names = FALSE)
}

#' @param
swapColumnContents <- function(df, column1 = "targetId", column2 = "comparatorId") {
  temp <- df[, column1]
  df[, column1] <- df[, column2]
  df[, column2] <- temp
  return(df)
}

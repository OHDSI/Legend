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

#' Create figures and tables for report
#'
#' @details
#' This function generates tables and figures for the report on the study results.
#'
#' @param exportFolders        Vector of names of local folders where the results were exported; make sure
#'                             to use forward slashes (/). D
#' @param maExportFolder       A local folder where the meta-anlysis results will be written.
#' @param maxCores             How many parallel cores should be used? If more cores are made available
#'                             this can speed up the analyses.
#'
#' @export
doMetaAnalysis <- function(exportFolders, maExportFolder, maxCores) {
    if (!file.exists(maExportFolder))
        dir.create(maExportFolder, recursive = TRUE)
    ParallelLogger::logInfo("Performing meta-analysis for main effects")
    doMaEffectType(exportFolders = exportFolders,
                   maExportFolder = maExportFolder,
                   maxCores = maxCores,
                   interactions = FALSE)
    ParallelLogger::logInfo("Performing meta-analysis for interaction effects")
    doMaEffectType(exportFolders = exportFolders,
                   maExportFolder = maExportFolder,
                   maxCores = maxCores,
                   interactions = TRUE)
    ParallelLogger::logInfo("Creating database table")
    database <- data.frame(database_id = "Meta-analysis",
                           database_name = "Random effects meta-analysis",
                           description = "Random effects meta-analysis using the DerSimonian-Laird estimator.",
                           is_meta_analysis = 1)
    fileName <- file.path(exportFolder, "database.csv")
    write.csv(database, fileName, row.names = FALSE)

}

doMaEffectType <- function(exportFolders,
                           maExportFolder,
                           maxCores,
                           interactions) {

    loadMainResults <- function(exportFolder) {
        ParallelLogger::logInfo("Loading main results from ", exportFolder, " for meta-analysis")
        zipFile <- list.files(exportFolder, "^Results.*.zip$")[1]
        utils::unzip(zipfile = zipFile,
                     files = c("cohort_method_result.csv",
                               "negative_control_outcome.csv",
                               "positive_control_outcome.csv"))
        results <- read.csv(file.path(exportFolder, "cohort_method_result.csv"))
        colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
        ncs <- read.csv(file.path(exportFolder, "negative_control_outcome.csv"))
        colnames(ncs) <- SqlRender::snakeCaseToCamelCase(colnames(ncs))
        pcs <- read.csv(file.path(exportFolder, "positive_control_outcome.csv"))
        colnames(pcs) <- SqlRender::snakeCaseToCamelCase(colnames(pcs))
        results$trueEffectSize <- NA
        idx <- results$outcomeId %in% ncs$outcomeId
        results$trueEffectSize[idx] <- 1
        idx <- results$outcomeId %in% pcs$outcomeId
        results$trueEffectSize[idx] <- pcs$effectSize[match(results$outcomeId[idx],
                                                            pcs$outcomeId)]
        return(results)
    }
    loadInteractionResults <- function(exportFolder) {
        ParallelLogger::logInfo("Loading interaction results from ", exportFolder, " for meta-analysis")
        zipFile <- list.files(exportFolder, "^Results.*.zip$")[1]
        utils::unzip(zipfile = zipFile,
                     files = c("cm_interaction_result.csv",
                               "negative_control_outcome.csv"))
        results <- read.csv(file.path(exportFolder, "cm_interaction_result.csv"))
        colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
        ncs <- read.csv(file.path(exportFolder, "negative_control_outcome.csv"))
        colnames(ncs) <- SqlRender::snakeCaseToCamelCase(colnames(ncs))
        results$trueEffectSize <- NA
        idx <- results$outcomeId %in% ncs$outcomeId
        results$trueEffectSize[idx] <- 1
        results$rr <- results$rrr
        results$logRr <- results$logRrr
        results$seLogRr <- results$seLogRrr
        return(results)
    }
    if (interactions) {
        allResults <- lapply(exportFolders, loadInteractionResults)
    } else {
        allResults <- lapply(exportFolders, loadMainResults)
    }
    allResults <- do.call(rbind, allResults)
    groups <- split(allResults, paste(allResults$targetId, allResults$comparatorId, allResults$analysisId))
    cluster <- ParallelLogger::makeCluster(min(maxCores, 10))
    results <- ParallelLogger::clusterApply(cluster, groups, computeGroupMetaAnalysis, interactions = interactions)
    ParallelLogger::stopCluster(cluster)
    results <- do.call(rbind, results)
    results$trueEffectSize <- NULL
    if (interactions) {
        results$rrr <- results$rr
        results$logRrr <- results$logRr
        results$seLogRrr <- results$seLogRr
        results$rr <- NULL
        results$logRr <- NULL
        results$seLogRr <- NULL
        fileName <-  file.path(maExportFolder, paste0("cm_interaction_result.csv"))
        write.csv(results, fileName, row.names = FALSE)
    } else {
        fileName <-  file.path(maExportFolder, paste0("cohort_method_result.csv"))
        write.csv(results, fileName, row.names = FALSE)
    }
}

computeGroupMetaAnalysis <- function(group, interactions) {
    # group <- groups[[2]]
    analysisId <- group$analysisId[1]
    targetId <- group$targetId[1]
    comparatorId <- group$comparatorId[1]
    ParallelLogger::logTrace("Performing meta-analysis for target ", targetId, ", comparator ", comparatorId, ", analysis", analysisId)
    outcomeGroups <- split(group, group$outcomeId)
    outcomeGroupResults <- lapply(outcomeGroups, computeSingleMetaAnalysis)
    groupResults <- do.call(rbind, outcomeGroupResults)
    ncs <- groupResults[groupResults$trueEffectSize == 1, ]
    validNcs <- sum(!is.na(ncs$seLogRr))
    if (validNcs >= 5) {
        null <- EmpiricalCalibration::fitMcmcNull(ncs$logRr, ncs$seLogRr)
        calibratedP <- EmpiricalCalibration::calibrateP(null = null,
                                                        logRr = groupResults$logRr,
                                                        seLogRr = groupResults$seLogRr)
        groupResults$calibratedP <- calibratedP$p
    } else {
        groupResults$calP <- NA
    }
    if (!interactions) {
        pcs <- groupResults[!is.na(groupResults$trueEffectSize) &
                                groupResults$trueEffectSize != 1, ]
        validPcs <- sum(!is.na(pcs$seLogRr))
        if (nrow(validPcs) > 5) {
            model <- EmpiricalCalibration::fitSystematicErrorModel(logRr = c(ncs$logRr, pcs$logRr),
                                                                   seLogRr = c(ncs$seLogRr,
                                                                               pcs$seLogRr),
                                                                   trueLogRr = c(rep(0, nrow(ncs)),
                                                                                 log(pcs$trueEffectSize)),
                                                                   estimateCovarianceMatrix = FALSE)
            calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = groupResults$logRr,
                                                                              seLogRr = groupResults$seLogRr,
                                                                              model = model)
            groupResults$calibratedRr <- exp(calibratedCi$logRr)
            groupResults$calibratedCi95Lb <- exp(calibratedCi$logLb95Rr)
            groupResults$calibratedCi95Ub <- exp(calibratedCi$logUb95Rr)
            groupResults$calibratedLogRr <- calibratedCi$logRr
            groupResults$calibratedSeLogRr <- calibratedCi$seLogRr
        } else {
            groupResults$calibratedRr <- rep(NA, nrow(groupResults))
            groupResults$calibratedCi95Lb <- rep(NA, nrow(groupResults))
            groupResults$calibratedCi95Ub <- rep(NA, nrow(groupResults))
            groupResults$calibratedLogRr <- rep(NA, nrow(groupResults))
            groupResults$calibratedSeLogRr <- rep(NA, nrow(groupResults))
        }
    }
    return(groupResults)
}

sumMinCellCount <- function(counts) {
  total <- sum(abs(counts))
  if (any(counts < 0)) {
      total <- -total
  }
  return(total)
}

computeSingleMetaAnalysis <- function(outcomeGroup) {
    # outcomeGroup <- outcomeGroups[[1]]
    maRow <- outcomeGroup[1, ]
    outcomeGroup <- outcomeGroup[!is.na(outcomeGroup$seLogRr), ]
    if (nrow(outcomeGroup) == 0) {
        maRow$targetSubjects <- 0
        maRow$comparatorSubjects <- 0
        maRow$targetDays <- 0
        maRow$comparatorDays <- 0
        maRow$targetOutcomes <- 0
        maRow$comparatorOutcomes <- 0
        maRow$rr <- NA
        maRow$ci95lb <- NA
        maRow$ci95ub <- NA
        maRow$p <- NA
        maRow$logRr <- NA
        maRow$seLogRr <- NA
        maRow$i2 <- NA
    } else if (nrow(outcomeGroup) == 1) {
        maRow <- outcomeGroup[1, ]
        maRow$i2 <- 0
    } else {
        maRow$targetSubjects <- sumMinCellCount(outcomeGroup$targetSubjects)
        maRow$comparatorSubjects <- sumMinCellCount(outcomeGroup$comparatorSubjects)
        maRow$targetDays <- sum(outcomeGroup$targetDays)
        maRow$comparatorDays <- sum(outcomeGroup$comparatorDays)
        maRow$targetOutcomes <- sumMinCellCount(outcomeGroup$targetOutcomes)
        maRow$comparatorOutcomes <- sumMinCellCount(outcomeGroup$comparatorOutcomes)
        meta <- meta::metagen(TE = outcomeGroup$logRr,
                              seTE = outcomeGroup$seLogRr,
                              sm = "RR",
                              hakn = FALSE)
        s <- summary(meta)
        maRow$i2 <- s$I2$TE
        rnd <- s$random
        maRow$rr <- exp(rnd$TE)
        maRow$ci95lb <- exp(rnd$lower)
        maRow$ci95ub <- exp(rnd$upper)
        maRow$p <- rnd$p
        maRow$logRr <- rnd$TE
        maRow$seLogRr <- rnd$seTE
    }
    maRow$databaseId <- "Meta-analysis"
    return(maRow)
}

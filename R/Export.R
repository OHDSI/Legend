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

#' Export all results to tables
#'
#' @description
#' Outputs all results to a folder called 'export', and zips them.
#'
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param databaseId           A short string for identifying the database (e.g. 'Synpuf').
#' @param databaseName         The full name of the database.
#' @param minCellCount         The minimum cell count for fields contains person counts or fractions.
#' @param maxCores             How many parallel cores should be used? If more cores are made available
#'                             this can speed up the analyses.
#'
#' @export
exportResults <- function(outputFolder,
                          databaseId,
                          databaseName,
                          minCellCount = 5,
                          maxCores) {
    exportFolder <- file.path(outputFolder, "export")
    if (!file.exists(exportFolder)) {
        dir.create(exportFolder, recursive = TRUE)
    }

    exportIndications(outputFolder = outputFolder,
                      exportFolder = exportFolder,
                      databaseId = databaseId)

    exportAnalyses(outputFolder = outputFolder,
                   exportFolder = exportFolder,
                   databaseId = databaseId)

    exportExposures(outputFolder = outputFolder,
                    exportFolder = exportFolder,
                    databaseId = databaseId)

    exportOutcomes(outputFolder = outputFolder,
                   exportFolder = exportFolder,
                   databaseId = databaseId)

    exportMetadata(outputFolder = outputFolder,
                   exportFolder = exportFolder,
                   databaseId = databaseId,
                   databaseName = databaseName,
                   minCellCount = minCellCount)

    exportMainResults(outputFolder = outputFolder,
                      exportFolder = exportFolder,
                      databaseId = databaseId,
                      minCellCount = minCellCount,
                      maxCores = maxCores)

    exportDiagnostics(outputFolder = outputFolder,
                      exportFolder = exportFolder,
                      databaseId = databaseId,
                      minCellCount = minCellCount,
                      maxCores = maxCores)

    # Add all to zip file -------------------------------------------------------------------------------
    ParallelLogger::logInfo("Adding results to zip file")
    zipName <- file.path(exportFolder, "Results.zip")
    files <- list.files(exportFolder, pattern = ".*\\.csv$")
    oldWd <- setwd(exportFolder)
    on.exit(setwd(oldWd))
    zip::zip(zipfile = zipName, files = files, recurse = FALSE)
    ParallelLogger::logInfo("Results are ready for sharing at:", zipName)
}

swapColumnContents <- function(df, column1 = "targetId", column2 = "comparatorId") {
    temp <- df[, column1]
    df[, column1] <- df[, column2]
    df[, column2] <- temp
    return(df)
}

exportIndications <- function(outputFolder,
                              exportFolder,
                              databaseId) {
    ParallelLogger::logInfo("Exporting indications")
    ParallelLogger::logInfo("- indication table")
    pathToCsv <- system.file("settings", "Indications.csv", package = "Legend")
    indications <- read.csv(pathToCsv)
    indications$definition <- ""
    indicationTable <- indications[, c("indicationId", "indicationName", "definition")]
    colnames(indicationTable) <- SqlRender::camelCaseToSnakeCase(colnames(indicationTable))
    fileName <- file.path(exportFolder, "indication.csv")
    write.csv(indicationTable, fileName, row.names = FALSE)
}

exportAnalyses <- function(outputFolder,
                           exportFolder,
                           databaseId) {
    ParallelLogger::logInfo("Exporting analyses")
    ParallelLogger::logInfo("- cohort_method_analysis table")
    pathToCsv <- system.file("settings", "Indications.csv", package = "Legend")
    indications <- read.csv(pathToCsv)

    tempFileName <- tempfile()
    loadCmAnalyses <- function(indicationId) {
        cmAnalysisListFile <- system.file("settings",
                                          sprintf("cmAnalysisList%s.json", indicationId),
                                          package = "Legend")
        if (cmAnalysisListFile == "") {
            return(NULL)
        }
        cmAnalysisList <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)
        cmAnalysisListFile <- system.file("settings",
                                          sprintf("cmAnalysisListInteractions%s.json", indicationId),
                                          package = "Legend")
        cmAnalysisList <- c(cmAnalysisList, CohortMethod::loadCmAnalysisList(cmAnalysisListFile))
        cmAnalysisListFile <- system.file("settings",
                                          sprintf("cmAnalysisListAsym%s.json", indicationId),
                                          package = "Legend")
        cmAnalysisList <- c(cmAnalysisList, CohortMethod::loadCmAnalysisList(cmAnalysisListFile))
        cmAnalysisToRow <- function(cmAnalysis) {
            ParallelLogger::saveSettingsToJson(cmAnalysis, tempFileName)
            row <- data.frame(analysisId = cmAnalysis$analysisId,
                              description = cmAnalysis$description,
                              definition = readChar(tempFileName, file.info(tempFileName)$size))
            return(row)
        }
        cohortMethodAnalysis <- lapply(cmAnalysisList, cmAnalysisToRow)
        cohortMethodAnalysis <- do.call("rbind", cohortMethodAnalysis)
        return(cohortMethodAnalysis)
    }
    cohortMethodAnalysis <- lapply(indications$indicationId, loadCmAnalyses)
    cohortMethodAnalysis <- do.call("rbind", cohortMethodAnalysis)
    unlink(tempFileName)
    colnames(cohortMethodAnalysis) <- SqlRender::camelCaseToSnakeCase(colnames(cohortMethodAnalysis))
    fileName <- file.path(exportFolder, "cohort_method_analysis.csv")
    write.csv(cohortMethodAnalysis, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- covariate_analysis table")
    indicationFolder <- file.path(outputFolder, indications$indicationId[1])
    covariateData <- FeatureExtraction::loadCovariateData(file.path(indicationFolder, "allCovariates"))
    covariateAnalysis <- ff::as.ram(covariateData$analysisRef)
    covariateAnalysis <- covariateAnalysis[, c("analysisId", "analysisName")]
    colnames(covariateAnalysis) <- c("covariate_analysis_id", "covariate_analysis_name")
    fileName <- file.path(exportFolder, "covariate_analysis.csv")
    write.csv(covariateAnalysis, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- incidence_analysis table")
    incidenceAnalysis <- data.frame(incidence_analysis_id = c("On-treatment", "Intent-to-treat"),
                                    description = c("On-treatment", "Intent-to-treat"))
    fileName <- file.path(exportFolder, "incidence_analysis.csv")
    write.csv(incidenceAnalysis, fileName, row.names = FALSE)
}

exportExposures <- function(outputFolder,
                            exportFolder,
                            databaseId) {
    ParallelLogger::logInfo("Exporting exposures")
    ParallelLogger::logInfo("- single_exposure_of_interest table")
    pathToCsv <- system.file("settings", "Indications.csv", package = "Legend")
    indications <- read.csv(pathToCsv)
    pathToCsv <- system.file("settings", "ExposuresOfInterest.csv", package = "Legend")
    exposuresOfInterest <- read.csv(pathToCsv)
    singleExposuresOfInterest <- exposuresOfInterest[, c("conceptId", "name" ,"indicationId")]
    singleExposuresOfInterest$definition <- ""
    singleExposuresOfInterest$filterConceptIds <- ""
    colnames(singleExposuresOfInterest) <- c("exposure_id", "exposure_name", "indication_id", "definition", "filter_concept_ids")
    fileName <- file.path(exportFolder, "single_exposure_of_interest.csv")
    write.csv(singleExposuresOfInterest, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- combi_exposure_of_interest table")
    loadCombiExposures <- function(indicationId) {
        pathToCsv <- file.path(outputFolder, indicationId, "exposureCombis.csv")
        if (file.exists(pathToCsv)) {
            combiExposures <- read.csv(pathToCsv, stringsAsFactors = FALSE)
            combiExposures$indicationId <- indicationId
            combiExposures <- combiExposures[, c("cohortDefinitionId", "cohortName", "exposureId1", "exposureId2", "indicationId")]
            colnames(combiExposures) <- c("exposure_id", "exposure_name", "single_exposure_id_1", "single_exposure_id_2", "indication_id")
            return(combiExposures)
        } else {
            return(NULL)
        }
    }
    combiExposures <- lapply(indications$indicationId, loadCombiExposures)
    combiExposures <- do.call("rbind", combiExposures)
    if (is.null(combiExposures) || nrow(combiExposures) == 0) {
        combiExposures <- data.frame(exposure_id = -1,
                                     exposure_name = "dummy",
                                     single_exposure_id_1 = -1,
                                     single_exposure_id_2 = -1,
                                     single_exposure_id_3 = -1,
                                     indication_id = "Dummy")
    }
    fileName <- file.path(exportFolder, "combi_exposure_of_interest.csv")
    write.csv(combiExposures, fileName, row.names = FALSE)
}

exportOutcomes <- function(outputFolder,
                           exportFolder,
                           databaseId) {
    ParallelLogger::logInfo("Exporting outcomes")
    ParallelLogger::logInfo("- outcome_of_interest table")
    pathToCsv <- system.file("settings", "Indications.csv", package = "Legend")
    indications <- read.csv(pathToCsv)
    pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Legend")
    outcomesOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)
    getDefinition <- function(name) {
        fileName <- system.file("cohorts", paste0(name, ".json"), package = "Legend")
        return(readChar(fileName, file.info(fileName)$size))
    }
    outcomesOfInterest$definition <- sapply(outcomesOfInterest$name, getDefinition)
    outcomesOfInterest <- outcomesOfInterest[, c("cohortId", "name", "definition", "indicationId")]
    colnames(outcomesOfInterest) <- c("outcome_id", "outcome_name", "definition", "indication_id")
    fileName <- file.path(exportFolder, "outcome_of_interest.csv")
    write.csv(outcomesOfInterest, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- negative_control_outcome table")
    pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Legend")
    negativeControls <- read.csv(pathToCsv)
    negativeControls <- negativeControls[, c("cohortId", "name", "conceptId", "indicationId")]
    colnames(negativeControls) <- c("outcome_id", "outcome_name", "concept_id", "indication_id")
    fileName <- file.path(exportFolder, "negative_control_outcome.csv")
    write.csv(negativeControls, fileName, row.names = FALSE)
    colnames(negativeControls) <- SqlRender::snakeCaseToCamelCase(colnames(negativeControls)) # Need this later

    ParallelLogger::logInfo("- positive_control_outcome table")
    loadPositiveControls <- function(indicationId) {
        pathToCsv <- file.path(outputFolder, indicationId, "signalInjectionSummary.csv")
        if (file.exists(pathToCsv)) {
            positiveControls <- read.csv(pathToCsv, stringsAsFactors = FALSE)
            positiveControls$indicationId <- indicationId
            positiveControls <- merge(positiveControls, negativeControls[, c("outcomeId", "outcomeName")])
            positiveControls$outcomeName <- paste0(positiveControls$outcomeName, ", RR = ", positiveControls$targetEffectSize)
            positiveControls <- positiveControls[, c("newOutcomeId", "outcomeName", "exposureId", "outcomeId", "targetEffectSize", "indicationId")]
            colnames(positiveControls) <- c("outcomeId", "outcomeName", "exposureId", "negativeControlId", "effectSize", "indication_id")
            return(positiveControls)
        } else {
            return(NULL)
        }
    }
    positiveControls <- lapply(indications$indicationId, loadPositiveControls)
    positiveControls <- do.call("rbind", positiveControls)
    colnames(positiveControls) <- SqlRender::camelCaseToSnakeCase(colnames(positiveControls))
    fileName <- file.path(exportFolder, "positive_control_outcome.csv")
    write.csv(positiveControls, fileName, row.names = FALSE)
}

exportMetadata <- function(outputFolder,
                           exportFolder,
                           databaseId,
                           databaseName,
                           minCellCount) {
    ParallelLogger::logInfo("Exporting metadata")
    ParallelLogger::logInfo("- database table")
    pathToCsv <- system.file("settings", "Indications.csv", package = "Legend")
    indications <- read.csv(pathToCsv)
    database <- data.frame(database_id = databaseId,
                           database_name = databaseName,
                           is_meta_analysis = 0)
    fileName <- file.path(exportFolder, "database.csv")
    write.csv(database, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- exposure_summary table")
    loadExposurePairs <- function(indicationId) {
        pathToCsv <- file.path(outputFolder, indicationId, "pairedExposureSummary.csv")
        if (file.exists(pathToCsv)) {
            return(read.csv(pathToCsv, stringsAsFactors = FALSE))
        } else {
            return(NULL)
        }
    }
    exposurePairs <- lapply(indications$indicationId, loadExposurePairs)
    exposurePairs <- do.call("rbind", exposurePairs)
    exposurePairs$databaseId <- databaseId
    exposureSummary1 <- exposurePairs[, c("databaseId", "targetId", "targetMinDate", "targetMaxDate")]
    colnames(exposureSummary1) <- c("databaseId", "exposureId", "minDate", "maxDate")
    exposureSummary2 <- exposurePairs[, c("databaseId", "comparatorId", "comparatorMinDate", "comparatorMaxDate")]
    colnames(exposureSummary2) <- c("databaseId", "exposureId", "minDate", "maxDate")
    exposureSummary <- unique(rbind(exposureSummary1, exposureSummary2))
    colnames(exposureSummary) <- SqlRender::camelCaseToSnakeCase(colnames(exposureSummary))
    fileName <- file.path(exportFolder, "exposure_summary.csv")
    write.csv(exposureSummary, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- comparison_summary table")
    exposurePairs <- exposurePairs[, c("databaseId", "targetId", "comparatorId", "pairedMinDate", "pairedMaxDate")]
    colnames(exposurePairs) <- c("databaseId", "targetId", "comparatorId", "minDate", "maxDate")
    exposurePairs <- rbind(exposurePairs,
                           swapColumnContents(exposurePairs, "targetId", "comparatorId"))
    colnames(exposurePairs) <- SqlRender::camelCaseToSnakeCase(colnames(exposurePairs))
    fileName <- file.path(exportFolder, "comparison_summary.csv")
    write.csv(exposurePairs, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- attrition table")
    pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Legend")
    outcomesOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)

    fileName <- file.path(exportFolder, "attrition.csv")
    if (file.exists(fileName)) {
        unlink(fileName)
    }
    loadAttrition <- function(indicationId) {
        ParallelLogger::logInfo("   compiling attrition table for ", indicationId)
        pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds")
        if (!file.exists(pathToRds)) {
            warning("Cannot find ", pathToRds)
            return(NULL)
        }
        outcomeModelReference1 <- readRDS(pathToRds)
        pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference2.rds")
        outcomeModelReference2 <- readRDS(pathToRds)
        outcomeModelReference <- rbind(outcomeModelReference1, outcomeModelReference2)
        outcomeModelReference$symmetrical <- TRUE
        pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference3.rds")
        outcomeModelReference3 <- readRDS(pathToRds)
        outcomeModelReference3$symmetrical <- FALSE
        outcomeModelReference <- rbind(outcomeModelReference, outcomeModelReference3)
        outcomeModelReference <- outcomeModelReference[outcomeModelReference$outcomeId %in% outcomesOfInterest$cohortId, ]
        first <- !file.exists(fileName)
        pb <- txtProgressBar(style = 3)
        for (i in 1:nrow(outcomeModelReference)) {
            outcomeModel <- readRDS(file.path(outputFolder, indicationId, "cmOutput", outcomeModelReference$outcomeModelFile[i]))
            attrition <- outcomeModel$attrition[, c("description", "targetPersons", "comparatorPersons")]
            attrition <- attrition[2:nrow(attrition), ] # First row is duplicate of last row from DB pull
            attrition$sequenceNumber <- 1:nrow(attrition) + 5
            attrition1 <- attrition[, c("sequenceNumber", "description", "targetPersons")]
            colnames(attrition1)[3] <- "subjects"
            attrition1$exposureId <- outcomeModelReference$targetId[i]
            attrition2 <- attrition[, c("sequenceNumber", "description", "comparatorPersons")]
            colnames(attrition2)[3] <- "subjects"
            attrition2$exposureId <- outcomeModelReference$comparatorId[i]
            attrition <- rbind(attrition1, attrition2)
            attrition$targetId <- outcomeModelReference$targetId[i]
            attrition$comparatorId <- outcomeModelReference$comparatorId[i]
            if (outcomeModelReference$symmetrical[i]) {
                attrition <- rbind(attrition, Legend:::swapColumnContents(attrition, "targetId", "comparatorId"))
            }
            attrition$analysisId <- outcomeModelReference$analysisId[i]
            attrition$outcomeId <-  outcomeModelReference$outcomeId[i]
            attrition$databaseId <- databaseId
            attrition <- attrition[, c("databaseId", "exposureId", "targetId", "comparatorId", "outcomeId", "analysisId", "sequenceNumber", "description", "subjects")]
            attrition <- Legend:::enforceMinCellValue(attrition, "subjects", minCellCount, silent = TRUE)

            colnames(attrition) <- SqlRender::camelCaseToSnakeCase(colnames(attrition))
            write.table(x = attrition,
                        file = fileName,
                        row.names = FALSE,
                        col.names = first,
                        sep = ",",
                        dec = ".",
                        qmethod = "double",
                        append = !first)
            first <- FALSE
            if (i %% 1000 == 10) {
                setTxtProgressBar(pb, i/nrow(outcomeModelReference))
            }
        }
        setTxtProgressBar(pb, 1)
        close(pb)

        pathToCsv <- file.path(outputFolder, indicationId, "attrition.csv")
        attritionFromDb <- read.csv(pathToCsv, stringsAsFactors = FALSE)
        attritionFromDb$targetId[attritionFromDb$targetId == -1] <- NA
        attritionFromDb$comparatorId[attritionFromDb$comparatorId == -1] <- NA
        attritionFromDbTc <- attritionFromDb[attritionFromDb$targetId != -1, ]
        attritionFromDb <- rbind(attritionFromDb, Legend:::swapColumnContents(attritionFromDbTc, "targetId", "comparatorId"))
        attritionFromDb$analysisId <- -1
        attritionFromDb$outcomeId <- -1
        attritionFromDb$databaseId <- databaseId
        attritionFromDb <- attritionFromDb[, c("databaseId", "exposureId", "targetId", "comparatorId", "outcomeId", "analysisId", "sequenceNumber", "description", "subjects")]
        colnames(attritionFromDb) <- SqlRender::camelCaseToSnakeCase(colnames(attritionFromDb))
        write.table(x = attritionFromDb,
                    file = fileName,
                    row.names = FALSE,
                    col.names = first,
                    sep = ",",
                    dec = ".",
                    qmethod = "double",
                    append = !first)
        return(NULL)
    }
    lapply(indications$indicationId, loadAttrition)

    ParallelLogger::logInfo("- covariate table")
    loadCovariateNames <- function(indicationId) {
        covariateFolder <- file.path(outputFolder, indicationId, "allCovariates")
        if (!file.exists(covariateFolder)) {
            warning("Cannot find ", covariateFolder)
            return(NULL)
        }
        covariateData <- FeatureExtraction::loadCovariateData(covariateFolder)
        covariateNames <- ff::as.ram(covariateData$covariateRef[, c("covariateId", "covariateName", "analysisId")])
        return(covariateNames)
    }
    covariateNames <- lapply(indications$indicationId, loadCovariateNames)
    covariateNames <- do.call("rbind", covariateNames)
    covariateNames <- unique(covariateNames)
    covariateNames$databaseId <- databaseId
    colnames(covariateNames)[colnames(covariateNames) == "analysisId"] <- "covariateAnalysisId"
    colnames(covariateNames) <- SqlRender::camelCaseToSnakeCase(colnames(covariateNames))
    fileName <- file.path(exportFolder, "covariate.csv")
    write.csv(covariateNames, fileName, row.names = FALSE)
    rm(covariateNames) # Free up memory

    ParallelLogger::logInfo("- cm_follow_up_dist table")
    pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Legend")
    outcomesOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)

    loadCmFollowUpDist <- function(indicationId) {
        ParallelLogger::logInfo("   fetching cohort method follow up distributions for ", indicationId)
        pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds")
        if (!file.exists(pathToRds)) {
            warning("Cannot find ", pathToRds)
            return(NULL)
        }
        outcomeModelReference1 <- readRDS(pathToRds)
        pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference2.rds")
        outcomeModelReference2 <- readRDS(pathToRds)
        outcomeModelReference <- rbind(outcomeModelReference1, outcomeModelReference2)
        outcomeModelReference$symmetrical <- TRUE
        pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference3.rds")
        outcomeModelReference3 <- readRDS(pathToRds)
        outcomeModelReference3$symmetrical <- FALSE
        outcomeModelReference <- rbind(outcomeModelReference, outcomeModelReference3)
        outcomeModelReference <- outcomeModelReference[outcomeModelReference$outcomeId %in% outcomesOfInterest$cohortId, ]
        getResult <- function(i) {
            strataPop <- readRDS(file.path(outputFolder, indicationId, "cmOutput", outcomeModelReference$strataFile[i]))
            targetDist <- quantile(strataPop$survivalTime[strataPop$treatment == 1], c(0, 0.1, 0.25, 0.5, 0.85, 0.9, 1))
            comparatorDist <- quantile(strataPop$survivalTime[strataPop$treatment == 1], c(0, 0.1, 0.25, 0.5, 0.85, 0.9, 1))
            row <- data.frame(targetId = outcomeModelReference$targetId[i],
                              comparatorId = outcomeModelReference$comparatorId[i],
                              outcomeId = outcomeModelReference$outcomeId[i],
                              analysisId = outcomeModelReference$analysisId[i],
                              target_min = targetDist[1],
                              target_p10 = targetDist[2],
                              target_p25 = targetDist[3],
                              target_median = targetDist[4],
                              target_p75 = targetDist[5],
                              target_p90 = targetDist[6],
                              target_max = targetDist[7],
                              comparator_min = comparatorDist[1],
                              comparator_p10 = comparatorDist[2],
                              comparator_p25 = comparatorDist[3],
                              comparator_median = comparatorDist[4],
                              comparator_p75 = comparatorDist[5],
                              comparator_p90 = comparatorDist[6],
                              comparator_max = comparatorDist[7])
            return(row)
        }
        results <- plyr::llply(1:nrow(outcomeModelReference), getResult, .progress = "text")
        results <- do.call("rbind", results)
        return(results)
    }
    results <- lapply(indications$indicationId, loadCmFollowUpDist)
    results <- do.call("rbind", results)
    results$databaseId <- databaseId
    colnames(results) <- SqlRender::camelCaseToSnakeCase(colnames(results))
    fileName <- file.path(exportFolder, "cm_follow_up_dist.csv")
    write.csv(results, fileName, row.names = FALSE)
    rm(results) # Free up memory
}

enforceMinCellValue <- function(data, fieldName, minValues, silent = FALSE) {
    toCensor <- !is.na(data[, fieldName]) & data[, fieldName] < minValues & data[, fieldName] != 0
    if (!silent) {
        percent <- round(100 * sum(toCensor) / nrow(data), 1)
        ParallelLogger::logInfo("   censoring ",
                                sum(toCensor),
                                " values (",
                                percent,
                                "%) from ",
                                fieldName,
                                " because value below minimum")
    }
    if (length(minValues) == 1) {
        data[toCensor, fieldName] <- -minValues
    } else {
        data[toCensor, fieldName] <- -minValues[toCensor]
    }
    return(data)
}


exportMainResults <- function(outputFolder,
                              exportFolder,
                              databaseId,
                              minCellCount,
                              maxCores) {
    ParallelLogger::logInfo("Exporting main results")
    ParallelLogger::logInfo("- cohort_method_result table")
    pathToCsv <- system.file("settings", "Indications.csv", package = "Legend")
    indications <- read.csv(pathToCsv)
    positiveControls <- read.csv(file.path(exportFolder, "positive_control_outcome.csv"))
    colnames(positiveControls) <- SqlRender::snakeCaseToCamelCase(colnames(positiveControls))
    negativeControls <- read.csv(file.path(exportFolder, "negative_control_outcome.csv"))
    colnames(negativeControls) <- SqlRender::snakeCaseToCamelCase(colnames(negativeControls))

    loadCmResults <- function(indicationId) {
        summaryFile <- file.path(outputFolder, indicationId, "analysisSummary.csv")
        if (!file.exists(summaryFile)) {
            warning("Cannot find ", summaryFile)
            return(NULL)
        }
        analysesSum <- read.csv(summaryFile)
        analysesSum2 <- read.csv(file.path(outputFolder, indicationId, "analysisSummaryInteractions.csv"))
        analysesSum <- rbind(analysesSum, analysesSum2[, colnames(analysesSum)])
        analysesSum$symmetrical <- TRUE
        analysesSum3 <- read.csv(file.path(outputFolder, indicationId, "analysisSummaryAsym.csv"))
        analysesSum3$symmetrical <- FALSE
        analysesSum <- rbind(analysesSum, analysesSum3[, colnames(analysesSum)])
        return(analysesSum)
    }
    cmResults <- lapply(indications$indicationId, loadCmResults)
    cmResults <- do.call("rbind", cmResults)

    ParallelLogger::logInfo("  Performing empirical calibration on main effects")
    cluster <- ParallelLogger::makeCluster(min(6, maxCores))
    subsets <- split(cmResults, paste(cmResults$targetId, cmResults$comparatorId, cmResults$analysisId))
    rm(cmResults) # Free up memory
    results <- ParallelLogger::clusterApply(cluster,
                                            subsets,
                                            Legend:::calibrate,
                                            negativeControls = negativeControls,
                                            positiveControls = positiveControls)
    ParallelLogger::stopCluster(cluster)
    rm(subsets) # Free up memory
    results <- do.call("rbind", results)
    results$databaseId <- databaseId
    results <- enforceMinCellValue(results, "targetSubjects", minCellCount)
    results <- enforceMinCellValue(results, "comparatorSubjects", minCellCount)
    results <- enforceMinCellValue(results, "targetOutcomes", minCellCount)
    results <- enforceMinCellValue(results, "comparatorOutcomes", minCellCount)
    colnames(results) <- SqlRender::camelCaseToSnakeCase(colnames(results))
    fileName <- file.path(exportFolder, "cohort_method_result.csv")
    write.csv(results, fileName, row.names = FALSE)
    rm(results) # Free up memory

    ParallelLogger::logInfo("- cm_interaction_result table")
    loadInteractionEffects <- function(indicationId) {
        ParallelLogger::logInfo("   compiling interaction results for ", indicationId)
        pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference3.rds")
        outcomeModelReference <- readRDS(pathToRds)
        loadInteractionsFromOutcomeModel <- function(i) {
            outcomeModel <- readRDS(file.path(outputFolder, indicationId, "cmOutput", outcomeModelReference$outcomeModelFile[i]))
            if (!is.null(outcomeModel$subgroupCounts)) {
                rows <- data.frame(targetId = outcomeModelReference$targetId[i],
                                   comparatorId = outcomeModelReference$comparatorId[i],
                                   outcomeId = outcomeModelReference$outcomeId[i],
                                   analysisId = outcomeModelReference$analysisId[i],
                                   interactionCovariateId = outcomeModel$subgroupCounts$subgroupCovariateId,
                                   rrr = NA,
                                   ci95Lb = NA,
                                   ci95Ub = NA,
                                   p = NA,
                                   i2 = NA,
                                   logRrr = NA,
                                   seLogRrr = NA,
                                   targetSubjects = outcomeModel$subgroupCounts$targetPersons,
                                   comparatorSubjects = outcomeModel$subgroupCounts$comparatorPersons,
                                   targetDays = outcomeModel$subgroupCounts$targetDays,
                                   comparatorDays = outcomeModel$subgroupCounts$comparatorDays,
                                   targetOutcomes = outcomeModel$subgroupCounts$targetOutcomes,
                                   comparatorOutcomes = outcomeModel$subgroupCounts$comparatorOutcomes)
                if (!is.null(outcomeModel$outcomeModelInteractionEstimates)) {
                    idx <- match(outcomeModel$outcomeModelInteractionEstimates$covariateId,
                                 rows$interactionCovariateId)
                    rows$rrr[idx] <- exp(outcomeModel$outcomeModelInteractionEstimates$logRr)
                    rows$ci95Lb[idx] <- exp(outcomeModel$outcomeModelInteractionEstimates$logLb95)
                    rows$ci95Ub[idx] <- exp(outcomeModel$outcomeModelInteractionEstimates$logUb95)
                    rows$logRrr[idx] <- outcomeModel$outcomeModelInteractionEstimates$logRr
                    rows$seLogRrr[idx] <- outcomeModel$outcomeModelInteractionEstimates$seLogRr
                    z <- rows$logRrr[idx]/rows$seLogRrr[idx]
                    rows$p[idx] <- 2 * pmin(pnorm(z), 1 - pnorm(z))
                }
                return(rows)
            } else {
                return(NULL)
            }

        }

        interactions <- plyr::llply(1:nrow(outcomeModelReference), loadInteractionsFromOutcomeModel, .progress = "text")
        interactions <- do.call("rbind", interactions)
        return(interactions)
    }
    interactions <- lapply(indications$indicationId, loadInteractionEffects)
    interactions <- do.call("rbind", interactions)

    ParallelLogger::logInfo("  Performing empirical calibration on interaction effects")
    cluster <- ParallelLogger::makeCluster(min(6, maxCores))
    subsets <- split(interactions, paste(interactions$targetId, interactions$comparatorId, interactions$analysisId))
    interactions <- ParallelLogger::clusterApply(cluster,
                                                 subsets,
                                                 Legend:::calibrateInteractions,
                                                 negativeControls = negativeControls)
    ParallelLogger::stopCluster(cluster)
    rm(subsets) # Free up memory
    interactions <- do.call("rbind", interactions)

    # Add TC -> CT swap
    interactionsCt <- Legend:::swapColumnContents(interactions, "targetId", "comparatorId")
    interactionsCt$rrr <- 1/interactionsCt$rrr
    interactionsCt$logRrr <- -interactionsCt$logRrr
    temp <- 1/interactionsCt$ci95Lb
    interactionsCt$ci95Lb <- 1/interactionsCt$ci95Ub
    interactionsCt$ci95Ub <- temp
    interactions <- rbind(interactionsCt)
    interactions$databaseId <- databaseId

    interactions <- enforceMinCellValue(interactions, "targetSubjects", minCellCount)
    interactions <- enforceMinCellValue(interactions, "comparatorSubjects", minCellCount)
    interactions <- enforceMinCellValue(interactions, "targetOutcomes", minCellCount)
    interactions <- enforceMinCellValue(interactions, "comparatorOutcomes", minCellCount)
    colnames(interactions) <- SqlRender::camelCaseToSnakeCase(colnames(interactions))
    fileName <- file.path(exportFolder, "cm_interaction_result.csv")
    write.csv(interactions, fileName, row.names = FALSE)
    rm(interactions) # Free up memory

    ParallelLogger::logInfo("- incidence table")
    loadIncidence <- function(indicationId) {
        pathToCsv <- file.path(outputFolder, indicationId, "incidence.csv")
        if (!file.exists(pathToCsv)) {
            warning("Cannot find ", pathToCsv)
            return(NULL)
        }
        incidence <- read.csv(pathToCsv)
        return(incidence)
    }

    incidence <- lapply(indications$indicationId, loadIncidence)
    incidence <- do.call("rbind", incidence)
    incidence$databaseId <- databaseId
    incidence <- enforceMinCellValue(incidence, "subjects", minCellCount)
    incidence <- enforceMinCellValue(incidence, "outcomes", minCellCount)
    colnames(incidence) <- SqlRender::camelCaseToSnakeCase(colnames(incidence))
    fileName <- file.path(exportFolder, "incidence.csv")
    write.csv(incidence, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- chronograph table")
    loadChronograph <- function(indicationId) {
        pathToCsv <- file.path(outputFolder, indicationId, "chronographData.csv")
        if (!file.exists(pathToCsv)) {
            warning("Cannot find ", pathToCsv)
            return(NULL)
        }
        chronograph <- read.csv(pathToCsv)
    }

    chronograph <- lapply(indications$indicationId, loadChronograph)
    chronograph <- do.call("rbind", chronograph)
    chronograph$databaseId <- databaseId
    chronograph <- chronograph[, c("databaseId", "exposureId", "outcomeId", "periodId", "outcomeCount", "expectedCount", "ic", "icLow", "icHigh")]
    colnames(chronograph) <- c("databaseId", "exposureId", "outcomeId", "time", "outcomes", "expectedOutcomes", "ic", "icLb", "icUb")
    # IC metric depends on number of observed outcomes, so consider together for minCellCount:
    toCensor <- chronograph$outcomes < minCellCount & chronograph$outcomes != 0
    percent <- round(100 * sum(toCensor) / nrow(chronograph), 1)
    chronograph$outcomes[toCensor] <- minCellCount
    chronograph$ic[toCensor] <- NA
    chronograph$icLb[toCensor] <- NA
    chronograph$icUb[toCensor] <- NA
    ParallelLogger::logInfo("   censoring ",
                            sum(toCensor),
                            " values (",
                            percent,
                            "%) from outcomes, ic, icLb, icUb because value below minimum")

    colnames(chronograph) <- SqlRender::camelCaseToSnakeCase(colnames(chronograph))
    fileName <- file.path(exportFolder, "chronograph.csv")
    write.csv(chronograph, fileName, row.names = FALSE)
}

calibrate <- function(subset, negativeControls, positiveControls) {
    ncs <- subset[subset$outcomeId %in% negativeControls$outcomeId, ]
    ncs <- ncs[!is.na(ncs$seLogRr), ]
    if (nrow(ncs) > 5) {
        null <- EmpiricalCalibration::fitMcmcNull(ncs$logRr, ncs$seLogRr)
        calibratedP <- EmpiricalCalibration::calibrateP(null = null,
                                                        logRr = subset$logRr,
                                                        seLogRr = subset$seLogRr)
        subset$calibratedP <- calibratedP$p
    } else {
        subset$calibratedP <- rep(NA, nrow(subset))
    }
    targetPcs <- merge(subset, data.frame(targetId = positiveControls$exposureId,
                                          outcomeId = positiveControls$outcomeId,
                                          effectSize = positiveControls$effectSize))

    comparatorPcs <- merge(subset, data.frame(comparatorId = positiveControls$exposureId,
                                              outcomeId = positiveControls$outcomeId,
                                              effectSize = positiveControls$effectSize))
    subsetTc <- subset[!(subset$outcomeId %in% comparatorPcs$outcomeId), ]
    subsetCt <- subset[!(subset$outcomeId %in% targetPcs$outcomeId), ]
    subsetCt <- swapColumnContents(subsetCt, "targetId", "comparatorId")
    subsetCt$rr <- 1/subsetCt$rr
    temp <- 1/subsetCt$ci95lb
    subsetCt$ci95lb <- 1/subsetCt$ci95ub
    subsetCt$ci95ub <- temp
    subsetCt$logRr <- -subsetCt$logRr
    targetPcs <- targetPcs[!is.na(targetPcs$seLogRr), ]
    if (nrow(targetPcs) > 5) {
        model <- EmpiricalCalibration::fitSystematicErrorModel(logRr = c(ncs$logRr, targetPcs$logRr),
                                                               seLogRr = c(ncs$seLogRr, targetPcs$seLogRr),
                                                               trueLogRr = c(rep(0, nrow(ncs)), log(targetPcs$effectSize)),
                                                               estimateCovarianceMatrix = FALSE)
        calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = subsetTc$logRr,
                                                                          seLogRr = subsetTc$seLogRr,
                                                                          model = model)
        subsetTc$calibratedRr <- exp(calibratedCi$logRr)
        subsetTc$calibratedCi95Lb <- exp(calibratedCi$logLb95Rr)
        subsetTc$calibratedCi95Ub <- exp(calibratedCi$logUb95Rr)
        subsetTc$calibratedLogRr <- calibratedCi$logRr
        subsetTc$calibratedSeLogRr <- calibratedCi$seLogRr
    } else {
        subsetTc$calibratedRr <- rep(NA, nrow(subsetTc))
        subsetTc$calibratedCi95Lb <- rep(NA, nrow(subsetTc))
        subsetTc$calibratedCi95Ub <- rep(NA, nrow(subsetTc))
        subsetTc$calibratedLogRr <- rep(NA, nrow(subsetTc))
        subsetTc$calibratedSeLogRr <- rep(NA, nrow(subsetTc))
    }
    comparatorPcs <- comparatorPcs[!is.na(comparatorPcs$seLogRr), ]
    if (nrow(comparatorPcs) > 5) {
        model <- EmpiricalCalibration::fitSystematicErrorModel(logRr = c(-ncs$logRr, -comparatorPcs$logRr),
                                                               seLogRr = c(ncs$seLogRr, comparatorPcs$seLogRr),
                                                               trueLogRr = c(rep(0, nrow(ncs)), log(comparatorPcs$effectSize)),
                                                               estimateCovarianceMatrix = FALSE)
        calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = subsetCt$logRr,
                                                                          seLogRr = subsetCt$seLogRr,
                                                                          model = model)
        subsetCt$calibratedRr <- exp(calibratedCi$logRr)
        subsetCt$calibratedCi95Lb <- exp(calibratedCi$logLb95Rr)
        subsetCt$calibratedCi95Ub <- exp(calibratedCi$logUb95Rr)
        subsetCt$calibratedLogRr <- calibratedCi$logRr
        subsetCt$calibratedSeLogRr <- calibratedCi$seLogRr
    } else {
        subsetCt$calibratedRr <- rep(NA, nrow(subsetCt))
        subsetCt$calibratedCi95Lb <- rep(NA, nrow(subsetCt))
        subsetCt$calibratedCi95Ub <- rep(NA, nrow(subsetCt))
        subsetCt$calibratedLogRr <- rep(NA, nrow(subsetCt))
        subsetCt$calibratedSeLogRr <- rep(NA, nrow(subsetCt))
    }
    subset <- rbind(subsetTc, subsetCt)
    subset$i2 <- rep(NA, nrow(subset))
    subset <- subset[, c("targetId", "comparatorId", "outcomeId", "analysisId", "rr", "ci95lb",
                         "ci95ub", "p", "i2", "logRr", "seLogRr", "target", "comparator", "targetDays", "comparatorDays",
                         "eventsTarget", "eventsComparator", "calibratedP", "calibratedRr", "calibratedCi95Lb", "calibratedCi95Ub",
                         "calibratedLogRr", "calibratedSeLogRr")]
    colnames(subset) <- c("targetId", "comparatorId", "outcomeId", "analysisId", "rr", "ci95lb",
                          "ci95ub", "p", "i2", "logRr", "seLogRr", "targetSubjects", "comparatorSubjects", "targetDays", "comparatorDays",
                          "targetOutcomes", "comparatorOutcomes", "calibratedP", "calibratedRr", "calibratedCi95Lb", "calibratedCi95Ub",
                          "calibratedLogRr", "calibratedSeLogRr")
    return(subset)
}

calibrateInteractions <- function(subset, negativeControls) {
    ncs <- subset[subset$outcomeId %in% negativeControls$outcomeId, ]
    ncs <- ncs[!is.na(ncs$seLogRr), ]
    if (nrow(ncs) > 5) {
        null <- EmpiricalCalibration::fitMcmcNull(ncs$logRr, ncs$seLogRr)
        calibratedP <- EmpiricalCalibration::calibrateP(null = null,
                                                        logRr = subset$logRr,
                                                        seLogRr = subset$seLogRr)
        subset$calibratedP <- calibratedP$p
    } else {
        subset$calibratedP <- rep(NA, nrow(subset))
    }
    return(subset)
}


exportDiagnostics <- function(outputFolder,
                              exportFolder,
                              databaseId,
                              minCellCount,
                              maxCores) {
    ParallelLogger::logInfo("Exporting diagnostics")
    pathToCsv <- system.file("settings", "Indications.csv", package = "Legend")
    indications <- read.csv(pathToCsv)

    ParallelLogger::logInfo("- covariate_balance table")
    fileName <- file.path(exportFolder, "covariate_balance.csv")
    if (file.exists(fileName)) {
        unlink(fileName)
    }
    loadAndSaveBalance <- function(indicationId) {
        ParallelLogger::logInfo("   compiling covariate balance statistics for ", indicationId)
        first <- !file.exists(fileName)
        files <- list.files(file.path(outputFolder, indicationId, "balance"), pattern = "Bal_.*.rds", full.names = TRUE)
        pb <- txtProgressBar(style = 3)
        for (i in 1:length(files)) {
            ids <- gsub("^.*Bal_t", "", files[i])
            targetId <- as.numeric(gsub("_c.*", "", ids))
            ids <- gsub("^.*_c", "", ids)
            comparatorId <- as.numeric(gsub("(_s.*$)|(\\.rds$)", "", ids))
            if (grepl("_s", ids)) {
                subgroupId <- as.numeric(gsub("^.*_s", "", gsub("\\.rds", "", ids)))
            } else {
                subgroupId <- NA
            }
            balance <- readRDS(files[i])

            inferredTargetBeforeSize <- mean(balance$beforeMatchingSumTarget/balance$beforeMatchingMeanTarget, na.rm = TRUE)
            inferredComparatorBeforeSize <- mean(balance$beforeMatchingSumComparator/balance$beforeMatchingMeanComparator, na.rm = TRUE)
            inferredTargetAfterSize <- mean(balance$afterMatchingSumTarget/balance$afterMatchingMeanTarget, na.rm = TRUE)
            inferredComparatorAfterSize <- mean(balance$afterMatchingSumComparator/balance$afterMatchingMeanComparator, na.rm = TRUE)

            balance$databaseId <- databaseId
            balance$targetId <- targetId
            balance$comparatorId <- comparatorId
            balance$interactionCovariateId <- subgroupId
            balance <- balance[, c("databaseId", "targetId", "comparatorId", "interactionCovariateId", "covariateId",
                                   "beforeMatchingMeanTarget", "beforeMatchingMeanComparator", "beforeMatchingStdDiff",
                                   "afterMatchingMeanTarget", "afterMatchingMeanComparator", "afterMatchingStdDiff")]
            colnames(balance) <- c("databaseId", "targetId", "comparatorId", "interactionCovariateId", "covariateId",
                                   "targetMeanBefore", "comparatorMeanBefore", "stdDiffBefore",
                                   "targetMeanAfter", "comparatorMeanAfter", "stdDiffAfter")
            balance$targetMeanBefore[is.na(balance$targetMeanBefore)] <- 0
            balance$comparatorMeanBefore[is.na(balance$comparatorMeanBefore)] <- 0
            balance$stdDiffBefore <- round(balance$stdDiffBefore, 3)
            balance$targetMeanAfter[is.na(balance$targetMeanAfter)] <- 0
            balance$comparatorMeanAfter[is.na(balance$comparatorMeanAfter)] <- 0
            balance$stdDiffAfter <- round(balance$stdDiffAfter, 3)

            balanceCt <- swapColumnContents(balance, "targetId", "comparatorId")
            balanceCt <- swapColumnContents(balanceCt, "targetMeanBefore", "comparatorMeanBefore")
            balanceCt$stdDiffBefore <- -balanceCt$stdDiffBefore
            balanceCt <- swapColumnContents(balanceCt, "targetMeanAfter", "comparatorMeanAfter")
            balanceCt$stdDiffAfter <- -balanceCt$stdDiffAfter
            balance <- rbind(balance, balanceCt)

            balance <- enforceMinCellValue(balance, "targetMeanBefore",  minCellCount / inferredTargetBeforeSize, TRUE)
            balance <- enforceMinCellValue(balance, "comparatorMeanBefore",  minCellCount / inferredComparatorBeforeSize, TRUE)
            balance <- enforceMinCellValue(balance, "targetMeanAfter",  minCellCount / inferredTargetAfterSize, TRUE)
            balance <- enforceMinCellValue(balance, "comparatorMeanAfter",  minCellCount / inferredComparatorAfterSize, TRUE)

            balance$targetMeanBefore <- round(balance$targetMeanBefore, 3)
            balance$comparatorMeanBefore <- round(balance$comparatorMeanBefore, 3)
            balance$targetMeanAfter <- round(balance$targetMeanAfter, 3)
            balance$comparatorMeanAfter <- round(balance$comparatorMeanAfter, 3)


            balance <- balance[balance$targetMeanBefore != 0 &
                                   balance$comparatorMeanBefore != 0 &
                                   balance$targetMeanAfter != 0 &
                                   balance$comparatorMeanAfter != 0 &
                                   balance$stdDiffBefore != 0 &
                                   balance$stdDiffAfter != 0, ]
            colnames(balance) <- SqlRender::camelCaseToSnakeCase(colnames(balance))
            write.table(x = balance,
                        file = fileName,
                        row.names = FALSE,
                        col.names = first,
                        sep = ",",
                        dec = ".",
                        qmethod = "double",
                        append = !first)
            first <- FALSE
            setTxtProgressBar(pb, i/length(files))
        }
        close(pb)
        return(NULL)
    }
    lapply(indications$indicationId, loadAndSaveBalance)

    ParallelLogger::logInfo("- preference_score_dist table")
    preparePlots <- function(indicationId) {
        if (!file.exists(file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds"))) {
            warning("Cannot find ", file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds"))
            return(NULL)
        }
        ParallelLogger::logInfo("   compiling preference score distributions for ", indicationId)

        preparePlot <- function(i, outcomeModelReference) {
            psFileName <- file.path(outputFolder, indicationId, "cmOutput", outcomeModelReference$sharedPsFile[i])
            if (file.exists(psFileName)) {
                ps <- readRDS(psFileName)
                if (min(ps$propensityScore) < max(ps$propensityScore)) {
                    ps <- CohortMethod:::computePreferenceScore(ps)

                    d1 <- density(ps$preferenceScore[ps$treatment == 1], from = 0, to = 1, n = 100)
                    d0 <- density(ps$preferenceScore[ps$treatment == 0], from = 0, to = 1, n = 100)

                    result <- data.frame(databaseId = databaseId,
                                         targetId = outcomeModelReference$targetId[i],
                                         comparatorId = outcomeModelReference$comparatorId[i],
                                         preferenceScore = d1$x,
                                         targetDensity = d1$y,
                                         comparatorDensity = d0$y)
                    result <- rbind(result,
                                    swapColumnContents(swapColumnContents(result, "targetId", "comparatorId"),
                                                       "targetDensity",
                                                       "comparatorDensity"))

                    return(result)
                }
            }
            return(NULL)
        }
        outcomeModelReference <- readRDS(file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds"))
        outcomeModelReference <- outcomeModelReference[order(outcomeModelReference$sharedPsFile), ]
        outcomeModelReference <- outcomeModelReference[!duplicated(outcomeModelReference$sharedPsFile), ]
        data <- plyr::llply(1:nrow(outcomeModelReference), preparePlot, outcomeModelReference = outcomeModelReference, .progress = "text")
        data <- do.call("rbind", data)
        return(data)
    }
    data <- lapply(indications$indicationId, preparePlots)
    data <- do.call("rbind", data)
    fileName <- file.path(exportFolder, "preference_score_dist.csv")
    colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
    write.csv(data, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- propensity_model table")
    getPsModels <- function(indicationId) {
        ParallelLogger::logInfo("   compiling propensity models for ", indicationId)
        if (!file.exists(file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds"))) {
            warning("Cannot find ", file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds"))
            return(NULL)
        }

        getPsModel <- function(i, outcomeModelReference) {
            psFileName <- file.path(outputFolder, indicationId, "cmOutput", outcomeModelReference$sharedPsFile[i])
            if (file.exists(psFileName)) {
                ps <- readRDS(psFileName)
                metaData <- attr(ps, "metaData")
                if (is.null(metaData$psError)) {
                    cmDataFile <- file.path(outputFolder, indicationId, "cmOutput", outcomeModelReference$cohortMethodDataFolder[i])
                    cmData <- CohortMethod::loadCohortMethodData(cmDataFile)
                    model <- CohortMethod::getPsModel(ps, cmData)
                    model$covariateId[is.na(model$covariateId)] <- 0
                    ff::close.ffdf(cmData$covariates)
                    ff::close.ffdf(cmData$covariateRef)
                    ff::close.ffdf(cmData$analysisRef)
                    model$databaseId <- databaseId
                    model$targetId <- outcomeModelReference$targetId[i]
                    model$comparatorId <- outcomeModelReference$comparatorId[i]
                    model <- model[, c("databaseId", "targetId", "comparatorId", "covariateId", "coefficient")]
                    modelCt <- swapColumnContents(model, "targetId", "comparatorId")
                    modelCt$coefficient <- -modelCt$coefficient
                    model <- rbind(model, modelCt)
                    return(model)
                }
            }
            return(NULL)
        }
        outcomeModelReference <- readRDS(file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds"))
        outcomeModelReference <- outcomeModelReference[order(outcomeModelReference$sharedPsFile), ]
        outcomeModelReference <- outcomeModelReference[!duplicated(outcomeModelReference$sharedPsFile), ]
        data <- plyr::llply(1:nrow(outcomeModelReference), getPsModel, outcomeModelReference = outcomeModelReference, .progress = "text")
        data <- do.call("rbind", data)
        return(data)
    }
    data <- lapply(indications$indicationId, getPsModels)
    data <- do.call("rbind", data)
    fileName <- file.path(exportFolder, "propensity_model.csv")
    colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
    write.csv(data, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- kaplan_meier_dist table")
    prepareKms <- function(indicationId) {
        ParallelLogger::logInfo("   preparing Kaplan-Meier plots for ", indicationId)
        if (!file.exists(file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds"))) {
            warning("Cannot find ", file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds"))
            return(NULL)
        }

        pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds")
        outcomeModelReference1 <- readRDS(pathToRds)
        pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference2.rds")
        outcomeModelReference2 <- readRDS(pathToRds)
        pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference3.rds")
        outcomeModelReference3 <- readRDS(pathToRds)
        outcomeModelReference <- rbind(outcomeModelReference1, outcomeModelReference2, outcomeModelReference3)
        outcomeModelReference <- outcomeModelReference[outcomeModelReference$strataFile != "", ] # HOIs only
        outcomeModelReference <- outcomeModelReference[, c("strataFile", "targetId", "comparatorId", "outcomeId", "analysisId")]
        cluster <- ParallelLogger::makeCluster(min(6, maxCores))
        data <- ParallelLogger::clusterApply(cluster,
                                             1:nrow(outcomeModelReference),
                                             Legend:::prepareKm,
                                             outcomeModelReference = outcomeModelReference,
                                             outputFolder = outputFolder,
                                             indicationId = indicationId)
        ParallelLogger::stopCluster(cluster)
        data <- do.call("rbind", data)
        data$databaseId <- databaseId
        return(data)
    }
    data <- lapply(indications$indicationId, prepareKms)
    data <- do.call("rbind", data)
    data <- enforceMinCellValue(data, "targetAtRisk", minCellCount)
    data <- enforceMinCellValue(data, "comparatorAtRisk", minCellCount)
    colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
    fileName <- file.path(exportFolder, "kaplan_meier_dist.csv")
    write.csv(data, fileName, row.names = FALSE)
    rm(data) # Free up memory
}

prepareKm <- function(i, outcomeModelReference, outputFolder, indicationId) {
    population <- readRDS(file.path(outputFolder, indicationId, "cmOutput", outcomeModelReference$strataFile[i]))
    if (nrow(population) == 0) {
        # Can happen when matching and treatment is predictable
        return(NULL)
    }
    dataTc <- prepareKaplanMeier(population)
    dataTc$targetId <- outcomeModelReference$targetId[i]
    dataTc$comparatorId <- outcomeModelReference$comparatorId[i]
    dataTc$outcomeId <- outcomeModelReference$outcomeId[i]
    dataTc$analysisId <- outcomeModelReference$analysisId[i]

    population$treatment <- 1 - population$treatment
    dataCt <- prepareKaplanMeier(population)
    dataCt$targetId <- outcomeModelReference$comparatorId[i]
    dataCt$comparatorId <- outcomeModelReference$targetId[i]
    dataCt$outcomeId <- outcomeModelReference$outcomeId[i]
    dataCt$analysisId <- outcomeModelReference$analysisId[i]

    data <- rbind(dataTc, dataCt)
    return(data)
}

prepareKaplanMeier <- function(population) {
    dataCutoff <- 0.90
    population$y <- 0
    population$y[population$outcomeCount != 0] <- 1
    population$stratumSizeT <- 1
    strataSizesT <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 1,], sum)
    strataSizesC <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 0,], sum)
    colnames(strataSizesC)[2] <- "stratumSizeC"
    weights <- merge(strataSizesT, strataSizesC)
    weights$weight <- weights$stratumSizeT / weights$stratumSizeC
    population <- merge(population, weights[, c("stratumId", "weight")])
    population$weight[population$treatment == 1] <- 1
    idx <- population$treatment == 1
    survTarget <- CohortMethod:::adjustedKm(weight = population$weight[idx],
                                            time = population$survivalTime[idx],
                                            y = population$y[idx])
    survTarget$targetSurvivalUb <- survTarget$s^exp(qnorm(0.975)/log(survTarget$s)*sqrt(survTarget$var)/survTarget$s)
    survTarget$targetSurvivalLb <- survTarget$s^exp(qnorm(0.025)/log(survTarget$s)*sqrt(survTarget$var)/survTarget$s)
    survTarget$targetSurvivalLb[survTarget$s > 0.9999] <- survTarget$s[survTarget$s > 0.9999]
    survTarget$targetSurvival <- survTarget$s
    survTarget$s <- NULL
    survTarget$var <- NULL
    idx <- population$treatment == 0
    survComparator <- CohortMethod:::adjustedKm(weight = population$weight[idx],
                                                time = population$survivalTime[idx],
                                                y = population$y[idx])
    survComparator$comparatorSurvivalUb <- survComparator$s^exp(qnorm(0.975)/log(survComparator$s)*sqrt(survComparator$var)/survComparator$s)
    survComparator$comparatorSurvivalLb <- survComparator$s^exp(qnorm(0.025)/log(survComparator$s)*sqrt(survComparator$var)/survComparator$s)
    survComparator$comparatorSurvivalLb[survComparator$s > 0.9999] <- survComparator$s[survComparator$s > 0.9999]
    survComparator$comparatorSurvival <- survComparator$s
    survComparator$s <- NULL
    survComparator$var <- NULL
    data <- merge(survTarget, survComparator, all = TRUE)

    cutoff <- quantile(population$survivalTime, dataCutoff)
    data <- data[data$time <= cutoff, ]
    if (cutoff <= 300) {
        xBreaks <- seq(0, cutoff, by = 50)
    } else if (cutoff <= 600) {
        xBreaks <- seq(0, cutoff, by = 100)
    } else {
        xBreaks <- seq(0, cutoff, by = 250)
    }

    targetAtRisk <- c()
    comparatorAtRisk <- c()
    for (xBreak in xBreaks) {
        targetAtRisk <- c(targetAtRisk, sum(population$treatment == 1 & population$survivalTime >= xBreak))
        comparatorAtRisk <- c(comparatorAtRisk, sum(population$treatment == 0 & population$survivalTime >= xBreak))
    }
    data <- merge(data,
                  data.frame(time = xBreaks,
                             targetAtRisk = targetAtRisk,
                             comparatorAtRisk = comparatorAtRisk),
                  all = TRUE)
    if (is.na(data$targetSurvival[1])) {
        data$targetSurvival[1] <- 1
        data$targetSurvivalUb[1] <- 1
        data$targetSurvivalLb[1] <- 1
    }
    if (is.na(data$comparatorSurvival[1])) {
        data$comparatorSurvival[1] <- 1
        data$comparatorSurvivalUb[1] <- 1
        data$comparatorSurvivalLb[1] <- 1
    }
    idx <- which(is.na(data$targetSurvival))
    while (length(idx) > 0) {
        data$targetSurvival[idx] <- data$targetSurvival[idx - 1]
        data$targetSurvivalLb[idx] <- data$targetSurvivalLb[idx - 1]
        data$targetSurvivalUb[idx] <- data$targetSurvivalUb[idx - 1]
        idx <- which(is.na(data$targetSurvival))
    }
    idx <- which(is.na(data$comparatorSurvival))
    while (length(idx) > 0) {
        data$comparatorSurvival[idx] <- data$comparatorSurvival[idx - 1]
        data$comparatorSurvivalLb[idx] <- data$comparatorSurvivalLb[idx - 1]
        data$comparatorSurvivalUb[idx] <- data$comparatorSurvivalUb[idx - 1]
        idx <- which(is.na(data$comparatorSurvival))
    }
    data$targetSurvival <- round(data$targetSurvival, 4)
    data$targetSurvivalLb <- round(data$targetSurvivalLb, 4)
    data$targetSurvivalUb <- round(data$targetSurvivalUb, 4)
    data$comparatorSurvival <- round(data$comparatorSurvival, 4)
    data$comparatorSurvivalLb <- round(data$comparatorSurvivalLb, 4)
    data$comparatorSurvivalUb <- round(data$comparatorSurvivalUb, 4)

    # Remove duplicate (except time) entries:
    data <- data[order(data$time), ]
    data <- data[!duplicated(data[, -1]), ]
    return(data)
}

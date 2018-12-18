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
#' @param indicationId          A string denoting the indicationId for which the results should be
#'                              exported.
#' @param outputFolder          Name of local folder to place results; make sure to use forward slashes
#'                              (/). Do not use a folder on a network drive since this greatly impacts
#'                              performance.
#' @param databaseId            A short string for identifying the database (e.g. 'Synpuf').
#' @param databaseName          The full name of the database.
#' @param databaseDescription   A short description (several sentences) of the database.
#' @param minCellCount          The minimum cell count for fields contains person counts or fractions.
#' @param maxCores              How many parallel cores should be used? If more cores are made
#'                              available this can speed up the analyses.
#'
#' @export
exportResults <- function(indicationId = "Depression",
                          outputFolder,
                          databaseId,
                          databaseName,
                          databaseDescription,
                          minCellCount = 5,
                          maxCores) {
    indicationFolder <- file.path(outputFolder, indicationId)
    exportFolder <- file.path(indicationFolder, "export")
    if (!file.exists(exportFolder)) {
        dir.create(exportFolder, recursive = TRUE)
    }

    exportIndication(indicationId = indicationId,
                     outputFolder = outputFolder,
                     exportFolder = exportFolder,
                     databaseId = databaseId)

    exportAnalyses(indicationId = indicationId,
                   outputFolder = outputFolder,
                   exportFolder = exportFolder,
                   databaseId = databaseId)

    exportExposures(indicationId = indicationId,
                    outputFolder = outputFolder,
                    exportFolder = exportFolder,
                    databaseId = databaseId)

    exportOutcomes(indicationId = indicationId,
                   outputFolder = outputFolder,
                   exportFolder = exportFolder,
                   databaseId = databaseId)

    exportMetadata(indicationId = indicationId,
                   outputFolder = outputFolder,
                   exportFolder = exportFolder,
                   databaseId = databaseId,
                   databaseName = databaseName,
                   databaseDescription = databaseDescription,
                   minCellCount = minCellCount)

    exportMainResults(indicationId = indicationId,
                      outputFolder = outputFolder,
                      exportFolder = exportFolder,
                      databaseId = databaseId,
                      minCellCount = minCellCount,
                      maxCores = maxCores)

    exportDiagnostics(indicationId = indicationId,
                      outputFolder = outputFolder,
                      exportFolder = exportFolder,
                      databaseId = databaseId,
                      minCellCount = minCellCount,
                      maxCores = maxCores)

    # Add all to zip file -------------------------------------------------------------------------------
    ParallelLogger::logInfo("Adding results to zip file")
    zipName <- file.path(exportFolder, paste0("Results", indicationId, databaseId, ".zip"))
    files <- list.files(exportFolder, pattern = ".*\\.csv$")
    oldWd <- setwd(exportFolder)
    on.exit(setwd(oldWd))
    DatabaseConnector::createZipFile(zipFile = zipName, files = files)
    ParallelLogger::logInfo("Results are ready for sharing at:", zipName)
}

swapColumnContents <- function(df, column1 = "targetId", column2 = "comparatorId") {
    temp <- df[, column1]
    df[, column1] <- df[, column2]
    df[, column2] <- temp
    return(df)
}


getAsymAnalysisIds <- function() {
    cmAnalysisListFile <- system.file("settings",
                                      sprintf("cmAnalysisListAsym%s.json", indicationId),
                                      package = "Legend")
    cmAnalysisListAsym <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)
    analysisIds <- as.vector(unlist(ParallelLogger::selectFromList(cmAnalysisListAsym, "analysisId")))
    return(analysisIds)
}


enforceMinCellValue <- function(data, fieldName, minValues, silent = FALSE) {
    toCensor <- !is.na(data[, fieldName]) & data[, fieldName] < minValues & data[, fieldName] != 0
    if (!silent) {
        percent <- round(100 * sum(toCensor)/nrow(data), 1)
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

exportIndication <- function(indicationId, outputFolder, exportFolder, databaseId) {
    ParallelLogger::logInfo("Exporting indication")
    ParallelLogger::logInfo("- indication table")
    pathToCsv <- system.file("settings", "Indications.csv", package = "Legend")
    indications <- read.csv(pathToCsv)
    indications$definition <- ""
    indications <- indications[indications$indicationId == indicationId, ]
    indicationTable <- indications[, c("indicationId", "indicationName", "definition")]
    colnames(indicationTable) <- SqlRender::camelCaseToSnakeCase(colnames(indicationTable))
    fileName <- file.path(exportFolder, "indication.csv")
    write.csv(indicationTable, fileName, row.names = FALSE)
}

exportAnalyses <- function(indicationId, outputFolder, exportFolder, databaseId) {
    ParallelLogger::logInfo("Exporting analyses")
    ParallelLogger::logInfo("- cohort_method_analysis table")

    tempFileName <- tempfile()

    cmAnalysisListFile <- system.file("settings",
                                      sprintf("cmAnalysisList%s.json", indicationId),
                                      package = "Legend")

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
    cohortMethodAnalysis <- unique(cohortMethodAnalysis)
    unlink(tempFileName)
    colnames(cohortMethodAnalysis) <- SqlRender::camelCaseToSnakeCase(colnames(cohortMethodAnalysis))
    fileName <- file.path(exportFolder, "cohort_method_analysis.csv")
    write.csv(cohortMethodAnalysis, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- covariate_analysis table")
    indicationFolder <- file.path(outputFolder, indicationId)
    if (!file.exists(file.path(indicationFolder, "allCovariates"))) {
        warning("Can't find allCovariates, skipping covariate_analysis table")
    } else {
        covariateData <- FeatureExtraction::loadCovariateData(file.path(indicationFolder,
                                                                        "allCovariates"))
        covariateAnalysis <- ff::as.ram(covariateData$analysisRef)
        covariateAnalysis <- covariateAnalysis[, c("analysisId", "analysisName")]
        colnames(covariateAnalysis) <- c("covariate_analysis_id", "covariate_analysis_name")
        fileName <- file.path(exportFolder, "covariate_analysis.csv")
        write.csv(covariateAnalysis, fileName, row.names = FALSE)
    }

    ParallelLogger::logInfo("- incidence_analysis table")
    incidenceAnalysis <- data.frame(incidence_analysis_id = c("On-treatment", "Intent-to-treat"),
                                    description = c("On-treatment", "Intent-to-treat"))
    fileName <- file.path(exportFolder, "incidence_analysis.csv")
    write.csv(incidenceAnalysis, fileName, row.names = FALSE)
}

exportExposures <- function(indicationId, outputFolder, exportFolder, databaseId) {
    ParallelLogger::logInfo("Exporting exposures")
    ParallelLogger::logInfo("- single_exposure_of_interest table")
    pathToCsv <- system.file("settings", "ExposuresOfInterest.csv", package = "Legend")
    exposuresOfInterest <- read.csv(pathToCsv)
    singleExposuresOfInterest <- exposuresOfInterest[, c("cohortId", "name", "indicationId")]
    singleExposuresOfInterest$definition <- ""
    singleExposuresOfInterest$filterConceptIds <- ""
    singleExposuresOfInterest$description <- sprintf("First use of %s",
                                                     tolower(singleExposuresOfInterest$name))
    idx <- singleExposuresOfInterest$indicationId == "Hypertension"
    singleExposuresOfInterest$description[idx] <- sprintf("First-line %s monotherapy",
                                                          tolower(singleExposuresOfInterest$name[idx]))
    singleExposuresOfInterest <- singleExposuresOfInterest[singleExposuresOfInterest$indicationId ==
                                                               indicationId, ]
    colnames(singleExposuresOfInterest) <- c("exposure_id",
                                             "exposure_name",
                                             "indication_id",
                                             "definition",
                                             "filter_concept_ids",
                                             "description")
    fileName <- file.path(exportFolder, "single_exposure_of_interest.csv")
    write.csv(singleExposuresOfInterest, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- combi_exposure_of_interest table")
    pathToCsv <- file.path(outputFolder, indicationId, "exposureCombis.csv")
    if (file.exists(pathToCsv)) {
        combiExposures <- read.csv(pathToCsv, stringsAsFactors = FALSE)
        combiExposures$indicationId <- indicationId
        combiExposures <- combiExposures[, c("cohortDefinitionId",
                                             "cohortName",
                                             "exposureId1",
                                             "exposureId2",
                                             "indicationId")]
        combiExposures$description <- sprintf("First-line dual therapy of %s",
                                              tolower(gsub("&", "and", combiExposures$cohortName)))
        colnames(combiExposures) <- c("exposure_id",
                                      "exposure_name",
                                      "single_exposure_id_1",
                                      "single_exposure_id_2",
                                      "indication_id",
                                      "description")
    } else {
        combiExposures <- NULL
    }
    if (is.null(combiExposures) || nrow(combiExposures) == 0) {
        combiExposures <- data.frame(exposure_id = -1,
                                     exposure_name = "dummy",
                                     single_exposure_id_1 = -1,
                                     single_exposure_id_2 = -1,
                                     indication_id = "Dummy",
                                     description = "dummy")
    }
    fileName <- file.path(exportFolder, "combi_exposure_of_interest.csv")
    write.csv(combiExposures, fileName, row.names = FALSE)


    ParallelLogger::logInfo("- exposure_group table")
    pathToCsv <- system.file("settings", "ExposuresOfInterest.csv", package = "Legend")
    exposuresOfInterest <- read.csv(pathToCsv)
    exposureGroup <- exposuresOfInterest[exposuresOfInterest$indicationId == indicationId, c("cohortId",
                                                                                             "type")]
    pathToCsv <- file.path(outputFolder, indicationId, "exposureCombis.csv")
    if (file.exists(pathToCsv)) {
        combiExposures <- read.csv(pathToCsv, stringsAsFactors = FALSE)
        exposureGroup <- rbind(exposureGroup,
                               data.frame(cohortId = combiExposures$cohortDefinitionId,
                                          type = combiExposures$exposureType))
    }
    colnames(exposureGroup) <- c("exposure_id", "exposure_group")
    fileName <- file.path(exportFolder, "exposure_group.csv")
    write.csv(exposureGroup, fileName, row.names = FALSE)
}

exportOutcomes <- function(indicationId, outputFolder, exportFolder, databaseId) {
    ParallelLogger::logInfo("Exporting outcomes")
    ParallelLogger::logInfo("- outcome_of_interest table")
    pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Legend")
    outcomesOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)
    outcomesOfInterest <- outcomesOfInterest[outcomesOfInterest$indicationId == indicationId, ]
    getDefinition <- function(name) {
        fileName <- system.file("cohorts", paste0(name, ".json"), package = "Legend")
        return(readChar(fileName, file.info(fileName)$size))
    }
    outcomesOfInterest$definition <- sapply(outcomesOfInterest$name, getDefinition)
    outcomesOfInterest$description <- ""
    outcomesOfInterest <- outcomesOfInterest[, c("cohortId",
                                                 "name",
                                                 "definition",
                                                 "indicationId",
                                                 "description")]
    colnames(outcomesOfInterest) <- c("outcome_id",
                                      "outcome_name",
                                      "definition",
                                      "indication_id",
                                      "description")
    fileName <- file.path(exportFolder, "outcome_of_interest.csv")
    write.csv(outcomesOfInterest, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- negative_control_outcome table")
    pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Legend")
    negativeControls <- read.csv(pathToCsv)
    negativeControls <- negativeControls[negativeControls$indicationId == indicationId, ]
    negativeControls <- negativeControls[, c("cohortId", "name", "conceptId", "indicationId")]
    colnames(negativeControls) <- c("outcome_id", "outcome_name", "concept_id", "indication_id")
    fileName <- file.path(exportFolder, "negative_control_outcome.csv")
    write.csv(negativeControls, fileName, row.names = FALSE)
    colnames(negativeControls) <- SqlRender::snakeCaseToCamelCase(colnames(negativeControls))  # Need this later

    ParallelLogger::logInfo("- positive_control_outcome table")
    pathToCsv <- file.path(outputFolder, indicationId, "signalInjectionSummary.csv")
    positiveControls <- read.csv(pathToCsv, stringsAsFactors = FALSE)
    positiveControls$indicationId <- indicationId
    positiveControls <- merge(positiveControls,
                              negativeControls[negativeControls$indicationId == indicationId,
                                               c("outcomeId", "outcomeName")])
    positiveControls$outcomeName <- paste0(positiveControls$outcomeName,
                                           ", RR = ",
                                           positiveControls$targetEffectSize)
    positiveControls <- positiveControls[, c("newOutcomeId",
                                             "outcomeName",
                                             "exposureId",
                                             "outcomeId",
                                             "targetEffectSize",
                                             "indicationId")]
    colnames(positiveControls) <- c("outcomeId",
                                    "outcomeName",
                                    "exposureId",
                                    "negativeControlId",
                                    "effectSize",
                                    "indication_id")
    colnames(positiveControls) <- SqlRender::camelCaseToSnakeCase(colnames(positiveControls))
    fileName <- file.path(exportFolder, "positive_control_outcome.csv")
    write.csv(positiveControls, fileName, row.names = FALSE)
}

exportMetadata <- function(indicationId,
                           outputFolder,
                           exportFolder,
                           databaseId,
                           databaseName,
                           databaseDescription,
                           minCellCount) {
    ParallelLogger::logInfo("Exporting metadata")
    ParallelLogger::logInfo("- database table")
    database <- data.frame(database_id = databaseId,
                           database_name = databaseName,
                           description = databaseDescription,
                           is_meta_analysis = 0)
    fileName <- file.path(exportFolder, "database.csv")
    write.csv(database, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- exposure_summary table")
    pathToCsv <- file.path(outputFolder, indicationId, "pairedExposureSummary.csv")
    if (file.exists(pathToCsv)) {
        exposurePairs <- read.csv(pathToCsv, stringsAsFactors = FALSE)
    } else {
        warning("Can't find pairedExposureSummary.csv, so using pairedExposureSummaryFilteredBySize")
        pathToCsv <- file.path(outputFolder, indicationId, "pairedExposureSummaryFilteredBySize.csv")
        exposurePairs <- read.csv(pathToCsv, stringsAsFactors = FALSE)
    }
    exposurePairs$databaseId <- databaseId
    exposureSummary1 <- exposurePairs[, c("databaseId", "targetId", "targetMinDate", "targetMaxDate")]
    colnames(exposureSummary1) <- c("databaseId", "exposureId", "minDate", "maxDate")
    exposureSummary2 <- exposurePairs[, c("databaseId",
                                          "comparatorId",
                                          "comparatorMinDate",
                                          "comparatorMaxDate")]
    colnames(exposureSummary2) <- c("databaseId", "exposureId", "minDate", "maxDate")
    exposureSummary <- unique(rbind(exposureSummary1, exposureSummary2))
    colnames(exposureSummary) <- SqlRender::camelCaseToSnakeCase(colnames(exposureSummary))
    fileName <- file.path(exportFolder, "exposure_summary.csv")
    write.csv(exposureSummary, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- comparison_summary table")
    exposurePairs <- exposurePairs[, c("databaseId",
                                       "targetId",
                                       "comparatorId",
                                       "pairedMinDate",
                                       "pairedMaxDate")]
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
    pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds")
    if (!file.exists(pathToRds)) {
        warning("Cannot find ", pathToRds)
        return(NULL)
    }
    outcomeModelReference1 <- readRDS(pathToRds)
    pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference2.rds")
    outcomeModelReference2 <- readRDS(pathToRds)
    pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference3.rds")
    outcomeModelReference3 <- readRDS(pathToRds)
    outcomeModelReference <- rbind(outcomeModelReference1,
                                   outcomeModelReference2,
                                   outcomeModelReference3)
    outcomeModelReference <- outcomeModelReference[outcomeModelReference$outcomeId %in% outcomesOfInterest$cohortId, ]

    # Flag assymmetric analyses:
    analysisIds <- Legend:::getAsymAnalysisIds()
    outcomeModelReference$symmetrical <- TRUE
    outcomeModelReference$symmetrical[outcomeModelReference$analysisId %in% analysisIds] <- FALSE

    first <- !file.exists(fileName)
    pb <- txtProgressBar(style = 3)
    for (i in 1:nrow(outcomeModelReference)) {
        outcomeModel <- readRDS(file.path(outputFolder,
                                          indicationId,
                                          "cmOutput",
                                          outcomeModelReference$outcomeModelFile[i]))
        attrition <- outcomeModel$attrition[, c("description", "targetPersons", "comparatorPersons")]
        attrition <- attrition[2:nrow(attrition), ]  # First row is duplicate of last row from DB pull
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
            attrition <- rbind(attrition,
                               Legend:::swapColumnContents(attrition, "targetId", "comparatorId"))
        }
        attrition$analysisId <- outcomeModelReference$analysisId[i]
        attrition$outcomeId <- outcomeModelReference$outcomeId[i]
        attrition$databaseId <- databaseId
        attrition <- attrition[, c("databaseId",
                                   "exposureId",
                                   "targetId",
                                   "comparatorId",
                                   "outcomeId",
                                   "analysisId",
                                   "sequenceNumber",
                                   "description",
                                   "subjects")]
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
    if (!file.exists(pathToCsv)) {
        warning("Cannot find attrition.csv (attrition from DB), not adding to attrition table")
    } else {
        attritionFromDb <- read.csv(pathToCsv, stringsAsFactors = FALSE)
        attritionFromDb$targetId[attritionFromDb$targetId == -1] <- NA
        attritionFromDb$comparatorId[attritionFromDb$comparatorId == -1] <- NA
        attritionFromDbTc <- attritionFromDb[!is.na(attritionFromDb$targetId), ]
        attritionFromDb <- rbind(attritionFromDb, Legend:::swapColumnContents(attritionFromDbTc,
                                                                              "targetId",
                                                                              "comparatorId"))
        attritionFromDb$analysisId <- NA
        attritionFromDb$outcomeId <- NA
        attritionFromDb$databaseId <- databaseId
        attritionFromDb <- attritionFromDb[, c("databaseId",
                                               "exposureId",
                                               "targetId",
                                               "comparatorId",
                                               "outcomeId",
                                               "analysisId",
                                               "sequenceNumber",
                                               "description",
                                               "subjects")]
        colnames(attritionFromDb) <- SqlRender::camelCaseToSnakeCase(colnames(attritionFromDb))
        write.table(x = attritionFromDb,
                    file = fileName,
                    row.names = FALSE,
                    col.names = first,
                    sep = ",",
                    dec = ".",
                    qmethod = "double",
                    append = !first)
    }

    ParallelLogger::logInfo("- covariate table")
    covariateFolder <- file.path(outputFolder, indicationId, "allCovariates")
    if (!file.exists(pathToCsv)) {
        warning("Cannot find allCovariates folder, skipping covariate table")
    } else {
        covariateData <- FeatureExtraction::loadCovariateData(covariateFolder)
        covariateNames <- ff::as.ram(covariateData$covariateRef[,
                                                                c("covariateId", "covariateName", "analysisId")])
        covariateNames <- unique(covariateNames)
        covariateNames$databaseId <- databaseId
        colnames(covariateNames)[colnames(covariateNames) == "analysisId"] <- "covariateAnalysisId"
        colnames(covariateNames) <- SqlRender::camelCaseToSnakeCase(colnames(covariateNames))
        fileName <- file.path(exportFolder, "covariate.csv")
        write.csv(covariateNames, fileName, row.names = FALSE)
        rm(covariateNames)  # Free up memory
    }

    ParallelLogger::logInfo("- cm_follow_up_dist table")
    pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Legend")
    outcomesOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)

    pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds")
    outcomeModelReference1 <- readRDS(pathToRds)
    pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference2.rds")
    outcomeModelReference2 <- readRDS(pathToRds)
    pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference3.rds")
    outcomeModelReference3 <- readRDS(pathToRds)
    outcomeModelReference <- rbind(outcomeModelReference1,
                                   outcomeModelReference2,
                                   outcomeModelReference3)
    outcomeModelReference <- outcomeModelReference[outcomeModelReference$outcomeId %in% outcomesOfInterest$cohortId, ]
    analysisIds <- Legend:::getAsymAnalysisIds()
    outcomeModelReference$symmetrical <- TRUE
    outcomeModelReference$symmetrical[outcomeModelReference$analysisId %in% analysisIds] <- FALSE

    getResult <- function(i) {
        strataPop <- readRDS(file.path(outputFolder,
                                       indicationId,
                                       "cmOutput",
                                       outcomeModelReference$strataFile[i]))
        targetDist <- quantile(strataPop$survivalTime[strataPop$treatment == 1],
                               c(0, 0.1, 0.25, 0.5, 0.85, 0.9, 1))
        comparatorDist <- quantile(strataPop$survivalTime[strataPop$treatment == 0],
                                   c(0, 0.1, 0.25, 0.5, 0.85, 0.9, 1))
        row <- data.frame(target_id = outcomeModelReference$targetId[i],
                          comparator_id = outcomeModelReference$comparatorId[i],
                          outcome_id = outcomeModelReference$outcomeId[i],
                          analysis_id = outcomeModelReference$analysisId[i],
                          target_min_days = targetDist[1],
                          target_p10_days = targetDist[2],
                          target_p25_days = targetDist[3],
                          target_median_days = targetDist[4],
                          target_p75_days = targetDist[5],
                          target_p90_days = targetDist[6],
                          target_max_days = targetDist[7],
                          comparator_min_days = comparatorDist[1],
                          comparator_p10_days = comparatorDist[2],
                          comparator_p25_days = comparatorDist[3],
                          comparator_median_days = comparatorDist[4],
                          comparator_p75_days = comparatorDist[5],
                          comparator_p90_days = comparatorDist[6],
                          comparator_max_days = comparatorDist[7])
        if (outcomeModelReference$symmetrical[i]) {
            row2 <- data.frame(target_id = outcomeModelReference$comparatorId[i],
                               comparator_id = outcomeModelReference$targetId[i],
                               outcome_id = outcomeModelReference$outcomeId[i],
                               analysis_id = outcomeModelReference$analysisId[i],
                               target_min_days = comparatorDist[1],
                               target_p10_days = comparatorDist[2],
                               target_p25_days = comparatorDist[3],
                               target_median_days = comparatorDist[4],
                               target_p75_days = comparatorDist[5],
                               target_p90_days = comparatorDist[6],
                               target_max_days = comparatorDist[7],
                               comparator_min_days = targetDist[1],
                               comparator_p10_days = targetDist[2],
                               comparator_p25_days = targetDist[3],
                               comparator_median_days = targetDist[4],
                               comparator_p75_days = targetDist[5],
                               comparator_p90_days = targetDist[6],
                               comparator_max_days = targetDist[7])
            row <- rbind(row, row2)

        }
        return(row)
    }
    results <- plyr::llply(1:nrow(outcomeModelReference), getResult, .progress = "text")
    results <- do.call("rbind", results)
    results$database_id <- databaseId
    fileName <- file.path(exportFolder, "cm_follow_up_dist.csv")
    write.csv(results, fileName, row.names = FALSE)
    rm(results)  # Free up memory
}

exportMainResults <- function(indicationId,
                              outputFolder,
                              exportFolder,
                              databaseId,
                              minCellCount,
                              maxCores) {
    ParallelLogger::logInfo("Exporting main results")
    ParallelLogger::logInfo("- cohort_method_result table")
    positiveControls <- read.csv(file.path(exportFolder, "positive_control_outcome.csv"))
    colnames(positiveControls) <- SqlRender::snakeCaseToCamelCase(colnames(positiveControls))
    negativeControls <- read.csv(file.path(exportFolder, "negative_control_outcome.csv"))
    colnames(negativeControls) <- SqlRender::snakeCaseToCamelCase(colnames(negativeControls))
    summaryFile1 <- file.path(outputFolder, indicationId, "analysisSummary1.csv")
    analysesSum1 <- read.csv(summaryFile1)
    analysesSum2 <- read.csv(file.path(outputFolder, indicationId, "analysisSummary2.csv"))
    analysesSum3 <- read.csv(file.path(outputFolder, indicationId, "analysisSummary3.csv"))
    if (file.exists(file.path(outputFolder, indicationId, "analysisSummary4.csv"))) {
        analysesSum4 <- read.csv(file.path(outputFolder, indicationId, "analysisSummary4.csv"))
        analysesSum <- rbind(analysesSum1[,
                                          colnames(analysesSum2)],
                             analysesSum2,
                             analysesSum3,
                             analysesSum4)
    } else {
        analysesSum <- rbind(analysesSum1[,
                                          colnames(analysesSum2)],
                             analysesSum2,
                             analysesSum3)
    }
    analysisIds <- Legend:::getAsymAnalysisIds()
    analysesSum$symmetrical <- TRUE
    analysesSum$symmetrical[analysesSum$analysisId %in% analysisIds] <- FALSE

    ParallelLogger::logInfo("  Performing empirical calibration on main effects")
    cluster <- ParallelLogger::makeCluster(min(6, maxCores))
    subsets <- split(analysesSum,
                     paste(analysesSum$targetId, analysesSum$comparatorId, analysesSum$analysisId))
    rm(analysesSum)  # Free up memory
    results <- ParallelLogger::clusterApply(cluster,
                                            subsets,
                                            Legend:::calibrate,
                                            negativeControls = negativeControls,
                                            positiveControls = positiveControls)
    ParallelLogger::stopCluster(cluster)
    rm(subsets)  # Free up memory
    results <- do.call("rbind", results)
    results$databaseId <- databaseId
    results <- enforceMinCellValue(results, "targetSubjects", minCellCount)
    results <- enforceMinCellValue(results, "comparatorSubjects", minCellCount)
    results <- enforceMinCellValue(results, "targetOutcomes", minCellCount)
    results <- enforceMinCellValue(results, "comparatorOutcomes", minCellCount)
    colnames(results) <- SqlRender::camelCaseToSnakeCase(colnames(results))
    fileName <- file.path(exportFolder, "cohort_method_result.csv")
    write.csv(results, fileName, row.names = FALSE)
    rm(results)  # Free up memory

    ParallelLogger::logInfo("- cm_interaction_result table")
    pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds")
    outcomeModelReference <- readRDS(pathToRds)
    outcomeModelReference <- outcomeModelReference[outcomeModelReference$analysisId > 4, ]
    if (nrow(outcomeModelReference) > 0) {
        loadInteractionsFromOutcomeModel <- function(i) {
            outcomeModel <- readRDS(file.path(outputFolder,
                                              indicationId,
                                              "cmOutput",
                                              outcomeModelReference$outcomeModelFile[i]))
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
        interactions <- plyr::llply(1:nrow(outcomeModelReference),
                                    loadInteractionsFromOutcomeModel,
                                    .progress = "text")
        interactions <- do.call("rbind", interactions)

        ParallelLogger::logInfo("  Performing empirical calibration on interaction effects")
        cluster <- ParallelLogger::makeCluster(min(6, maxCores))
        subsets <- split(interactions,
                         paste(interactions$targetId, interactions$comparatorId, interactions$analysisId))
        interactions <- ParallelLogger::clusterApply(cluster,
                                                     subsets,
                                                     Legend:::calibrateInteractions,
                                                     negativeControls = negativeControls)
        ParallelLogger::stopCluster(cluster)
        rm(subsets)  # Free up memory
        interactions <- do.call("rbind", interactions)

        # Add TC -> CT swap
        interactionsCt <- interactions
        interactionsCt <- Legend:::swapColumnContents(interactionsCt, "targetId", "comparatorId")
        interactionsCt <- Legend:::swapColumnContents(interactionsCt, "targetSubjects", "comparatorSubjects")
        interactionsCt <- Legend:::swapColumnContents(interactionsCt, "targetDays", "comparatorDays")
        interactionsCt <- Legend:::swapColumnContents(interactionsCt, "targetOutcomes", "comparatorOutcomes")
        interactionsCt$rrr <- 1/interactionsCt$rrr
        interactionsCt$logRrr <- -interactionsCt$logRrr
        temp <- 1/interactionsCt$ci95Lb
        interactionsCt$ci95Lb <- 1/interactionsCt$ci95Ub
        interactionsCt$ci95Ub <- temp
        interactions <- rbind(interactions, interactionsCt)
        interactions$databaseId <- databaseId

        interactions <- enforceMinCellValue(interactions, "targetSubjects", minCellCount)
        interactions <- enforceMinCellValue(interactions, "comparatorSubjects", minCellCount)
        interactions <- enforceMinCellValue(interactions, "targetOutcomes", minCellCount)
        interactions <- enforceMinCellValue(interactions, "comparatorOutcomes", minCellCount)
        colnames(interactions) <- SqlRender::camelCaseToSnakeCase(colnames(interactions))
        fileName <- file.path(exportFolder, "cm_interaction_result.csv")
        write.csv(interactions, fileName, row.names = FALSE)
        rm(interactions)  # Free up memory
    }

    ParallelLogger::logInfo("- incidence table")
    pathToCsv <- file.path(outputFolder, indicationId, "incidence.csv")
    if (!file.exists(pathToCsv)) {
        warning("Can't find incidence.csv, skipping incidence table")
    } else {
        incidence <- read.csv(pathToCsv)
        incidence$databaseId <- databaseId
        incidence <- enforceMinCellValue(incidence, "subjects", minCellCount)
        incidence <- enforceMinCellValue(incidence, "outcomes", minCellCount)
        colnames(incidence) <- SqlRender::camelCaseToSnakeCase(colnames(incidence))
        fileName <- file.path(exportFolder, "incidence.csv")
        write.csv(incidence, fileName, row.names = FALSE)
    }

    ParallelLogger::logInfo("- chronograph table")
    pathToCsv <- file.path(outputFolder, indicationId, "chronographData.csv")
    if (!file.exists(pathToCsv)) {
        warning("Can't find chronographData.csv, skipping chronograph table")
    } else {
        chronograph <- read.csv(pathToCsv)
        chronograph$databaseId <- databaseId
        chronograph <- chronograph[, c("databaseId",
                                       "exposureId",
                                       "outcomeId",
                                       "periodId",
                                       "outcomeCount",
                                       "expectedCount",
                                       "ic",
                                       "icLow",
                                       "icHigh")]
        colnames(chronograph) <- c("databaseId",
                                   "exposureId",
                                   "outcomeId",
                                   "time",
                                   "outcomes",
                                   "expectedOutcomes",
                                   "ic",
                                   "icLb",
                                   "icUb")
        # IC metric depends on number of observed outcomes, so consider together for minCellCount:
        toCensor <- chronograph$outcomes < minCellCount & chronograph$outcomes != 0
        percent <- round(100 * sum(toCensor)/nrow(chronograph), 1)
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
    if (subset$symmetrical[1]) {
        comparatorPcs <- merge(subset, data.frame(comparatorId = positiveControls$exposureId,
                                                  outcomeId = positiveControls$outcomeId,
                                                  effectSize = positiveControls$effectSize))
        subsetTc <- subset[!(subset$outcomeId %in% comparatorPcs$outcomeId), ]
        subsetCt <- subset[!(subset$outcomeId %in% targetPcs$outcomeId), ]
        subsetCt <- swapColumnContents(subsetCt, "targetId", "comparatorId")
        subsetCt <- swapColumnContents(subsetCt, "target", "comparator")
        subsetCt <- swapColumnContents(subsetCt, "targetDays", "comparatorDays")
        subsetCt <- swapColumnContents(subsetCt, "eventsTarget", "eventsComparator")
        subsetCt$rr <- 1/subsetCt$rr
        temp <- 1/subsetCt$ci95lb
        subsetCt$ci95lb <- 1/subsetCt$ci95ub
        subsetCt$ci95ub <- temp
        subsetCt$logRr <- -subsetCt$logRr
        comparatorPcs <- comparatorPcs[!is.na(comparatorPcs$seLogRr), ]
        if (nrow(comparatorPcs) > 5) {
            model <- EmpiricalCalibration::fitSystematicErrorModel(logRr = c(-ncs$logRr,
                                                                             -comparatorPcs$logRr),
                                                                   seLogRr = c(ncs$seLogRr,
                                                                               comparatorPcs$seLogRr),
                                                                   trueLogRr = c(rep(0, nrow(ncs)),
                                                                                 log(comparatorPcs$effectSize)),
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
    } else {
        subsetTc <- subset
        subsetCt <- NULL
    }
    targetPcs <- targetPcs[!is.na(targetPcs$seLogRr), ]
    if (nrow(targetPcs) > 5) {
        model <- EmpiricalCalibration::fitSystematicErrorModel(logRr = c(ncs$logRr, targetPcs$logRr),
                                                               seLogRr = c(ncs$seLogRr,
                                                                           targetPcs$seLogRr),
                                                               trueLogRr = c(rep(0, nrow(ncs)),
                                                                             log(targetPcs$effectSize)),
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
    subset <- rbind(subsetTc, subsetCt)
    subset$i2 <- rep(NA, nrow(subset))
    subset <- subset[, c("targetId",
                         "comparatorId",
                         "outcomeId",
                         "analysisId",
                         "rr",
                         "ci95lb",
                         "ci95ub",
                         "p",
                         "i2",
                         "logRr",
                         "seLogRr",
                         "target",
                         "comparator",
                         "targetDays",
                         "comparatorDays",
                         "eventsTarget",
                         "eventsComparator",
                         "calibratedP",
                         "calibratedRr",
                         "calibratedCi95Lb",
                         "calibratedCi95Ub",
                         "calibratedLogRr",
                         "calibratedSeLogRr")]
    colnames(subset) <- c("targetId",
                          "comparatorId",
                          "outcomeId",
                          "analysisId",
                          "rr",
                          "ci95Lb",
                          "ci95Ub",
                          "p",
                          "i2",
                          "logRr",
                          "seLogRr",
                          "targetSubjects",
                          "comparatorSubjects",
                          "targetDays",
                          "comparatorDays",
                          "targetOutcomes",
                          "comparatorOutcomes",
                          "calibratedP",
                          "calibratedRr",
                          "calibratedCi95Lb",
                          "calibratedCi95Ub",
                          "calibratedLogRr",
                          "calibratedSeLogRr")
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


exportDiagnostics <- function(indicationId,
                              outputFolder,
                              exportFolder,
                              databaseId,
                              minCellCount,
                              maxCores) {
    ParallelLogger::logInfo("Exporting diagnostics")
    ParallelLogger::logInfo("- covariate_balance table")
    fileName <- file.path(exportFolder, "covariate_balance.csv")
    if (file.exists(fileName)) {
        unlink(fileName)
    }
    first <- TRUE
    balanceFolder <- file.path(outputFolder, indicationId, "balance")
    files <- list.files(balanceFolder, pattern = "Bal_.*.rds", full.names = TRUE)
    analysisIds <- Legend:::getAsymAnalysisIds()
    pb <- txtProgressBar(style = 3)
    for (i in 1:length(files)) {
        ids <- gsub("^.*Bal_t", "", files[i])
        targetId <- as.numeric(gsub("_c.*", "", ids))
        ids <- gsub("^.*_c", "", ids)
        comparatorId <- as.numeric(gsub("_[aso].*$", "", ids))
        if (grepl("_s", ids)) {
            subgroupId <- as.numeric(gsub("^.*_s", "", gsub("_a[0-9]*.rds", "", ids)))
        } else {
            subgroupId <- NA
        }
        if (grepl("_o", ids)) {
            outcomeId <- as.numeric(gsub("^.*_o", "", gsub("_a[0-9]*.rds", "", ids)))
        } else {
            outcomeId <- NA
        }
        ids <- gsub("^.*_a", "", ids)
        analysisId <- as.numeric(gsub(".rds", "", ids))
        balance <- readRDS(files[i])
        inferredTargetBeforeSize <- mean(balance$beforeMatchingSumTarget/balance$beforeMatchingMeanTarget,
                                         na.rm = TRUE)
        inferredComparatorBeforeSize <- mean(balance$beforeMatchingSumComparator/balance$beforeMatchingMeanComparator,
                                             na.rm = TRUE)
        inferredTargetAfterSize <- mean(balance$afterMatchingSumTarget/balance$afterMatchingMeanTarget,
                                        na.rm = TRUE)
        inferredComparatorAfterSize <- mean(balance$afterMatchingSumComparator/balance$afterMatchingMeanComparator,
                                            na.rm = TRUE)

        balance$databaseId <- databaseId
        balance$targetId <- targetId
        balance$comparatorId <- comparatorId
        balance$outcomeId <- outcomeId
        balance$analysisId <- analysisId
        balance$interactionCovariateId <- subgroupId
        balance <- balance[, c("databaseId",
                               "targetId",
                               "comparatorId",
                               "outcomeId",
                               "analysisId",
                               "interactionCovariateId",
                               "covariateId",
                               "beforeMatchingMeanTarget",
                               "beforeMatchingMeanComparator",
                               "beforeMatchingStdDiff",
                               "afterMatchingMeanTarget",
                               "afterMatchingMeanComparator",
                               "afterMatchingStdDiff")]
        colnames(balance) <- c("databaseId",
                               "targetId",
                               "comparatorId",
                               "outcomeId",
                               "analysisId",
                               "interactionCovariateId",
                               "covariateId",
                               "targetMeanBefore",
                               "comparatorMeanBefore",
                               "stdDiffBefore",
                               "targetMeanAfter",
                               "comparatorMeanAfter",
                               "stdDiffAfter")
        balance$targetMeanBefore[is.na(balance$targetMeanBefore)] <- 0
        balance$comparatorMeanBefore[is.na(balance$comparatorMeanBefore)] <- 0
        balance$stdDiffBefore <- round(balance$stdDiffBefore, 3)
        balance$targetMeanAfter[is.na(balance$targetMeanAfter)] <- 0
        balance$comparatorMeanAfter[is.na(balance$comparatorMeanAfter)] <- 0
        balance$stdDiffAfter <- round(balance$stdDiffAfter, 3)
        if (!(analysisId %in% analysisIds)) {
            balanceCt <- swapColumnContents(balance, "targetId", "comparatorId")
            balanceCt <- swapColumnContents(balanceCt, "targetMeanBefore", "comparatorMeanBefore")
            balanceCt$stdDiffBefore <- -balanceCt$stdDiffBefore
            balanceCt <- swapColumnContents(balanceCt, "targetMeanAfter", "comparatorMeanAfter")
            balanceCt$stdDiffAfter <- -balanceCt$stdDiffAfter
            balance <- rbind(balance, balanceCt)
        }
        balance <- enforceMinCellValue(balance,
                                       "targetMeanBefore",
                                       minCellCount/inferredTargetBeforeSize,
                                       TRUE)
        balance <- enforceMinCellValue(balance,
                                       "comparatorMeanBefore",
                                       minCellCount/inferredComparatorBeforeSize,
                                       TRUE)
        balance <- enforceMinCellValue(balance,
                                       "targetMeanAfter",
                                       minCellCount/inferredTargetAfterSize,
                                       TRUE)
        balance <- enforceMinCellValue(balance,
                                       "comparatorMeanAfter",
                                       minCellCount/inferredComparatorAfterSize,
                                       TRUE)

        balance$targetMeanBefore <- round(balance$targetMeanBefore, 3)
        balance$comparatorMeanBefore <- round(balance$comparatorMeanBefore, 3)
        balance$targetMeanAfter <- round(balance$targetMeanAfter, 3)
        balance$comparatorMeanAfter <- round(balance$comparatorMeanAfter, 3)


        balance <- balance[balance$targetMeanBefore != 0 & balance$comparatorMeanBefore != 0 & balance$targetMeanAfter !=
                               0 & balance$comparatorMeanAfter != 0 & balance$stdDiffBefore != 0 & balance$stdDiffAfter !=
                               0, ]
        balance <- balance[!is.na(balance$targetId), ]
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

    ParallelLogger::logInfo("- preference_score_dist table")
    preparePlot <- function(i, outcomeModelReference) {
        psFileName <- file.path(outputFolder,
                                indicationId,
                                "cmOutput",
                                outcomeModelReference$sharedPsFile[i])
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
                reversed <- swapColumnContents(swapColumnContents(result, "targetId", "comparatorId"),
                                               "targetDensity",
                                               "comparatorDensity")
                reversed$preferenceScore <- 1 - reversed$preferenceScore
                result <- rbind(result, reversed)
                return(result)
            }
        }
        return(NULL)
    }
    outcomeModelReference <- readRDS(file.path(outputFolder,
                                               indicationId,
                                               "cmOutput",
                                               "outcomeModelReference1.rds"))
    outcomeModelReference <- outcomeModelReference[order(outcomeModelReference$sharedPsFile), ]
    outcomeModelReference <- outcomeModelReference[!duplicated(outcomeModelReference$sharedPsFile), ]
    data <- plyr::llply(1:nrow(outcomeModelReference),
                        preparePlot,
                        outcomeModelReference = outcomeModelReference,
                        .progress = "text")
    data <- do.call("rbind", data)
    fileName <- file.path(exportFolder, "preference_score_dist.csv")
    colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
    write.csv(data, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- propensity_model table")
    getPsModel <- function(i, outcomeModelReference) {
        psFileName <- file.path(outputFolder,
                                indicationId,
                                "cmOutput",
                                outcomeModelReference$sharedPsFile[i])
        if (file.exists(psFileName)) {
            ps <- readRDS(psFileName)
            metaData <- attr(ps, "metaData")
            if (is.null(metaData$psError)) {
                cmDataFile <- file.path(outputFolder,
                                        indicationId,
                                        "cmOutput",
                                        outcomeModelReference$cohortMethodDataFolder[i])
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
    outcomeModelReference <- readRDS(file.path(outputFolder,
                                               indicationId,
                                               "cmOutput",
                                               "outcomeModelReference1.rds"))
    outcomeModelReference <- outcomeModelReference[order(outcomeModelReference$sharedPsFile), ]
    outcomeModelReference <- outcomeModelReference[!duplicated(outcomeModelReference$sharedPsFile), ]
    data <- plyr::llply(1:nrow(outcomeModelReference),
                        getPsModel,
                        outcomeModelReference = outcomeModelReference,
                        .progress = "text")
    data <- do.call("rbind", data)
    fileName <- file.path(exportFolder, "propensity_model.csv")
    colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
    write.csv(data, fileName, row.names = FALSE)

    ParallelLogger::logInfo("- kaplan_meier_dist table")
    ParallelLogger::logInfo("  Computing KM curves")
    pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference1.rds")
    outcomeModelReference1 <- readRDS(pathToRds)
    pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference2.rds")
    outcomeModelReference2 <- readRDS(pathToRds)
    pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference3.rds")
    outcomeModelReference3 <- readRDS(pathToRds)
    pathToRds <- file.path(outputFolder, indicationId, "cmOutput", "outcomeModelReference4.rds")
    if (file.exists(pathToRds)) {
        outcomeModelReference4 <- readRDS(pathToRds)
        outcomeModelReference <- rbind(outcomeModelReference1,
                                       outcomeModelReference2,
                                       outcomeModelReference3,
                                       outcomeModelReference4)
    } else {
        outcomeModelReference <- rbind(outcomeModelReference1,
                                       outcomeModelReference2,
                                       outcomeModelReference3)
    }
    outcomeModelReference <- outcomeModelReference[outcomeModelReference$strataFile != "", ]  # HOIs only
    outcomeModelReference <- outcomeModelReference[, c("strataFile",
                                                       "targetId",
                                                       "comparatorId",
                                                       "outcomeId",
                                                       "analysisId")]
    # Flag assymmetric analyses:
    analysisIds <- Legend:::getAsymAnalysisIds()
    outcomeModelReference$symmetrical <- TRUE
    outcomeModelReference$symmetrical[outcomeModelReference$analysisId %in% analysisIds] <- FALSE

    tempFolder <- file.path(exportFolder, "temp")
    if (!file.exists(tempFolder)) {
        dir.create(tempFolder)
    }
    cluster <- ParallelLogger::makeCluster(min(10, maxCores))
    tasks <- split(outcomeModelReference, seq(nrow(outcomeModelReference)))
    ParallelLogger::clusterApply(cluster,
                                 tasks,
                                 Legend:::prepareKm,
                                 outputFolder = outputFolder,
                                 tempFolder = tempFolder,
                                 indicationId = indicationId,
                                 databaseId = databaseId,
                                 minCellCount = minCellCount)
    ParallelLogger::stopCluster(cluster)
    ParallelLogger::logInfo("  Writing to single csv file")
    saveKmToCsv <- function(file, first, outputFile) {
        data <- readRDS(file)
        colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
        write.table(x = data,
                    file = outputFile,
                    row.names = FALSE,
                    col.names = first,
                    sep = ",",
                    dec = ".",
                    qmethod = "double",
                    append = !first)
    }
    outputFile <- file.path(exportFolder, "kaplan_meier_dist.csv")
    files <- list.files(tempFolder, "km_.*.rds", full.names = TRUE)
    saveKmToCsv(files[1], first = TRUE, outputFile = outputFile)
    plyr::l_ply(files[2:length(files)], saveKmToCsv, first = FALSE, outputFile = outputFile, .progress = "text")

    unlink(tempFolder, recursive = TRUE)
}

prepareKm <- function(task,
                      outputFolder,
                      tempFolder,
                      indicationId,
                      databaseId,
                      minCellCount) {
    ParallelLogger::logTrace("Preparing KM plot for target ",
                             task$targetId,
                             ", comparator ",
                             task$comparatorId,
                             ", outcome ",
                             task$outcomeId,
                             ", analysis ",
                             task$analysisId)
    outputFileName <- file.path(tempFolder, sprintf("km_t%s_c%s_o%s_a%s.rds",
                                                    task$targetId,
                                                    task$comparatorId,
                                                    task$outcomeId,
                                                    task$analysisId))
    if (file.exists(outputFileName)) {
        return(NULL)
    }
    population <- readRDS(file.path(outputFolder,
                                    indicationId,
                                    "cmOutput",
                                    task$strataFile))
    if (nrow(population) == 0) {
        # Can happen when matching and treatment is predictable
        return(NULL)
    }
    dataTc <- Legend:::prepareKaplanMeier(population)
    if (is.null(dataTc)) {
        # No shared strata
        return(NULL)
    }
    dataTc$targetId <- task$targetId
    dataTc$comparatorId <- task$comparatorId
    dataTc$outcomeId <- task$outcomeId
    dataTc$analysisId <- task$analysisId
    if (task$symmetrical) {
        population$treatment <- 1 - population$treatment
        dataCt <- Legend:::prepareKaplanMeier(population)
        dataCt$targetId <- task$comparatorId
        dataCt$comparatorId <- task$targetId
        dataCt$outcomeId <- task$outcomeId
        dataCt$analysisId <- task$analysisId
        data <- rbind(dataTc, dataCt)
    } else {
        data <- dataTc
    }
    data$databaseId <- databaseId
    data <- enforceMinCellValue(data, "targetAtRisk", minCellCount)
    data <- enforceMinCellValue(data, "comparatorAtRisk", minCellCount)
    saveRDS(data, outputFileName)
}

prepareKaplanMeier <- function(population) {
    dataCutoff <- 0.9
    population$y <- 0
    population$y[population$outcomeCount != 0] <- 1
    population$stratumSizeT <- 1
    strataSizesT <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 1, ], sum)
    if (max(strataSizesT$stratumSizeT) == 1) {
        # variable ratio matching: use propensity score to compute IPTW
        if (is.null(population$propensityScore)) {
            stop("Variable ratio matching detected, but no propensity score found")
        }
        weights <- aggregate(propensityScore ~ stratumId, population, mean)
        if (max(weights$propensityScore) > 0.99999) {
            return(NULL)
        }
        weights$weight <- weights$propensityScore / (1 - weights$propensityScore)
    } else {
        # stratification: infer probability of treatment from subject counts
        strataSizesC <- aggregate(stratumSizeT ~ stratumId, population[population$treatment == 0, ], sum)
        colnames(strataSizesC)[2] <- "stratumSizeC"
        weights <- merge(strataSizesT, strataSizesC)
        if (nrow(weights) == 0) {
            warning("No shared strata between target and comparator")
            return(NULL)
        }
        weights$weight <- weights$stratumSizeT/weights$stratumSizeC
    }
    population <- merge(population, weights[, c("stratumId", "weight")])
    population$weight[population$treatment == 1] <- 1
    idx <- population$treatment == 1
    survTarget <- CohortMethod:::adjustedKm(weight = population$weight[idx],
                                            time = population$survivalTime[idx],
                                            y = population$y[idx])
    survTarget$targetSurvivalUb <- survTarget$s^exp(qnorm(0.975)/log(survTarget$s) * sqrt(survTarget$var)/survTarget$s)
    survTarget$targetSurvivalLb <- survTarget$s^exp(qnorm(0.025)/log(survTarget$s) * sqrt(survTarget$var)/survTarget$s)
    survTarget$targetSurvivalLb[survTarget$s > 0.9999] <- survTarget$s[survTarget$s > 0.9999]
    survTarget$targetSurvival <- survTarget$s
    survTarget$s <- NULL
    survTarget$var <- NULL
    idx <- population$treatment == 0
    survComparator <- CohortMethod:::adjustedKm(weight = population$weight[idx],
                                                time = population$survivalTime[idx],
                                                y = population$y[idx])
    survComparator$comparatorSurvivalUb <- survComparator$s^exp(qnorm(0.975)/log(survComparator$s) *
                                                                    sqrt(survComparator$var)/survComparator$s)
    survComparator$comparatorSurvivalLb <- survComparator$s^exp(qnorm(0.025)/log(survComparator$s) *
                                                                    sqrt(survComparator$var)/survComparator$s)
    survComparator$comparatorSurvivalLb[survComparator$s > 0.9999] <- survComparator$s[survComparator$s >
                                                                                           0.9999]
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
        targetAtRisk <- c(targetAtRisk,
                          sum(population$treatment == 1 & population$survivalTime >= xBreak))
        comparatorAtRisk <- c(comparatorAtRisk,
                              sum(population$treatment == 0 & population$survivalTime >=
                                      xBreak))
    }
    data <- merge(data, data.frame(time = xBreaks,
                                   targetAtRisk = targetAtRisk,
                                   comparatorAtRisk = comparatorAtRisk), all = TRUE)
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

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

# Use this script to replace outcomes of interest, negative control outcomes, or subgroup definitions
# when the study has already been executed.

# This function just reruns the SQL to regenerate all outcome cohorts (HOIs + controls)
rerunOutcomesOnServer <- function() {
    ParallelLogger::logInfo("Rerunning all outcome cohorts on server")
    createOutcomeCohorts(connectionDetails = connectionDetails,
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         tablePrefix = tablePrefix,
                         indicationId = indicationId,
                         oracleTempSchema = oracleTempSchema,
                         outputFolder = outputFolder)
    ParallelLogger::logInfo("Done rerunning all outcome cohorts on server")
}

# Used by other data fetch functions. Creates #exposure_cohorts table and checks rowIds are
# consistent with original fetch.
prepareForDataFetch <- function(conn, indicationFolder) {
    pairedCohortTable <- paste(tablePrefix, tolower(indicationId), "pair_cohort", sep = "_")
    cohortsFolder <- file.path(indicationFolder, "allCohorts")
    exposureSummary <- read.csv(file.path(indicationFolder,
                                          "pairedExposureSummaryFilteredBySize.csv"))
    table <- exposureSummary[, c("targetId", "comparatorId")]
    colnames(table) <- SqlRender::camelCaseToSnakeCase(colnames(table))
    DatabaseConnector::insertTable(connection = conn,
                                   tableName = "#comparisons",
                                   data = table,
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   oracleTempSchema = oracleTempSchema)

    sql <- SqlRender::loadRenderTranslateSql("UnionExposureCohorts.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             paired_cohort_table = pairedCohortTable)
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

    sql <- "TRUNCATE TABLE #comparisons; DROP TABLE #comparisons;"
    sql <- SqlRender::translateSql(sql = sql,
                                   targetDialect = connectionDetails$dbms,
                                   oracleTempSchema = oracleTempSchema)$sql
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

    # Just to make sure: check of rowIds are consistent with those generated before:
    sql <- "SELECT row_id, subject_id, cohort_start_date FROM #exposure_cohorts"
    sql <- SqlRender::translateSql(sql = sql,
                                   targetDialect = connectionDetails$dbms,
                                   oracleTempSchema = oracleTempSchema)$sql
    newCohorts <- DatabaseConnector::querySql(conn, sql)
    colnames(newCohorts) <- SqlRender::snakeCaseToCamelCase(colnames(newCohorts))
    newCohorts <- newCohorts[order(newCohorts$rowId,
                                   newCohorts$subjectId,
                                   newCohorts$cohortStartDate), ]
    row.names(newCohorts) <- NULL
    allCohorts <- readRDS(file.path(cohortsFolder, "allCohorts.rds"))
    allCohorts <- allCohorts[, colnames(newCohorts)]
    allCohorts <- unique(allCohorts)
    allCohorts <- allCohorts[order(allCohorts$rowId,
                                   allCohorts$subjectId,
                                   allCohorts$cohortStartDate), ]
    row.names(allCohorts) <- NULL
    if (!all.equal(allCohorts, newCohorts)) {
        stop("row IDs have changed. Hot swap failed")
    }
    ParallelLogger::logInfo("Verified that rowIds are the same")
}

# This function fetches the outcomes from the server, and updates the allOutcomes folder accordingly.
fetchOutcomesToAllOutcomes <- function(outcomeIds) {
    ParallelLogger::logInfo("Fetching outcomes from the server, and updating the allOutcomes folder")
    indicationFolder <- file.path(outputFolder, indicationId)
    conn <- DatabaseConnector::connect(connectionDetails)

    prepareForDataFetch(conn, indicationFolder)

    outcomeCohortTable <- paste(tablePrefix, tolower(indicationId), "out_cohort", sep = "_")
    outcomesFolder <- file.path(indicationFolder, "allOutcomes")
    ParallelLogger::logTrace("Retrieving outcomes from database")
    sql <- SqlRender::loadRenderTranslateSql("GetOutcomes.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             outcome_database_schema = cohortDatabaseSchema,
                                             outcome_table = outcomeCohortTable,
                                             outcome_ids = outcomeIds)
    newOutcomes <- DatabaseConnector::querySql.ffdf(conn, sql)
    colnames(newOutcomes) <- SqlRender::snakeCaseToCamelCase(colnames(newOutcomes))

    sql <- "TRUNCATE TABLE #exposure_cohorts; DROP TABLE #exposure_cohorts;"
    sql <- SqlRender::translateSql(sql = sql,
                                   targetDialect = connectionDetails$dbms,
                                   oracleTempSchema = oracleTempSchema)$sql
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

    DatabaseConnector::disconnect(conn)

    tempFolder <- paste0(outcomesFolder, "_temp")
    ParallelLogger::logDebug("Renaming ", outcomesFolder, " to ", tempFolder)
    if (!file.rename(outcomesFolder, tempFolder)) {
        stop("allOutcomes folder locked")
    }
    ffbase::load.ffdf(tempFolder)

    # Drop old outcomes:
    idx <- ffbase::`%in%`(outcomes$outcomeId, ff::as.ff(outcomeIds))
    outcomes <- outcomes[!idx, ]

    # Append new outcomes:
    outcomes <- ffbase::ffdfappend(outcomes, newOutcomes)

    ParallelLogger::logTrace("Saving new outcomes object to ", outcomesFolder)
    ffbase::save.ffdf(outcomes, dir = outcomesFolder)
    ff::close.ffdf(outcomes)
    ff::close.ffdf(newOutcomes)
    rm(outcomes)
    rm(newOutcomes)
    unlink(tempFolder, recursive = TRUE, force = TRUE)
    ParallelLogger::logInfo("Done Fetching outcomes from the server, and updating the allOutcomes folder")
}


# This function fetches the subgroup covariates from the server, and updates the allCovariatess
# folder accordingly.
fetchSubgroupCovarsToAllCovariates <- function(subgroupIds) {
    ParallelLogger::logInfo("Fetching subgroup covariates from the server, and updating the allCovariatess folder")
    indicationFolder <- file.path(outputFolder, indicationId)
    conn <- DatabaseConnector::connect(connectionDetails)

    prepareForDataFetch(conn, indicationFolder)

    covariatesFolder <- file.path(indicationFolder, "allCovariates")
    ParallelLogger::logTrace("Retrieving subgroup covariates from database")

    subgroupCovariateSettings <- createSubgroupCovariateSettings()

    newCovariates <- FeatureExtraction::getDbCovariateData(connection = conn,
                                                           oracleTempSchema = oracleTempSchema,
                                                           cdmDatabaseSchema = cdmDatabaseSchema,
                                                           cdmVersion = 5,
                                                           cohortTable = "#exposure_cohorts",
                                                           cohortTableIsTemp = TRUE,
                                                           rowIdField = "row_id",
                                                           covariateSettings = subgroupCovariateSettings,
                                                           aggregated = FALSE)

    sql <- "TRUNCATE TABLE #exposure_cohorts; DROP TABLE #exposure_cohorts;"
    sql <- SqlRender::translateSql(sql = sql,
                                   targetDialect = connectionDetails$dbms,
                                   oracleTempSchema = oracleTempSchema)$sql
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

    DatabaseConnector::disconnect(conn)

    # Subset fetched covariates to only the new ones:
    idx <- ffbase::`%in%`(newCovariates$covariates$covariateId, ff::as.ff(subgroupIds))
    if (!ffbase::any.ff(idx)) {
        stop("New subgroup(s) not found in this database")
    }
    newCovariates$covariates <- newCovariates$covariates[idx, ]
    idx <- ffbase::`%in%`(newCovariates$covariateRef$covariateId, ff::as.ff(subgroupIds))
    newCovariates$covariateRef <- newCovariates$covariateRef[idx, ]

    tempFolder <- paste0(covariatesFolder, "_temp")
    ParallelLogger::logDebug("Renaming ", covariatesFolder, " to ", tempFolder)
    if (!file.rename(covariatesFolder, tempFolder)) {
        stop("allCovariates folder locked")
    }
    covariates <- FeatureExtraction::loadCovariateData(tempFolder)

    # Drop old covariates:
    idx <- ffbase::`%in%`(covariates$covariates$covariateId, ff::as.ff(subgroupIds))
    if (ffbase::any.ff(idx)) {
        covariates$covariates <- covariates$covariates[!idx, ]
    }
    idx <- ffbase::`%in%`(covariates$covariateRef$covariateId, ff::as.ff(subgroupIds))
    if (ffbase::any.ff(idx)) {
        covariates$covariateRef <- covariates$covariateRef[!idx, ]
    }
    # Append new outcomes:
    covariates$covariates <- ffbase::ffdfappend(covariates$covariates, newCovariates$covariates)
    covariates$covariateRef <- ffbase::ffdfappend(covariates$covariateRef, newCovariates$covariateRef)

    ParallelLogger::logTrace("Saving new covariates object to ", covariatesFolder)
    FeatureExtraction::saveCovariateData(covariates, covariatesFolder)
    ff::close.ffdf(covariates$covariates)
    ff::close.ffdf(covariates$covariateRef)
    ff::close.ffdf(covariates$analysisRef)
    ff::close.ffdf(newCovariates$covariates)
    ff::close.ffdf(newCovariates$covariateRef)
    ff::close.ffdf(newCovariates$analysisRef)
    rm(covariates)
    rm(newCovariates)
    unlink(tempFolder, recursive = TRUE, force = TRUE)
    ParallelLogger::logInfo("Done fetching subgroup covariates from the server, and updating the allCovariatess folder")
}

# Update the outcomes portion of the CohortMethod data objects. Includes both negative controls,
# positive controls, and HOIs
updateCohortMethodDataOutcomes <- function() {
    ParallelLogger::logInfo("Updating CohortMethodData objects")
    indicationFolder <- file.path(outputFolder, indicationId)
    exposureSummary <- read.csv(file.path(indicationFolder,
                                          "pairedExposureSummaryFilteredBySize.csv"))

    redoOutcomes <- function(i) {
        useSample <- FALSE
        targetId <- exposureSummary$targetId[i]
        comparatorId <- exposureSummary$comparatorId[i]
        folderName <- file.path(indicationFolder,
                                "cmOutput",
                                paste0("CmData_l1_t", targetId, "_c", comparatorId))
        cohorts <- readRDS(file.path(folderName, "cohorts.rds"))
        outcomesFolder <- file.path(indicationFolder, "allOutcomes")
        ParallelLogger::logTrace("Updating ", folderName)

        # Copied directly from CustomCmDataObjectBuilding.R:

        # Subsetting outcomes
        outcomes <- NULL
        ffbase::load.ffdf(dir = outcomesFolder)  # Loads outcomes
        ff::open.ffdf(outcomes, readonly = TRUE)
        idx <- ffbase::`%in%`(outcomes$rowId, ff::as.ff(cohorts$rowId))
        if (ffbase::any.ff(idx)) {
            outcomes <- ff::as.ram(outcomes[idx, ])
        } else {
            outcomes <- as.data.frame(outcomes[1, ])
            outcomes <- outcomes[T == F, ]
        }
        if (!useSample) {
            # Add injected outcomes (no signal injection when doing sampling)
            injectionSummary <- read.csv(file.path(indicationFolder, "signalInjectionSummary.csv"),
                                         stringsAsFactors = FALSE)
            injectionSummary <- injectionSummary[injectionSummary$exposureId == targetId |
                                                     injectionSummary$exposureId == comparatorId, ]
            injectionSummary <- injectionSummary[injectionSummary$outcomesToInjectFile != "", ]

            if (nrow(injectionSummary) > 0) {
                # Add original (background) negative control outcomes
                bgOutcomes <- merge(outcomes, injectionSummary[, c("outcomeId", "newOutcomeId")])
                bgOutcomes$outcomeId <- bgOutcomes$newOutcomeId
                outcomes <- rbind(outcomes, bgOutcomes[, colnames(outcomes)])

                # Add additional outcomes
                synthOutcomes <- lapply(injectionSummary$outcomesToInjectFile, readRDS)
                synthOutcomes <- do.call("rbind", synthOutcomes)
                colnames(synthOutcomes)[colnames(synthOutcomes) == "cohortStartDate"] <- "eventDate"
                colnames(synthOutcomes)[colnames(synthOutcomes) == "cohortDefinitionId"] <- "outcomeId"
                synthOutcomes <- merge(synthOutcomes, cohorts[, c("rowId", "subjectId", "cohortStartDate")])
                synthOutcomes$daysToEvent <- synthOutcomes$eventDate - synthOutcomes$cohortStartDate
                outcomes <- rbind(outcomes, synthOutcomes[, colnames(outcomes)])
            }
        }
        metaData <- data.frame(outcomeIds = unique(outcomes$outcomeId))
        attr(outcomes, "metaData") <- metaData

        saveRDS(outcomes, file.path(folderName, "outcomes.rds"))
        return(NULL)
    }

    plyr::l_ply(1:nrow(exposureSummary), redoOutcomes, .progress = "text")
    ParallelLogger::logInfo("Done updating CohortMethodData objects")
}

deleteSignalInjectionFiles <- function() {
    ParallelLogger::logInfo("Deleting signal injection files")
    indicationFolder <- file.path(outputFolder, indicationId)
    signalInjectionFolder <- file.path(indicationFolder, "signalInjection")
    injectedOutcomesFolder <- file.path(indicationFolder, "injectedOutcomes")
    summaryFile <- file.path(indicationFolder, "signalInjectionSummary.csv")

    ParallelLogger::logTrace("Deleting ", signalInjectionFolder)
    unlink(signalInjectionFolder, recursive = TRUE)

    ParallelLogger::logTrace("Deleting ", injectedOutcomesFolder)
    unlink(injectedOutcomesFolder, recursive = TRUE)

    ParallelLogger::logTrace("Deleting ", summaryFile)
    unlink(summaryFile)

    summaryFile <- file.path(indicationFolder, "signalInjectionSummary.csv")
    injectionSummary <- read.csv(summaryFile)
    for (outcomeId in injectionSummary$newOutcomeId) {
        ParallelLogger::logDebug("Deleting CohortMethod files for positive control outcome ", outcomeId)
        files <- list.files(cmOutputFolder,
                            pattern = paste0("_o", outcomeId),
                            full.names = TRUE,
                            recursive = TRUE)
        unlink(files)
    }

    ParallelLogger::logInfo("Done deleting signal injection files")
}

# This function just reruns the signal injection
rerunSignalInjection <- function() {
    ParallelLogger::logInfo("Rerunning signal injection")
    synthesizePositiveControls(connectionDetails = connectionDetails,
                               cdmDatabaseSchema = cdmDatabaseSchema,
                               oracleTempSchema = oracleTempSchema,
                               cohortDatabaseSchema = cohortDatabaseSchema,
                               tablePrefix = tablePrefix,
                               indicationId = indicationId,
                               outputFolder = outputFolder,
                               maxCores = maxCores)
    ParallelLogger::logInfo("Done rerunning  signal injection")
}

# Deletes files generated by CohortMethod specifically for the outcomes
deleteCohortMethodObjectsForOutcomes <- function(outcomeIds) {
    ParallelLogger::logInfo("Deleting CohortMethod files specific to outcomes")
    indicationFolder <- file.path(outputFolder, indicationId)
    cmOutputFolder <- file.path(indicationFolder, "cmOutput")
    for (outcomeId in outcomeIds) {
        ParallelLogger::logDebug("Deleting files for outcome ", outcomeId)
        files <- list.files(cmOutputFolder,
                            pattern = paste0("_o", outcomeId),
                            full.names = TRUE,
                            recursive = TRUE)
        unlink(files)
    }
    ParallelLogger::logInfo("Done deleting CohortMethod files specific to outcomes")
}

# Deletes files generated by CohortMethod specifically for the outcomes
deleteBalanceObjectsForOutcomes <- function(outcomeIds) {
    ParallelLogger::logInfo("Deleting Balance files specific to outcomes")
    indicationFolder <- file.path(outputFolder, indicationId)
    balanceFolder <- file.path(indicationFolder, "balance")
    if (file.exists(balanceFolder)) {
        for (outcomeId in outcomeIds) {
            ParallelLogger::logDebug("Deleting files for outcome ", outcomeId)
            files <- list.files(balanceFolder,
                                pattern = paste0("_o", outcomeId),
                                full.names = TRUE,
                                recursive = TRUE)
            unlink(files)
        }
    }
    ParallelLogger::logInfo("Done deleting balance files specific to outcomes")
}

# Deletes files generated by CohortMethod specifically for the subgroups
deleteCohortMethodObjectsForSubgroups <- function(subgroupIds) {
    ParallelLogger::logInfo("Deleting CohortMethod files specific to subgroups")
    indicationFolder <- file.path(outputFolder, indicationId)
    cmOutputFolder <- file.path(indicationFolder, "cmOutput")
    cmAnalysisListFile <- system.file("settings",
                                      sprintf("cmAnalysisListInteractions%s.json", indicationId),
                                      package = "Legend")
    cmAnalysisListInteractions <- CohortMethod::loadCmAnalysisList(cmAnalysisListFile)
    analysisIds <- c()
    for (cmAnalysisListInteraction in cmAnalysisListInteractions) {
        if (any(subgroupIds %in% cmAnalysisListInteraction$fitOutcomeModelArgs$interactionCovariateIds)) {
            analysisIds <- c(analysisIds, cmAnalysisListInteraction$analysisId)
        }
    }
    for (analysisId in analysisIds) {
        ParallelLogger::logDebug("Deleting files for analysis ", analysisId)
        # Only need to delete outcome model files, because that is the first part of the analysis pipeline
        # where subgroups come in:
        folderName <- file.path(cmOutputFolder, paste0("Analysis_", analysisId))
        unlink(folderName, recursive = TRUE)
    }
    ParallelLogger::logDebug("Deleting all prefilter folders")
    files <- list.files(cmOutputFolder,
                        pattern = "Prefilter_",
                        full.names = TRUE,
                        include.dirs = TRUE)
    unlink(files, recursive = TRUE)
    ParallelLogger::logInfo("Done deleting CohortMethod files specific to outcomes")
}

deleteAllCohortMethodData <- function() {
    ParallelLogger::logInfo("Deleting all CohortMethodData objects")
    indicationFolder <- file.path(outputFolder, indicationId)
    cmOutputFolder <- file.path(indicationFolder, "cmOutput")
    files <- list.files(cmOutputFolder, pattern = "CmData_", full.names = TRUE, include.dirs = TRUE)
    unlink(files, recursive = TRUE)
    ParallelLogger::logInfo("Done deleting all CohortMethodData objects")
}

regenerateAllCohortMethodData <- function() {
    generateAllCohortMethodDataObjects(outputFolder = outputFolder,
                                       indicationId = indicationId,
                                       maxCores = maxCores)
}


# Adding race subgroup to existing run ---------------------------------------------------
# subgroupIds <- c(8998)
# ParallelLogger::addDefaultFileLogger(file.path(outputFolder, indicationId, "logHotSwap.txt"))
# fetchSubgroupCovarsToAllCovariates(subgroupIds)
# deleteAllCohortMethodData()
# regenerateAllCohortMethodData()


# Add and modify several HOIs ------------------------------------------------------------
ParallelLogger::addDefaultFileLogger(file.path(outputFolder, indicationId, "logHotSwapHois.txt"))
outcomeIds <- c(35, 37, 39, 42, 72)
rerunOutcomesOnServer()
fetchOutcomesToAllOutcomes(outcomeIds)
updateCohortMethodDataOutcomes()
deleteCohortMethodObjectsForOutcomes(outcomeIds)
deleteBalanceObjectsForOutcomes(outcomeIds)
unlink(file.path(outputFolder, "export"), recursive = TRUE)
unlink(file.path(outputFolder, "chronographData.csv"))
unlink(file.path(outputFolder, "incidence.csv"))


# Code to run ------------------------------------------------------------------------------
# This assumes the inst/settings/OutcomesOfInterest.csv, R/SubgroupCovariateBuilder.R,
# inst/cohorts, and inst/sql have already been updated
# outcomeIds <- c(18)  # Outcome IDs that are new (both completely new or those that replace older definitions)
# subgroupIds <- c()  # Subgroup IDs that are new (both completely new or those that replace older definitions)
# negativeControlsChanged <- FALSE
# updateCohortMethodDataOutcomes()
#
#
#
#
# if (length(outcomeIds) > 0) {
#
#     rerunOutcomesOnServer()
#
#     fetchOutcomesToAllOutcomes(outcomeIds)
#
#     deleteCohortMethodObjectsForOutcomes(outcomeIds)
# }
#
# if (length(subgroupIds) > 0) {
#
#     fetchSubgroupCovarsToAllCovariates(subgroupIds)
#
#
#     deleteCohortMethodObjectsForSubgroups(subgroupIds)
# }
#
# if (negativeControlsChanged) {
#
#     deleteSignalInjectionFiles()
#
#     rerunSignalInjection()
#
# }
#
# if (length(subgroupIds) == 0) {
#
#     updateCohortMethodDataOutcomes()
#
# } else {
#
#     deleteAllCohortMethodData()
#
#     regenerateAllCohortMethodData()
#
# }
#
# # Remember to drop covariate balance data, incidence data, chronograph data, and exported CSV files
# # Then rerun from cohortMethod = TRUE
#







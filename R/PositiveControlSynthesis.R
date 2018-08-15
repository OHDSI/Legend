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

#' Inject outcomes on top of negative controls
#'
#' @details
#' This function injects outcomes on top of negative controls to create controls with predefined relative risks greater than one.
#'
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param tablePrefix          A prefix to be used for all table names created for this study.
#' @param indicationId           A string denoting the indicationId for which the exposure cohorts should be created.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/)
#' @param sampleSize           The maximum sample size to be used to fit the outcome models.
#' @param maxCores             How many parallel cores should be used? If more cores are made available
#'                             this can speed up the analyses.
#'
#' @export
synthesizePositiveControls <- function(connectionDetails,
                                       cdmDatabaseSchema,
                                       cohortDatabaseSchema,
                                       tablePrefix = "legend",
                                       indicationId = "Depression",
                                       oracleTempSchema,
                                       outputFolder,
                                       sampleSize = 100000,
                                       maxCores = 4) {
    ParallelLogger::logInfo("Synthesizing positive controls for: ", indicationId)

    indicationFolder <- file.path(outputFolder, indicationId)
    signalInjectionFolder <- file.path(indicationFolder, "signalInjection")
    if (!file.exists(signalInjectionFolder))
        dir.create(signalInjectionFolder)
    outcomeCohortTable <- paste(tablePrefix, tolower(indicationId), "out_cohort", sep = "_")

    createSignalInjectionDataFiles(indicationFolder, signalInjectionFolder, sampleSize = sampleSize)

    # Get all possible exposure IDs, including ones not found in this database
    # to make sure new outcome IDs translate across databases:
    pathToCsv <- system.file("settings", "ExposuresOfInterest.csv", package = "Legend")
    exposuresOfInterest <- read.csv(pathToCsv)
    exposuresOfInterest <- exposuresOfInterest[exposuresOfInterest$indicationId == indicationId, ]
    exposureIds <- unique(exposuresOfInterest$cohortId)
    if (indicationId == "Hypertension") {
        exposureCombis <- read.csv(file.path(indicationFolder, "exposureCombis.csv"))
        exposureIds <- unique(c(exposureCombis$cohortDefinitionId,
                                exposureCombis$exposureId1,
                                exposureCombis$exposureId2))
    }
    exposureIds <- exposureIds[order(exposureIds)]

    pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Legend")
    negativeControls <- read.csv(pathToCsv)
    negativeControls <- negativeControls[negativeControls$indicationId == indicationId, ]
    negativeControlIds <- negativeControls$cohortId
    exposureOutcomePairs <- data.frame(exposureId = rep(exposureIds, each = length(negativeControlIds)),
                                       outcomeId = rep(negativeControlIds, length(exposureIds)))

    pathToCsv <- system.file("settings", "Indications.csv", package = "Legend")
    indications <- read.csv(pathToCsv)
    positiveControlIdOffset <- indications$positiveControlIdOffset[indications$indicationId == indicationId]

    # Create a dummy exposure table:
    ParallelLogger::logTrace("Create a dummy exposure table")
    conn <- DatabaseConnector::connect(connectionDetails)
    sql <- "IF OBJECT_ID('@cohort_database_schema.@table_prefix_dummy', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@table_prefix_dummy;
    CREATE TABLE @cohort_database_schema.@table_prefix_dummy (cohort_definition_id BIGINT, subject_id BIGINT, cohort_start_date DATE, cohort_end_date DATE);"
    sql <- SqlRender::renderSql(sql,
                                cohort_database_schema = cohortDatabaseSchema,
                                table_prefix = tablePrefix)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)
    DatabaseConnector::disconnect(conn)

    #exposureOutcomePairs <- exposureOutcomePairs[1,]
    ParallelLogger::logTrace("Calling injectSignals function")
    summ <- MethodEvaluation::injectSignals(connectionDetails = connectionDetails,
                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                            oracleTempSchema = cdmDatabaseSchema,
                                            outcomeDatabaseSchema = cohortDatabaseSchema,
                                            outcomeTable = outcomeCohortTable,
                                            exposureDatabaseSchema = cohortDatabaseSchema,
                                            exposureTable = paste(tablePrefix, "dummy", sep = "_"),
                                            outputDatabaseSchema = cohortDatabaseSchema,
                                            outputTable = outcomeCohortTable,
                                            createOutputTable = FALSE,
                                            exposureOutcomePairs = exposureOutcomePairs,
                                            modelType = "survival",
                                            minOutcomeCountForModel = 100,
                                            minOutcomeCountForInjection = 25,
                                            prior = Cyclops::createPrior("laplace", exclude = 0, useCrossValidation = TRUE),
                                            control = Cyclops::createControl(cvType = "auto",
                                                                             startingVariance = 0.01,
                                                                             tolerance = 2e-07,
                                                                             cvRepetitions = 1,
                                                                             noiseLevel = "silent",
                                                                             threads = min(10, maxCores)),
                                            firstExposureOnly = TRUE,
                                            washoutPeriod = 365,
                                            riskWindowStart = 0,
                                            riskWindowEnd = 99999,
                                            addExposureDaysToEnd = FALSE,
                                            addIntentToTreat = TRUE,
                                            firstOutcomeOnly = TRUE,
                                            removePeopleWithPriorOutcomes = TRUE,
                                            maxSubjectsForModel = 100000,
                                            effectSizes = c(1.5, 2, 4),
                                            precision = 0.01,
                                            outputIdOffset = positiveControlIdOffset,
                                            workFolder = signalInjectionFolder,
                                            cdmVersion = "5",
                                            modelThreads = max(1, round(maxCores/10)),
                                            generationThreads = min(6, maxCores))
    # summ <- read.csv(file.path(indicationFolder, "signalInjectionSummary.csv"))
    write.csv(summ, file.path(indicationFolder, "signalInjectionSummary.csv"), row.names = FALSE)

    counts <- read.csv(file.path(indicationFolder, "outcomeCohortCounts.csv"))
    if (any(counts$cohortDefinitionId >= min(summ$newOutcomeId) & counts$cohortDefinitionId <= max(summ$newOutcomeId))) {
        stop("Collision between original outcome IDs and synthetic outcome IDs")
    }
    conn <- DatabaseConnector::connect(connectionDetails)

    # Only fetch outcomes for subjects in the exposure cohorts, because
    # only those are used in a cohort method design:
    # cohorts <- NULL
    # ffbase::load.ffdf(dir = file.path(indicationFolder, "allCohorts")) # Loads cohorts
    # subjectIds <- ffbase::unique.ff(cohorts$subjectId)

    exposures <- readRDS(file.path(signalInjectionFolder, "exposures.rds"))
    subjectIds <- data.frame(subject_id = unique(exposures$personId))
    # USe non-temp table in case bulk loading is enabled:

    subjectsTableName = paste0(cohortDatabaseSchema, ".temp_subjects_", paste(sample(letters, 5),collapse = ""))
    # sql <- "IF OBJECT_ID('@subjects_table', 'U') IS NOT NULL DROP TABLE @subjects_table;--HINT DISTRIBUTE_ON_KEY(subject_id)
    #   CREATE TABLE @subjects_table (subject_id BIGINT);"
    # sql <- SqlRender::renderSql(sql, subjects_table = subjectsTableName)$sql
    # sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    # DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)
    DatabaseConnector::insertTable(connection = conn,
                                   tableName = subjectsTableName,
                                   data = subjectIds,
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = FALSE,
                                   oracleTempSchema = oracleTempSchema)
    sql <- SqlRender::loadRenderTranslateSql("GetInjectedOutcomes.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             output_database_schema = cohortDatabaseSchema,
                                             output_table = outcomeCohortTable,
                                             min_id = min(summ$newOutcomeId),
                                             max_id = max(summ$newOutcomeId),
                                             subjects_table = subjectsTableName)
    injectedOutcomes <- DatabaseConnector::querySql.ffdf(conn, sql)
    colnames(injectedOutcomes) <- SqlRender::snakeCaseToCamelCase(colnames(injectedOutcomes))

    injectedOutcomesFolder <- file.path(indicationFolder, "injectedOutcomes")
    if (file.exists(injectedOutcomesFolder)) {
        unlink(injectedOutcomesFolder, recursive = TRUE)
    }
    ffbase::save.ffdf(injectedOutcomes, dir = injectedOutcomesFolder)

    # Drop dummy and temp table:
    sql <- "TRUNCATE TABLE @cohort_database_schema.@table_prefix_dummy; DROP TABLE @cohort_database_schema.@table_prefix_dummy;"
    sql <- SqlRender::renderSql(sql,
                                cohort_database_schema = cohortDatabaseSchema,
                                table_prefix = tablePrefix)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

    sql <- "TRUNCATE TABLE @subjects_table; DROP TABLE @subjects_table;"
    sql <- SqlRender::renderSql(sql,
                                subjects_table = subjectsTableName)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

    DatabaseConnector::disconnect(conn)
}

createSignalInjectionDataFiles <- function(indicationFolder, signalInjectionFolder, sampleSize = 100000) {
    # Creating all data files needed by MethodEvaluation::injectSignals from our big data fetch.
    ParallelLogger::logInfo("- Preparing data files")
    # exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))

    # Create exposures file ----------------------------------------------------------
    ParallelLogger::logTrace("Create exposures file")
    exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
    cohortIds <- unique(exposureSummary$targetId, exposureSummary$comparatorId)
    exposures <- plyr::llply(cohortIds,
                             getCohort,
                             exposureSummary = exposureSummary,
                             indicationFolder = indicationFolder,
                             .progress = "text")
    exposures <- readRDS(file.path(indicationFolder, "allCohorts", "allCohorts.rds"))
    # exposures <- data.frame()
    # for (i in 1:nrow(exposureSummary)) {
    #     print(i)
    #     targetId <- exposureSummary$targetId[i]
    #     comparatorId <- exposureSummary$comparatorId[i]
    #     fileName <- file.path(cohortsFolder, paste0("cohorts_t", targetId, "_c", comparatorId))
    #     cohorts <- readRDS(fileName)
    #     # idx <- !(cohorts$rowId %in% exposures$rowId)
    #     idxTarget <- !(cohorts$rowId %in% exposures$rowId[exposures$cohortId == targetId]) & cohorts$treatment == 1
    #     idxComparator <- !(cohorts$rowId %in% exposures$rowId[exposures$cohortId == comparatorId]) & cohorts$treatment == 0
    #     if (any(idxTarget) | any(idxComparator)) {
    #         cohorts$cohortId <- targetId
    #         cohorts$cohortId[cohorts$treatment == 0] <- comparatorId
    #         cohorts$treatment <- NULL
    #         exposures <- rbind(exposures, cohorts[idxTarget | idxComparator, ])
    #     }
    # }
#
#
#     cohorts <- NULL
#     ffbase::load.ffdf(dir = file.path(indicationFolder, "allCohorts")) # Loads cohorts
#     ff::open.ffdf(cohorts)
#     cohortIds <- ff::as.ram(ffbase::unique.ff(cohorts$cohortDefinitionId))
#     dedupe <- function(cohortId, data) {
#         data <- data[data$cohortDefinitionId == cohortId,]
#         data <- ff::as.ram(data)
#         data$targetId <- NULL
#         data$comparatorId <- NULL
#         data <- data[order(data$rowId), ]
#         data <- data[!duplicated(data$rowId), ]
#         return(data)
#     }
#     exposures <- sapply(cohortIds, dedupe, data = cohorts, simplify = FALSE)
#     exposures <- do.call("rbind", exposures)
    exposures$daysToCohortEnd[exposures$daysToCohortEnd > exposures$daysToObsEnd] <- exposures$daysToObsEnd[exposures$daysToCohortEnd > exposures$daysToObsEnd]

    colnames(exposures)[colnames(exposures) == "daysToCohortEnd"] <- "daysAtRisk"
    colnames(exposures)[colnames(exposures) == "daysToObsEnd"] <- "daysObserved"
    # colnames(exposures)[colnames(exposures) == "cohortDefinitionId"] <- "exposureId"
    colnames(exposures)[colnames(exposures) == "cohortId"] <- "exposureId"
    colnames(exposures)[colnames(exposures) == "subjectId"] <- "personId"
    exposures$eraNumber <- 1
    exposures <- exposures[, c("rowId", "exposureId", "personId", "cohortStartDate", "daysAtRisk", "daysObserved", "eraNumber")]
    saveRDS(exposures, file.path(signalInjectionFolder, "exposures.rds"))

    # Create outcomes file ----------------------------------------------------------
    ParallelLogger::logTrace("Create outcomes file")
    outcomes <- NULL
    ffbase::load.ffdf(dir = file.path(indicationFolder, "allOutcomes")) # Loads outcomes
    ff::open.ffdf(outcomes)
    pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Legend")
    negativeControls <- read.csv(pathToCsv)
    negativeControlIds <- negativeControls$cohortId
    negativeControlOutcomes <- outcomes[ffbase::`%in%`(outcomes$outcomeId, negativeControlIds),]
    negativeControlOutcomes <- merge(negativeControlOutcomes, ff::as.ffdf(exposures[, c("rowId", "daysAtRisk", "daysObserved")]))

    dedupeAndCount <- function(outcomeId, data) {
        if (!ffbase::any.ff(data$outcomeId == outcomeId)) {
            return(data.frame())
        }
        data <- ff::as.ram(data[data$outcomeId == outcomeId, ])
        data <- data[data$daysToEvent >= 0 & data$daysToEvent <= data$daysObserved, ]
        if (nrow(data) == 0) {
            return(data.frame())
        }
        data$y <- data$daysToEvent >= 0 & data$daysToEvent <= data$daysAtRisk
        data$yItt <- 1
        y <- aggregate(y ~ rowId, data, sum)
        yItt <- aggregate(yItt ~ rowId, data, sum)
        timeToEvent <- aggregate(daysToEvent ~ rowId, data, min)
        colnames(timeToEvent)[colnames(timeToEvent) == "daysToEvent"] <- "timeToEvent"
        result <- merge(y, timeToEvent)
        result <- merge(yItt, result)
        result$outcomeId <- outcomeId
        return(result)
    }
    outcomes2 <- sapply(negativeControlIds, dedupeAndCount, data = negativeControlOutcomes, simplify = FALSE)
    outcomes2 <- do.call("rbind", outcomes2)
    saveRDS(outcomes2, file.path(signalInjectionFolder, "outcomes.rds"))

    priorOutcomes <- negativeControlOutcomes[negativeControlOutcomes$daysToEvent < 0, c("rowId", "outcomeId")]
    dedupe2 <- function(outcomeId, data) {
        if (!ffbase::any.ff(data$outcomeId == outcomeId)) {
            return(data.frame())
        }
        data <- data[data$outcomeId == outcomeId, ]
        rowIds <- ff::as.ram(ffbase::unique.ff(data$rowId))
        return(data.frame(rowId = rowIds, outcomeId = outcomeId))
    }
    priorOutcomes <- sapply(negativeControlIds, dedupe2, data = priorOutcomes, simplify = FALSE)
    priorOutcomes <- do.call("rbind", priorOutcomes)
    saveRDS(priorOutcomes, file.path(signalInjectionFolder, "priorOutcomes.rds"))

    # Clone covariate data for prediction----------------------------------------------------
    ParallelLogger::logTrace("Clone covariate data for prediction")
    covariatesFolder <- file.path(signalInjectionFolder, "covarsForPrediction_g1")
    if (file.exists(covariatesFolder)) {
        unlink(covariatesFolder, recursive = TRUE)
    }
    covariateData <- FeatureExtraction::loadCovariateData(file.path(indicationFolder, "allCovariates"))
    covariateDataClone <- list(covariates = ff::clone.ffdf(covariateData$covariates),
                               covariateRef = ff::clone.ffdf(covariateData$covariateRef),
                               analysisRef = ff::clone.ffdf(covariateData$analysisRef),
                               metaData = covariateData$metaData)
    class(covariateDataClone) = class(covariateData)
    FeatureExtraction::saveCovariateData(covariateDataClone, covariatesFolder)

    # Sample for model fitting --------------------------------------------------------------
    ParallelLogger::logTrace("Sample for model fitting")
    #sampleSize = 10000
    uniqueGroups <- list(unique(exposures$exposureId))
    saveRDS(uniqueGroups, file.path(signalInjectionFolder, "uniqueGroups.rds"))

    if (nrow(exposures) > sampleSize) {
        sampledRowIds <- sample(exposures$rowId, sampleSize, replace = FALSE)
    } else {
        sampledRowIds <- exposures$rowId
    }
    saveRDS(sampledRowIds, file.path(signalInjectionFolder, "sampledRowIds_g1.rds"))
    sampledRowIds <- ff::as.ff(sampledRowIds)

    covariatesFolder <- file.path(signalInjectionFolder, "covarsForModel_g1")
    if (file.exists(covariatesFolder)) {
        unlink(covariatesFolder, recursive = TRUE)
    }
    covariateData <- FeatureExtraction::loadCovariateData(file.path(indicationFolder, "allCovariates"))
    covariateDataSample <- list(covariates = covariateData$covariates[ffbase::`%in%`(covariateData$covariates$rowId, sampledRowIds)],
                                covariateRef = ff::clone.ffdf(covariateData$covariateRef),
                                analysisRef = ff::clone.ffdf(covariateData$analysisRef),
                                metaData = covariateData$metaData)
    class(covariateDataSample) = class(covariateData)
    FeatureExtraction::saveCovariateData(covariateDataSample, covariatesFolder)
}

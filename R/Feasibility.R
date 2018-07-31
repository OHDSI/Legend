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

#' Assess propensity models
#'
#' @details
#' This function will sample the exposure cohorts, and fit propensity models to identify issues. Assumes
#' the exposure and outcome cohorts have already been created.
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
#' @param indicationId          A string denoting the indicationId for which the exposure cohorts should be created.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/)
#' @param sampleSize           What is the maximum sample size across exposure cohorts?
#' @param minCellCount         The minimum cell count for fields contains person counts or fractions.
#' @param databaseId           A short string for identifying the database (e.g. 'Synpuf').
#'
#' @export
assessPhenotypes <- function(connectionDetails,
                             cdmDatabaseSchema,
                             cohortDatabaseSchema,
                             tablePrefix = "legend",
                             indicationId = "Depression",
                             oracleTempSchema,
                             outputFolder,
                             sampleSize = 100000,
                             minCellCount = 5,
                             databaseId) {
    indicationFolder <- file.path(outputFolder, indicationId)
    if (!file.exists(indicationFolder)) {
        dir.create(indicationFolder, recursive = TRUE)
    }
    assessmentExportFolder <- file.path(indicationFolder, "assessmentOfPhenotypes")
    if (!file.exists(assessmentExportFolder)) {
        dir.create(assessmentExportFolder, recursive = TRUE)
    }
    ParallelLogger::addDefaultFileLogger(file.path(indicationFolder, "logAssesPhenotypes.txt"))

    createExposureCohorts(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          tablePrefix = tablePrefix,
                          indicationId = indicationId,
                          oracleTempSchema = oracleTempSchema,
                          outputFolder = outputFolder)
    exposureSummary <- read.csv(file.path(outputFolder, indicationId, "pairedExposureSummary.csv"))
    exposureSummary$targetPersons[exposureSummary$targetPersons < minCellCount] <- paste0("<", minCellCount)
    exposureSummary$comparatorPersons[exposureSummary$comparatorPersons < minCellCount] <- paste0("<", minCellCount)
    exposureSummary$targetPairedPersons[exposureSummary$targetPairedPersons < minCellCount] <- paste0("<", minCellCount)
    exposureSummary$comparatorPairedPersons[exposureSummary$comparatorPairedPersons < minCellCount] <- paste0("<", minCellCount)
    exposureSummary$indicationId <- indicationId
    exposureSummary$databaseId <- databaseId
    write.csv(exposureSummary, file.path(assessmentExportFolder, "exposurePairs.csv"), row.names = FALSE)

    createOutcomeCohorts(connectionDetails = connectionDetails,
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         tablePrefix = tablePrefix,
                         indicationId = indicationId,
                         oracleTempSchema = oracleTempSchema,
                         outputFolder = outputFolder)
    outcomeCounts <- read.csv(file.path(outputFolder, indicationId, "outcomeCohortCounts.csv"))
    outcomeCounts$count[outcomeCounts$count < minCellCount] <- paste0("<", minCellCount)
    outcomeCounts$indicationId <- indicationId
    outcomeCounts$databaseId <- databaseId
    write.csv(outcomeCounts, file.path(assessmentExportFolder, "outcomes.csv"), row.names = FALSE)


    ParallelLogger::logInfo("Sampling cohorts for subgroup feasibility")
    pairedCohortTable <- paste(tablePrefix, tolower(indicationId), "pair_cohort", sep = "_")
    smallSampleTable <- paste(tablePrefix, tolower(indicationId), "small_sample", sep = "_")
    sql <- SqlRender::loadRenderTranslateSql("SampleCohortsForSubgroupFeasibility.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             paired_cohort_table = pairedCohortTable,
                                             small_sample_table = smallSampleTable,
                                             sample_size = sampleSize)
    conn <- DatabaseConnector::connect(connectionDetails)
    DatabaseConnector::executeSql(conn, sql)

    subgroupCovariateSettings <- createSubgroupCovariateSettings()
    subgroupCovs <- FeatureExtraction::getDbCovariateData(connection = conn,
                                                          oracleTempSchema = oracleTempSchema,
                                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                                          cohortTable = smallSampleTable,
                                                          cohortTableIsTemp = FALSE,
                                                          covariateSettings = subgroupCovariateSettings,
                                                          aggregated = FALSE)
    DatabaseConnector::disconnect(conn)
    covs <- ff::as.ram(subgroupCovs$covariates)
    covs <- aggregate(covariateValue ~ covariateId, covs, sum)
    covs <- merge(covs, data.frame(covariateId = ff::as.ram(subgroupCovs$covariateRef$covariateId),
                                   covariateName = ff::as.ram(subgroupCovs$covariateRef$covariateName)))

    covs$fraction <- round(covs$covariateValue / subgroupCovs$metaData$populationSize, 3)
    idx <- covs$covariateValue < minCellCount
    covs$fraction[idx] <- paste0("<", round(minCellCount / subgroupCovs$metaData$populationSize, 3))
    covs <- covs[, c("covariateId", "covariateName", "fraction")]
    covs$indicationId <- indicationId
    covs$databaseId <- databaseId
    write.csv(covs, file.path(assessmentExportFolder, "subgroups.csv"), row.names = FALSE)

    zipName <- file.path(assessmentExportFolder, "PhenotypeAssessment.zip")
    files <- list.files(assessmentExportFolder, pattern = ".*\\.csv$")
    oldWd <- setwd(assessmentExportFolder)
    on.exit(setwd(oldWd))
    zip::zip(zipfile = zipName, files = files, recurse = FALSE)
    ParallelLogger::logInfo("Results are ready for sharing at:", zipName)
}

#' Assess propensity models
#'
#' @details
#' This function will sample the exposure cohorts, and fit propensity models to identify issues. Assumes
#' the exposure and outcome cohorts have already been created.
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
#' @param indicationId          A string denoting the indicationId for which the exposure cohorts should be created.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/)
#' @param sampleSize           What is the maximum sample size for each exposure cohort?
#' @param maxCores             How many parallel cores should be used? If more cores are made available
#'                             this can speed up the analyses.
#' @param databaseId           A short string for identifying the database (e.g. 'Synpuf').
#'
#' @export
assessPropensityModels <- function(connectionDetails,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   tablePrefix = "legend",
                                   indicationId = "Depression",
                                   oracleTempSchema,
                                   outputFolder,
                                   sampleSize = 1000,
                                   maxCores = 4,
                                   databaseId) {
    pairedCohortTable <- paste(tablePrefix, tolower(indicationId), "pair_cohort", sep = "_")
    sampledCohortTable <- paste(tablePrefix, tolower(indicationId), "sample_cohort", sep = "_")
    indicationFolder <- file.path(outputFolder, indicationId)
    assessmentExportFolder <- file.path(indicationFolder, "assessmentOfPropensityScores")
    if (!file.exists(assessmentExportFolder)) {
        dir.create(assessmentExportFolder, recursive = TRUE)
    }

    ParallelLogger::logInfo("Sampling cohorts for propensity model feasibility")
    sql <- SqlRender::loadRenderTranslateSql("SampleCohortsForPsFeasibility.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             paired_cohort_table = pairedCohortTable,
                                             sampled_cohort_table = sampledCohortTable,
                                             sample_size = sampleSize)
    conn <- DatabaseConnector::connect(connectionDetails)
    DatabaseConnector::executeSql(conn, sql)
    DatabaseConnector::disconnect(conn)

    filterByExposureCohortsSize(outputFolder = outputFolder,
                                indicationId = indicationId)

    fetchAllDataFromServer(connectionDetails = connectionDetails,
                           cdmDatabaseSchema = cdmDatabaseSchema,
                           oracleTempSchema = oracleTempSchema,
                           cohortDatabaseSchema = cohortDatabaseSchema,
                           tablePrefix = tablePrefix,
                           indicationId = indicationId,
                           outputFolder = outputFolder,
                           useSample = TRUE)

    generateAllCohortMethodDataObjects(outputFolder = outputFolder,
                                       indicationId = indicationId,
                                       useSample = TRUE)

    ParallelLogger::logInfo("Fitting propensity models on sampled data")
    fitPsModel <- function(i, exposureSummary, psCvThreads, indicationFolder) {
        targetId <- exposureSummary$targetId[i]
        comparatorId <- exposureSummary$comparatorId[i]
        folderName <- file.path(indicationFolder, "cmSampleOutput", paste0("CmData_l1_t", targetId, "_c", comparatorId))
        cmData <- CohortMethod::loadCohortMethodData(folderName)
        studyPop <- CohortMethod::createStudyPopulation(cohortMethodData = cmData,
                                                        removeDuplicateSubjects = "keep first",
                                                        minDaysAtRisk = 0)
        ps <- CohortMethod::createPs(cohortMethodData = cmData,
                                     population = studyPop,
                                     errorOnHighCorrelation = TRUE,
                                     stopOnError = FALSE,
                                     control = Cyclops::createControl(noiseLevel = "silent",
                                                                      cvType = "auto",
                                                                      tolerance = 2e-07,
                                                                      cvRepetitions = 1,
                                                                      startingVariance = 0.01,
                                                                      seed = 123,
                                                                      threads = psCvThreads))
        fileName <- file.path(indicationFolder, "cmSampleOutput", paste0("Ps_t", targetId, "_c", comparatorId, ".rds"))
        saveRDS(ps, fileName)
        return(NULL)
    }
    createPsThreads <- max(1, round(maxCores/10))
    psCvThreads <- min(10, maxCores)
    exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
    cluster <- ParallelLogger::makeCluster(createPsThreads)
    ParallelLogger::clusterApply(cluster = cluster,
                                 fun = fitPsModel,
                                 x = 1:nrow(exposureSummary),
                                 exposureSummary = exposureSummary,
                                 psCvThreads = psCvThreads,
                                 indicationFolder = indicationFolder)
    ParallelLogger::stopCluster(cluster)

    ParallelLogger::logInfo("Fetching propensity models")
    getModel <- function(i, exposureSummary, indicationFolder) {
        targetId <- exposureSummary$targetId[i]
        comparatorId <- exposureSummary$comparatorId[i]
        psFileName <- file.path(indicationFolder, "cmSampleOutput", paste0("Ps_t", targetId, "_c", comparatorId, ".rds"))
        if (file.exists(psFileName)) {
            ps <- readRDS(psFileName)
            metaData <- attr(ps, "metaData")
            if (is.null(metaData$psError)) {
                folderName <- file.path(indicationFolder, "cmSampleOutput", paste0("CmData_l1_t", targetId, "_c", comparatorId))
                cmData <- CohortMethod::loadCohortMethodData(folderName)
                model <- CohortMethod::getPsModel(ps, cmData)
                ff::close.ffdf(cmData$covariates)
                ff::close.ffdf(cmData$covariateRef)
                ff::close.ffdf(cmData$analysisRef)
                # Truncate to first 25 covariates:
                if (nrow(model) > 25) {
                    model <- model[1:25, ]
                }
            } else if (!is.null(metaData$psHighCorrelation)) {
                model <- data.frame(coefficient = Inf,
                                    covariateId = metaData$psHighCorrelation$covariateId,
                                    covariateName = metaData$psHighCorrelation$covariateName)
            } else {
                model <- data.frame(coefficient = NA,
                                    covariateId = NA,
                                    covariateName = paste("Error:", metaData$psError))
            }
            targetName <- exposureSummary$targetName[i]
            comparatorName <- exposureSummary$comparatorName[i]
            model$targetId <- targetId
            model$targetName <- targetName
            model$comparatorId <- comparatorId
            model$comparatorName <- comparatorName
            model$comparison <- paste(targetName, comparatorName, sep = " vs. ")
            return(model)
        }
        return(NULL)
    }

    data <- plyr::llply(1:nrow(exposureSummary), getModel, exposureSummary = exposureSummary, indicationFolder = indicationFolder, .progress = "text")
    data <- do.call("rbind", data)
    data$databaseId <- databaseId
    data$indicationId <- indicationId
    write.csv(data, file.path(assessmentExportFolder, "propensityModels.csv"), row.names = FALSE)

    ParallelLogger::logInfo("Computing AUCs")
    getAuc <- function(i, exposureSummary, indicationFolder) {
        targetId <- exposureSummary$targetId[i]
        comparatorId <- exposureSummary$comparatorId[i]
        psFileName <- file.path(indicationFolder, "cmSampleOutput", paste0("Ps_t", targetId, "_c", comparatorId, ".rds"))
        if (file.exists(psFileName)) {
            ps <- readRDS(psFileName)
            targetName <- exposureSummary$targetName[i]
            comparatorName <- exposureSummary$comparatorName[i]
            auc <- data.frame(auc = CohortMethod::computePsAuc(ps),
                              targetId = targetId,
                              targetName = targetName,
                              comparatorId = comparatorId,
                              comparatorName = comparatorName,
                              comparison = paste(targetName, comparatorName, sep = " vs. "))
            return(auc)
        }
        return(NULL)
    }

    data <- plyr::llply(1:nrow(exposureSummary), getAuc, exposureSummary = exposureSummary, indicationFolder = indicationFolder, .progress = "text")
    data <- do.call("rbind", data)
    data$databaseId <- databaseId
    data$indicationId <- indicationId
    write.csv(data, file.path(assessmentExportFolder, "aucs.csv"), row.names = FALSE)

    zipName <- file.path(assessmentExportFolder, "PropensityModelAssessment.zip")
    files <- list.files(assessmentExportFolder, pattern = ".*\\.csv$")
    oldWd <- setwd(assessmentExportFolder)
    on.exit(setwd(oldWd))
    zip::zip(zipfile = zipName, files = files, recurse = FALSE)
    ParallelLogger::logInfo("Results are ready for sharing at:", zipName)
}

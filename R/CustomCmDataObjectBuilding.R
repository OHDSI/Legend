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

#' Fetch all data on the cohorts for analysis
#'
#' @details
#' This function will create covariates and fetch outcomes and person information from the server.
#'
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param outputFolder         Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param studyCohortTable     The name of the study cohort table  in the work database schema.
#' @param exposureCohortSummaryTable     The name of the exposure summary table in the work database schema.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder           Name of local folder to place results; make sure to use forward slashes
#'                             (/)
#'
#' @export
fetchAllDataFromServer <- function(connectionDetails,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   tablePrefix = "legend",
                                   indication = "Depression",
                                   oracleTempSchema,
                                   outputFolder) {
    # Some ad-hoc nomenclature:
    #
    # exposureId: Denotes a T or C in a specific TC combination, so filtered to common calendar time
    # cohortId: Denotes a T or C independent of each other
    # exposureConceptId: Denotes the main concept(s) for each cohort. For each combi treatments there are 2 of these.
    # ancestorConceptId: a concept ID explicitly related to a cohort (eequivalent to xposureConceptId).
    # descendantConceptId: a concept ID related to the ancestor concept ID through a custom ancestry
    # filterConceptId: A concept ID that needs to be filtered when fitting propensity model.

    OhdsiRTools::logInfo("Fetching all data from the server")
    indicationFolder <- file.path(outputFolder, indication)
    exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
    exposureIdToCohortId <- rbind(data.frame(exposureId = exposureSummary$tprimeCohortDefinitionId,
                                              cohortId = exposureSummary$tCohortDefinitionId),
                                   data.frame(exposureId = exposureSummary$cprimeCohortDefinitionId,
                                              cohortId = exposureSummary$cCohortDefinitionId))

    counts <- read.csv(file.path(indicationFolder, "outcomeCohortCounts.csv"))
    outcomeIds <- counts$cohortDefinitionId

    conn <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(conn))

    # Lump persons of interest into one table -----------------------------------------------------
    pairedCohortTable <- paste(tablePrefix, tolower(indication), "pair_cohort", sep = "_")
    sql <- SqlRender::loadRenderTranslateSql("UnionExposureCohorts.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             paired_cohort_table = pairedCohortTable,
                                             exposure_ids = exposureIdToCohortId$exposureId)
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

    # Construct covariates ---------------------------------------------------------------------
    pathToCsv <- system.file("settings", "Indications.csv", package = "Legend")
    indications <- read.csv(pathToCsv)
    filterConceptIds <- as.character(indications$filterConceptIds[indications$indication == indication])
    filterConceptIds <- as.numeric(strsplit(filterConceptIds, split = ";")[[1]])
    defaultCovariateSettings <- FeatureExtraction::createDefaultCovariateSettings(excludedCovariateConceptIds = filterConceptIds,
                                                                                  addDescendantsToExclude = TRUE)
    exposureEraTable <- paste(tablePrefix, tolower(indication), "exp_era", sep = "_")
    priorExposureCovariateSettings <- createPriorExposureCovariateSettings(cohortDatabaseSchema = cohortDatabaseSchema,
                                                                           exposureEraTable = exposureEraTable)
    subgroupCovariateSettings <- createSubgroupCovariateSettings()
    covariateSettings <- list(priorExposureCovariateSettings, subgroupCovariateSettings, defaultCovariateSettings)
    # covariateSettings <- list(priorExposureCovariateSettings, defaultCovariateSettings)
    covariates <- FeatureExtraction::getDbCovariateData(connection = conn,
                                                        oracleTempSchema = oracleTempSchema,
                                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                                        cdmVersion = 5,
                                                        cohortTable = "#exposure_cohorts",
                                                        cohortTableIsTemp = TRUE,
                                                        rowIdField = "row_id",
                                                        covariateSettings = covariateSettings,
                                                        aggregated = FALSE)
    FeatureExtraction::saveCovariateData(covariates, file.path(indicationFolder, "allCovariates"))

    # Retrieve cohorts -------------------------------------------------------------------------
    OhdsiRTools::logInfo("Retrieving cohorts")
    sql <- SqlRender::loadRenderTranslateSql("GetExposureCohorts.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             paired_cohort_table = pairedCohortTable)
    cohorts <- DatabaseConnector::querySql.ffdf(conn, sql)
    colnames(cohorts) <- SqlRender::snakeCaseToCamelCase(colnames(cohorts))
    ffbase::save.ffdf(cohorts, dir = file.path(indicationFolder, "allCohorts"))
    ff::close.ffdf(cohorts)

    OhdsiRTools::logInfo("Retrieving outcomes")
    outcomeCohortTable <- paste(tablePrefix, tolower(indication), "out_cohort", sep = "_")
    sql <- SqlRender::loadRenderTranslateSql("GetOutcomes.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             outcome_database_schema = cohortDatabaseSchema,
                                             outcome_table = outcomeCohortTable,
                                             outcome_ids = outcomeIds)
    outcomes <- DatabaseConnector::querySql.ffdf(conn, sql)
    colnames(outcomes) <- SqlRender::snakeCaseToCamelCase(colnames(outcomes))
    ffbase::save.ffdf(outcomes, dir = file.path(indicationFolder, "allOutcomes"))
    ff::close.ffdf(outcomes)

    # Retrieve filter concepts ---------------------------------------------------------
    OhdsiRTools::logInfo("Retrieving filter concepts")
    pathToCsv <- system.file("settings", "ExposuresOfInterest.csv", package = "Legend")
    exposuresOfInterest <- read.csv(pathToCsv)
    exposuresOfInterest <- exposuresOfInterest[exposuresOfInterest$indication == indication, ]
    procedures <- exposuresOfInterest[exposuresOfInterest$type == "Procedure", ]
    ancestor <- data.frame(ancestorConceptId = exposuresOfInterest$conceptId,
                           descendantConceptId = exposuresOfInterest$conceptId)
    for (i in 1:nrow(procedures)) {
        descendantConceptIds <- as.numeric(strsplit(as.character(procedures$includedConceptIds[i]), ";")[[1]])
        ancestor <- rbind(ancestor, data.frame(ancestorConceptId = procedures$conceptId[i],
                                               descendantConceptId = descendantConceptIds))
    }
    sql <- SqlRender::loadRenderTranslateSql("GetFilterConcepts.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             exposure_concept_ids = unique(ancestor$descendantConceptId))
    filterConcepts <- DatabaseConnector::querySql(conn, sql)
    colnames(filterConcepts) <- SqlRender::snakeCaseToCamelCase(colnames(filterConcepts))
    filterConcepts <- merge(ancestor, data.frame(descendantConceptId = filterConcepts$conceptId,
                                                 filterConceptId = filterConcepts$filterConceptId,
                                                 filterConceptName = filterConcepts$filterConceptName))
    exposureCombis <- read.csv(file.path(indicationFolder, "exposureCombis.csv"))
    cohortIdToAncestorIds <- data.frame(cohortId = rep(exposureCombis$cohortDefinitionId, 2),
                                        ancestorConceptId = c(exposureCombis$exposureId1, exposureCombis$exposureId2))
    cohortIdToAncestorIds <- rbind(cohortIdToAncestorIds,
                                   data.frame(cohortId = exposuresOfInterest$conceptId,
                                              ancestorConceptId = exposuresOfInterest$conceptId))

    filterConcepts <- merge(filterConcepts, cohortIdToAncestorIds)
    counts <- read.csv(file.path(indicationFolder, "exposureCohortCounts.csv"))
    filterConcepts <- merge(filterConcepts, data.frame(cohortId = counts$cohortDefinitionId,
                                                       cohortName = counts$cohortName))
    saveRDS(filterConcepts, file.path(indicationFolder, "filterConceps.rds"))
}

constructCohortMethodDataObject <- function(targetId,
                                            comparatorId,
                                            targetConceptId,
                                            comparatorConceptId,
                                            indicationFolder) {
    # Subsetting cohorts
    ffbase::load.ffdf(dir = file.path(indicationFolder, "allCohorts"))
    ff::open.ffdf(cohorts, readonly = TRUE)
    idx <- cohorts$cohortDefinitionId == targetId | cohorts$cohortDefinitionId == comparatorId
    cohorts <- ff::as.ram(cohorts[ffbase::ffwhich(idx, idx == TRUE), ])
    cohorts$treatment <- 0
    cohorts$treatment[cohorts$cohortDefinitionId == targetId] <- 1
    cohorts$cohortDefinitionId <- NULL
    targetPersons <- length(unique(cohorts$subjectId[cohorts$treatment == 1]))
    comparatorPersons <- length(unique(cohorts$subjectId[cohorts$treatment == 0]))
    targetExposures <- length(cohorts$subjectId[cohorts$treatment == 1])
    comparatorExposures <- length(cohorts$subjectId[cohorts$treatment == 0])
    counts <- data.frame(description = "Starting cohorts",
                         targetPersons = targetPersons,
                         comparatorPersons = comparatorPersons,
                         targetExposures = targetExposures,
                         comparatorExposures = comparatorExposures)
    metaData <- list(targetId = targetId,
                     comparatorId = comparatorId,
                     attrition = counts)
    attr(cohorts, "metaData") <- metaData

    # Subsetting outcomes
    ffbase::load.ffdf(dir = file.path(indicationFolder, "allOutcomes"))
    ff::open.ffdf(outcomes, readonly = TRUE)
    idx <- !is.na(ffbase::ffmatch(outcomes$rowId, ff::as.ff(cohorts$rowId)))
    if (ffbase::any.ff(idx)){
        outcomes <- ff::as.ram(outcomes[ffbase::ffwhich(idx, idx == TRUE), ])
    } else {
        outcomes <- as.data.frame(outcomes[1, ])
        outcomes <- outcomes[T == F,]
    }
    # Add injected outcomes
    ffbase::load.ffdf(dir = file.path(indicationFolder, "injectedOutcomes"))
    ff::open.ffdf(injectedOutcomes, readonly = TRUE)
    injectionSummary <- read.csv(file.path(indicationFolder, "signalInjectionSummary.csv"))
    injectionSummary <- injectionSummary[injectionSummary$exposureId %in% c(targetConceptId, comparatorConceptId), ]
    idx1 <- ffbase::'%in%'(injectedOutcomes$subjectId, cohorts$subjectId)
    idx2 <- ffbase::'%in%'(injectedOutcomes$cohortDefinitionId, injectionSummary$newOutcomeId)
    idx <- idx1 & idx2
    if (ffbase::any.ff(idx)){
        injectedOutcomes <- ff::as.ram(injectedOutcomes[idx, ])
        colnames(injectedOutcomes)[colnames(injectedOutcomes) == "cohortStartDate"] <- "eventDate"
        colnames(injectedOutcomes)[colnames(injectedOutcomes) == "cohortDefinitionId"] <- "outcomeId"
        injectedOutcomes <- merge(cohorts[, c("rowId", "subjectId", "cohortStartDate")], injectedOutcomes[, c("subjectId", "outcomeId", "eventDate")])
        injectedOutcomes$daysToEvent = injectedOutcomes$eventDate - injectedOutcomes$cohortStartDate
        #any(injectedOutcomes$daysToEvent < 0)
        #min(outcomes$daysToEvent[outcomes$outcomeId == 73008])
        outcomes <- rbind(outcomes, injectedOutcomes[, c("rowId", "outcomeId", "daysToEvent")])
    }
    metaData <- data.frame(outcomeIds = unique(outcomes$outcomeId))
    attr(outcomes, "metaData") <- metaData

    # Subsetting covariates
    covariateData <- FeatureExtraction::loadCovariateData(file.path(indicationFolder, "allCovariates"))
    idx <- is.na(ffbase::ffmatch(covariateData$covariates$rowId, ff::as.ff(cohorts$rowId)))
    covariates <- covariateData$covariates[ffbase::ffwhich(idx, idx == FALSE), ]

    # Filtering covariates
    filterConcepts <- readRDS(file.path(indicationFolder, "filterConceps.rds"))
    filterConcepts <- filterConcepts[filterConcepts$cohortId %in% c(targetConceptId, comparatorConceptId),]
    filterConceptIds <- unique(filterConcepts$filterConceptId)
    idx <- is.na(ffbase::ffmatch(covariateData$covariateRef$conceptId, ff::as.ff(filterConceptIds)))
    covariateRef <- covariateData$covariateRef[ffbase::ffwhich(idx, idx == TRUE), ]
    filterCovariateIds <- covariateData$covariateRef$covariateId[ffbase::ffwhich(idx, idx == FALSE), ]
    idx <- is.na(ffbase::ffmatch(covariates$covariateId, filterCovariateIds))
    covariates <- covariates[ffbase::ffwhich(idx, idx == TRUE), ]

    result <- list(cohorts = cohorts,
                   outcomes = outcomes,
                   covariates = covariates,
                   covariateRef = covariateRef,
                   analysisRef = ff::clone.ffdf(covariateData$analysisRef),
                   metaData = covariateData$metaData)

    class(result) <- "cohortMethodData"
    return(result)
}

#' Construct all cohortMethodData object
#'
#' @details
#' This function constructs all cohortMethodData objects using the data
#' fetched earlier using the \code{\link{fetchAllDataFromServer}} function.
#'
#' @param outputFolder           Name of local folder to place results; make sure to use forward slashes
#'                             (/)
#'
#' @export
generateAllCohortMethodDataObjects <- function(outputFolder, indication = "Depression") {
    OhdsiRTools::logInfo("Constructing CohortMethodData objects")
    indicationFolder <- file.path(outputFolder, indication)
    start <- Sys.time()
    exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
    pb <- txtProgressBar(style = 3)
    for (i in 1:nrow(exposureSummary)) {
        targetId <- exposureSummary$tprimeCohortDefinitionId[i]
        comparatorId <- exposureSummary$cprimeCohortDefinitionId[i]
        targetConceptId <- exposureSummary$tCohortDefinitionId[i]
        comparatorConceptId <- exposureSummary$cCohortDefinitionId[i]
        folderName <- file.path(indicationFolder, "cmOutput", paste0("CmData_l1_t", targetId, "_c", comparatorId))
        if (!file.exists(folderName)) {
            cmData <- constructCohortMethodDataObject(targetId = targetId,
                                                      comparatorId = comparatorId,
                                                      targetConceptId = targetConceptId,
                                                      comparatorConceptId = comparatorConceptId,
                                                      indicationFolder = indicationFolder)
            CohortMethod::saveCohortMethodData(cmData, folderName)
        }
        setTxtProgressBar(pb, i/nrow(exposureSummary))
    }
    close(pb)
    delta <- Sys.time() - start
    OhdsiRTools::logInfo(paste("Generating all CohortMethodData objects took", signif(delta, 3), attr(delta, "units")))
}

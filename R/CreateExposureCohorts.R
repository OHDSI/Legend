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

#' Create the exposure cohorts
#'
#' @details
#' This function will create the exposure cohorts following the definitions included in this package.
#'
#' @param connectionDetails      An object of type \code{connectionDetails} as created using the
#'                               \code{\link[DatabaseConnector]{createConnectionDetails}} function in
#'                               the DatabaseConnector package.
#' @param cdmDatabaseSchema      Schema name where your patient-level data in OMOP CDM format resides.
#'                               Note that for SQL Server, this should include both the database and
#'                               schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema   Schema name where intermediate data can be stored. You will need to
#'                               have write priviliges in this schema. Note that for SQL Server, this
#'                               should include both the database and schema name, for example
#'                               'cdm_data.dbo'.
#' @param tablePrefix            A prefix to be used for all table names created for this study.
#' @param indicationId           A string denoting the indicationId for which the exposure cohorts
#'                               should be created.
#' @param oracleTempSchema       Should be used in Oracle to specify a schema where the user has write
#'                               priviliges for storing temporary tables.
#' @param outputFolder           Name of local folder to place results; make sure to use forward
#'                               slashes (/)
#'
#' @export
createExposureCohorts <- function(connectionDetails,
                                  cdmDatabaseSchema,
                                  cohortDatabaseSchema,
                                  tablePrefix = "legend",
                                  indicationId = "Depression",
                                  oracleTempSchema,
                                  outputFolder) {
    ParallelLogger::logInfo("Creating exposure cohorts for indicationId: ", indicationId)

    indicationFolder <- file.path(outputFolder, indicationId)
    attritionTable <- paste(tablePrefix, tolower(indicationId), "attrition", sep = "_")
    exposureEraTable <- paste(tablePrefix, tolower(indicationId), "exp_era", sep = "_")
    exposureCohortTable <- paste(tablePrefix, tolower(indicationId), "exp_cohort", sep = "_")
    pairedCohortTable <- paste(tablePrefix, tolower(indicationId), "pair_cohort", sep = "_")
    pairedCohortSummaryTable <- paste(tablePrefix, tolower(indicationId), "pair_sum", sep = "_")

    if (!file.exists(indicationFolder)) {
        dir.create(indicationFolder, recursive = TRUE)
    }
    conn <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(conn))

    # Create empty cohort table ----------------------------------------------------------------------
    sql <- SqlRender::loadRenderTranslateSql("CreateCohortTable.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             cohort_table = exposureCohortTable)
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

    # Load exposures of interest --------------------------------------------------------------------
    pathToCsv <- system.file("settings", "ExposuresOfInterest.csv", package = "Legend")
    exposuresOfInterest <- read.csv(pathToCsv)
    exposuresOfInterest <- exposuresOfInterest[exposuresOfInterest$indicationId == indicationId, ]
    exposuresOfInterest <- exposuresOfInterest[order(exposuresOfInterest$conceptId), ]

    # Create exposure eras and cohorts ------------------------------------------------------
    ParallelLogger::logInfo("- Populating tables ", exposureEraTable, " and ", exposureCohortTable)
    exposureGroupTable <- ""
    exposureCombis <- NULL
    if (indicationId == "Depression") {
        # Upload custom ancestor tables
        subset <- exposuresOfInterest[exposuresOfInterest$includedConceptIds != "", ]
        customAncestor <- data.frame()
        for (i in 1:nrow(subset)) {
            descendantConceptIds <- as.numeric(strsplit(as.character(subset$includedConceptIds[i]), ";")[[1]])
            customAncestor <- rbind(customAncestor, data.frame(ancestorConceptId = subset$cohortId[i],
                                                               descendantConceptId = descendantConceptIds))
        }
        customAncestor <- unique(customAncestor)
        colnames(customAncestor) <- SqlRender::camelCaseToSnakeCase(colnames(customAncestor))
        DatabaseConnector::insertTable(connection = conn,
                                       tableName = "#custom_ancestor",
                                       dropTableIfExists = TRUE,
                                       createTable = TRUE,
                                       tempTable = TRUE,
                                       oracleTempSchema = oracleTempSchema,
                                       data = customAncestor)

        drugCohortIds <- exposuresOfInterest$cohortId[exposuresOfInterest$type == "Drug"]
        procedureCohortIds <- exposuresOfInterest$cohortId[exposuresOfInterest$type == "Procedure"]
        drugClassCohortIds <- exposuresOfInterest$cohortId[exposuresOfInterest$type == "Drug class"]
        drugClassCohortIds <- drugClassCohortIds[!(drugClassCohortIds %in% procedureCohortIds)]
        sql <- SqlRender::loadRenderTranslateSql("CreateExposureCohortsDepression.sql",
                                                 "Legend",
                                                 dbms = connectionDetails$dbms,
                                                 oracleTempSchema = oracleTempSchema,
                                                 cdm_database_schema = cdmDatabaseSchema,
                                                 cohort_database_schema = cohortDatabaseSchema,
                                                 exposure_era_table = exposureEraTable,
                                                 exposure_cohort_table = exposureCohortTable,
                                                 attrition_table = attritionTable,
                                                 drug_cohort_ids = drugCohortIds,
                                                 procedure_cohort_ids = procedureCohortIds,
                                                 drug_class_cohort_ids = drugClassCohortIds,
                                                 custom_ancestor_table = "#custom_ancestor",
                                                 procedure_duration = 30,
                                                 max_gap = 30,
                                                 washout_period = 365)
        DatabaseConnector::executeSql(conn, sql)

        sql <- "TRUNCATE TABLE #custom_ancestor; DROP TABLE #custom_ancestor;"
        sql <- SqlRender::translateSql(sql = sql,
                                       targetDialect = connectionDetails$dbms,
                                       oracleTempSchema = oracleTempSchema)$sql
        DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

        # Upload exposure group table (determines which exposures can be paired)
        exposureGroups <- data.frame(exposureId = exposuresOfInterest$cohortId,
                                     exposureGroup = exposuresOfInterest$type)
        exposureGroups$exposureGroup[exposureGroups$exposureGroup == "Procedure"] <- "Drug"
        colnames(exposureGroups) <- SqlRender::camelCaseToSnakeCase(colnames(exposureGroups))
        DatabaseConnector::insertTable(connection = conn,
                                       tableName = "#exposure_group",
                                       dropTableIfExists = TRUE,
                                       createTable = TRUE,
                                       tempTable = TRUE,
                                       oracleTempSchema = oracleTempSchema,
                                       data = exposureGroups)
        exposureGroupTable <- "#exposure_group"
    } else if (indicationId == "Hypertension") {
        # Create drug and drug class combinations
        drugsOfInterest <- exposuresOfInterest[exposuresOfInterest$type == "Drug", ]
        classesOfInterest <- exposuresOfInterest[exposuresOfInterest$type == "Drug class", ]
        majorClassesOfInterest <- exposuresOfInterest[exposuresOfInterest$type == "Drug major class", ]

        drugPairs <- expand.grid(exposureId1 = drugsOfInterest$cohortId,
                                 exposureId2 = drugsOfInterest$cohortId)
        drugPairs$exposureType <- "Drug"
        classPairs <- expand.grid(exposureId1 = classesOfInterest$cohortId,
                                  exposureId2 = classesOfInterest$cohortId)
        classPairs$exposureType <- "Drug class"
        majorclassPairs <- expand.grid(exposureId1 = majorClassesOfInterest$cohortId,
                                       exposureId2 = majorClassesOfInterest$cohortId)
        majorclassPairs$exposureType <- "Drug major class"

        exposurePairs <- rbind(drugPairs, classPairs, majorclassPairs)
        exposurePairs <- exposurePairs[exposurePairs$exposureId1 < exposurePairs$exposureId2, ]
        exposureCombis <- exposurePairs
        exposureCombis$cohortDefinitionId <- 1000 + 1:nrow(exposureCombis)
        if (any(exposureCombis$cohortDefinitionId %in% exposuresOfInterest$cohortId)) {
            stop("Collision between exposure concept IDs and exposure combination IDs")
        }
        namedExposureCombis <- exposureCombis
        namedExposureCombis$exposureName1 <- exposuresOfInterest$name[match(namedExposureCombis$exposureId1,
                                                                            exposuresOfInterest$cohortId)]
        namedExposureCombis$exposureName2 <- exposuresOfInterest$name[match(namedExposureCombis$exposureId2,
                                                                            exposuresOfInterest$cohortId)]
        namedExposureCombis$cohortName <- paste(namedExposureCombis$exposureName1,
                                                namedExposureCombis$exposureName2,
                                                sep = " & ")
        write.csv(namedExposureCombis, file.path(indicationFolder,
                                                 "exposureCombis.csv"), row.names = FALSE)

        colnames(exposureCombis) <- SqlRender::camelCaseToSnakeCase(colnames(exposureCombis))
        DatabaseConnector::insertTable(connection = conn,
                                       tableName = "#exposure_combi",
                                       dropTableIfExists = TRUE,
                                       createTable = TRUE,
                                       tempTable = TRUE,
                                       oracleTempSchema = oracleTempSchema,
                                       data = exposureCombis)
        colnames(exposureCombis) <- SqlRender::snakeCaseToCamelCase(colnames(exposureCombis))  # Need this later

        # Upload exposures of interest
        exposuresOfInterest$exposureType <- exposuresOfInterest$type
        eoi <- exposuresOfInterest[, c("cohortId", "exposureType")]
        colnames(eoi) <- SqlRender::camelCaseToSnakeCase(colnames(eoi))
        DatabaseConnector::insertTable(connection = conn,
                                       tableName = "#eoi",
                                       dropTableIfExists = TRUE,
                                       createTable = TRUE,
                                       tempTable = TRUE,
                                       oracleTempSchema = oracleTempSchema,
                                       data = eoi)

        # Upload custom drug ancestor table
        classesOfInterest <- exposuresOfInterest[exposuresOfInterest$type == "Drug class" | exposuresOfInterest$type ==
                                                     "Drug major class", ]
        classesOfInterest <- classesOfInterest[, c("cohortId", "includedConceptIds")]
        classesOfInterest <- unique(classesOfInterest)
        drugAncestor <- data.frame()
        for (i in 1:nrow(classesOfInterest)) {
            descendantConceptIds <- as.numeric(strsplit(as.character(classesOfInterest$includedConceptIds[i]),
                                                        ";")[[1]])
            drugAncestor <- rbind(drugAncestor,
                                  data.frame(ancestorConceptId = classesOfInterest$cohortId[i],
                                             descendantConceptId = descendantConceptIds))
        }
        colnames(drugAncestor) <- SqlRender::camelCaseToSnakeCase(colnames(drugAncestor))
        DatabaseConnector::insertTable(connection = conn,
                                       tableName = "#drug_ancestor",
                                       dropTableIfExists = TRUE,
                                       createTable = TRUE,
                                       tempTable = TRUE,
                                       oracleTempSchema = oracleTempSchema,
                                       data = drugAncestor)

        # Create exposure cohorts
        sql <- SqlRender::loadRenderTranslateSql("CreateExposureCohortsHypertension.sql",
                                                 "Legend",
                                                 dbms = connectionDetails$dbms,
                                                 oracleTempSchema = oracleTempSchema,
                                                 cdm_database_schema = cdmDatabaseSchema,
                                                 cohort_database_schema = cohortDatabaseSchema,
                                                 exposure_cohort_table = exposureCohortTable,
                                                 attrition_table = attritionTable,
                                                 drug_ancestor_table = "#drug_ancestor",
                                                 exposure_combi_table = "#exposure_combi",
                                                 eoi_table = "#eoi",
                                                 max_gap = 30,
                                                 washout_period = 365)
        DatabaseConnector::executeSql(conn, sql)

        # Drop temp tables created in R

        sql <- "TRUNCATE TABLE #drug_ancestor; DROP TABLE #drug_ancestor;"
        sql <- SqlRender::translateSql(sql = sql,
                                       targetDialect = connectionDetails$dbms,
                                       oracleTempSchema = oracleTempSchema)$sql
        DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

        sql <- "TRUNCATE TABLE #exposure_combi; DROP TABLE #exposure_combi;"
        sql <- SqlRender::translateSql(sql = sql,
                                       targetDialect = connectionDetails$dbms,
                                       oracleTempSchema = oracleTempSchema)$sql
        DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

        sql <- "TRUNCATE TABLE #eoi; DROP TABLE #eoi;"
        sql <- SqlRender::translateSql(sql = sql,
                                       targetDialect = connectionDetails$dbms,
                                       oracleTempSchema = oracleTempSchema)$sql
        DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

        # Upload exposure group table (determines which exposures can be paired)
        exposureGroups <- rbind(data.frame(exposureId = exposureCombis$cohortDefinitionId,
                                           exposureGroup = exposureCombis$exposureType),
                                data.frame(exposureId = exposuresOfInterest$cohortId,
                                           exposureGroup = exposuresOfInterest$type))
        colnames(exposureGroups) <- SqlRender::camelCaseToSnakeCase(colnames(exposureGroups))
        DatabaseConnector::insertTable(connection = conn,
                                       tableName = "#exposure_group",
                                       dropTableIfExists = TRUE,
                                       createTable = TRUE,
                                       tempTable = TRUE,
                                       oracleTempSchema = oracleTempSchema,
                                       data = exposureGroups)
        exposureGroupTable <- "#exposure_group"

    } else {
        stop("Indication ", indicationId, " not implemented")
    }

    # Create cohort pairs ----------------------------------------------------------------------
    ParallelLogger::logInfo("- Pairing exposure cohorts")
    sql <- SqlRender::loadRenderTranslateSql("CreateCohortPairs.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             exposure_cohort_table = exposureCohortTable,
                                             attrition_table = attritionTable,
                                             paired_cohort_table = pairedCohortTable,
                                             paired_cohort_summary_table = pairedCohortSummaryTable,
                                             exposure_group_table = exposureGroupTable)
    DatabaseConnector::executeSql(conn, sql)

    # Attrition table --------------------------------------------------------------------------------
    ParallelLogger::logInfo("Fetching attrition table")
    sql <- "SELECT * FROM @cohort_database_schema.@attrition_table"
    sql <- SqlRender::renderSql(sql,
                                cohort_database_schema = cohortDatabaseSchema,
                                attrition_table = attritionTable)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    attrition <- DatabaseConnector::querySql(conn, sql)
    names(attrition) <- SqlRender::snakeCaseToCamelCase(names(attrition))
    write.csv(attrition, file.path(indicationFolder, "attrition.csv"), row.names = FALSE)

    # Cohort summary table --------------------------------------------------------------------------
    ParallelLogger::logInfo("Fetching cohort summary table")
    sql <- "SELECT * FROM @cohort_database_schema.@paired_cohort_summary_table"
    sql <- SqlRender::renderSql(sql = sql,
                                cohort_database_schema = cohortDatabaseSchema,
                                paired_cohort_summary_table = pairedCohortSummaryTable)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    pairedExposureSummary <- DatabaseConnector::querySql(conn, sql)
    colnames(pairedExposureSummary) <- SqlRender::snakeCaseToCamelCase(colnames(pairedExposureSummary))
    cohortNames <- data.frame(cohortDefinitionId = exposuresOfInterest$cohortId,
                              cohortName = exposuresOfInterest$name)
    if (!is.null(exposureCombis)) {
        cohortNames <- rbind(cohortNames,
                             data.frame(cohortDefinitionId = namedExposureCombis$cohortDefinitionId,
                                        cohortName = namedExposureCombis$cohortName))
    }
    cohortNames <- unique(cohortNames)
    pairedExposureSummary <- merge(pairedExposureSummary,
                                   data.frame(targetId = cohortNames$cohortDefinitionId,
                                              targetName = cohortNames$cohortName))
    pairedExposureSummary <- merge(pairedExposureSummary,
                                   data.frame(comparatorId = cohortNames$cohortDefinitionId,
                                              comparatorName = cohortNames$cohortName))
    write.csv(pairedExposureSummary, file.path(indicationFolder,
                                               "pairedExposureSummary.csv"), row.names = FALSE)

    # Drop temp tables -----------------------------------------------------------------------
    if (exposureGroupTable != "") {
        sql <- "TRUNCATE TABLE #exposure_group; DROP TABLE #exposure_group;"
        sql <- SqlRender::translateSql(sql = sql,
                                       targetDialect = connectionDetails$dbms,
                                       oracleTempSchema = oracleTempSchema)$sql
        DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)
    }
}

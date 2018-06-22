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
#' This function will create the exposure cohorts following the definitions included in
#' this package.
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
#' @param indication           A string denoting the indication for which the exposure cohorts should be created.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/)
#'
#' @export
createExposureCohorts <- function(connectionDetails,
                                  cdmDatabaseSchema,
                                  cohortDatabaseSchema,
                                  tablePrefix = "legend",
                                  indication = "Depression",
                                  oracleTempSchema,
                                  outputFolder) {
    OhdsiRTools::logInfo("Creating exposure cohorts for indication: ", indication)

    indicationFolder <- file.path(outputFolder, indication)
    exposureEraTable <- paste(tablePrefix, tolower(indication), "exp_era", sep = "_")
    exposureCohortTable <- paste(tablePrefix, tolower(indication), "exp_cohort", sep = "_")
    pairedCohortTable <- paste(tablePrefix, tolower(indication), "pair_cohort", sep = "_")
    pairedCohortSummaryTable <- paste(tablePrefix, tolower(indication), "pair_sum", sep = "_")

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

    # Load exposures of interest, and define exposure combinations of interest ----------------------
    pathToCsv <- system.file("settings", "ExposuresOfInterest.csv", package = "Legend")
    exposuresOfInterest <- read.csv(pathToCsv)
    exposuresOfInterest <- exposuresOfInterest[exposuresOfInterest$indication == indication, ]
    exposuresOfInterest <- exposuresOfInterest[order(exposuresOfInterest$conceptId), ]
    exposureCombis <- expand.grid(exposureId1 = exposuresOfInterest$conceptId, exposureId2 = exposuresOfInterest$conceptId)
    exposureCombis <- exposureCombis[exposureCombis$exposureId1 < exposureCombis$exposureId2, ]
    exposureCombis$cohortDefinitionId <- 1:nrow(exposureCombis)
    if (any(exposureCombis$cohortDefinitionId %in% exposuresOfInterest$conceptId)) {
        stop("Collision between exposure concept IDs and exposure combination IDs")
    }
    namedExposureCombis <- exposureCombis
    namedExposureCombis$exposureName1 <- exposuresOfInterest$name[match( namedExposureCombis$exposureId1, exposuresOfInterest$conceptId)]
    namedExposureCombis$exposureName2 <- exposuresOfInterest$name[match( namedExposureCombis$exposureId2, exposuresOfInterest$conceptId)]
    namedExposureCombis$cohortName <- paste(namedExposureCombis$exposureName1, namedExposureCombis$exposureName2, sep = " - ")
    write.csv(namedExposureCombis, file.path(indicationFolder, "exposureCombis.csv"), row.names = FALSE)

    # Upload combis and procedure groups -----------------------------------------------------------
    colnames(exposureCombis) <- SqlRender::camelCaseToSnakeCase(colnames(exposureCombis))
    DatabaseConnector::insertTable(connection = conn,
                                   tableName = "#exposure_combi",
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   oracleTempSchema = oracleTempSchema,
                                   data = exposureCombis)

    procedures <- exposuresOfInterest[exposuresOfInterest$type == "Procedure", ]
    procedureAncestor <- data.frame()
    for (i in 1:nrow(procedures)) {
        descendantConceptIds <- as.numeric(strsplit(as.character(procedures$includedConceptIds[i]), ";")[[1]])
        procedureAncestor <- rbind(procedureAncestor, data.frame(ancestorConceptId = procedures$conceptId[i],
                                                                 descendantConceptId = descendantConceptIds))
    }
    colnames(procedureAncestor) <- SqlRender::camelCaseToSnakeCase(colnames(procedureAncestor))
    DatabaseConnector::insertTable(connection = conn,
                                   tableName = "#procedure_ancestor",
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   oracleTempSchema = oracleTempSchema,
                                   data = procedureAncestor)

    # Create nesting cohort table ------------------------------------------------------------------------
    OhdsiRTools::logInfo("- Creating nesting cohort")
    sql <- SqlRender::loadRenderTranslateSql(sprintf("NestingCohort%s.sql", indication),
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             nesting_cohort_table = "#nesting_cohort")
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

    # Create exposure eras and cohorts ------------------------------------------------------
    OhdsiRTools::logInfo("- Populating tables ", exposureEraTable, " and ", exposureCohortTable)
    drugConceptIds <- exposuresOfInterest$conceptId[exposuresOfInterest$type == "Drug"]
    sql <- SqlRender::loadRenderTranslateSql("CreateExposureCohorts.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             exposure_era_table = exposureEraTable,
                                             exposure_cohort_table = exposureCohortTable,
                                             drug_concept_ids = drugConceptIds,
                                             include_procedures = TRUE,
                                             procedure_ancestor_table = "#procedure_ancestor",
                                             procedure_duration = 30,
                                             max_gap = 30,
                                             exposure_combi_table = "#exposure_combi",
                                             nesting_cohort_table = "#nesting_cohort",
                                             washout_period = 365)
    DatabaseConnector::executeSql(conn, sql)

    # Count cohorts --------------------------------------------------------------------------------
    OhdsiRTools::logInfo("Counting exposure cohorts")
    sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohort_database_schema.@cohort_table GROUP BY cohort_definition_id"
    sql <- SqlRender::renderSql(sql,
                                cohort_database_schema = cohortDatabaseSchema,
                                cohort_table = exposureCohortTable)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    counts <- DatabaseConnector::querySql(conn, sql)
    names(counts) <- SqlRender::snakeCaseToCamelCase(names(counts))
    countsSingleExposures <- merge(counts, data.frame(cohortDefinitionId = exposuresOfInterest$conceptId,
                                                      cohortName = exposuresOfInterest$name))
    countsCombiExposures <- merge(counts, data.frame(cohortDefinitionId = namedExposureCombis$cohortDefinitionId,
                                                     cohortName = namedExposureCombis$cohortName))
    counts <- rbind(countsSingleExposures, countsCombiExposures)
    write.csv(counts, file.path(indicationFolder, "exposureCohortCounts.csv"), row.names = FALSE)

    # Create cohort pairs ----------------------------------------------------------------------
    OhdsiRTools::logInfo("- Pairing exposure cohorts")
    sql <- SqlRender::loadRenderTranslateSql("CreateCohortPairs.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             exposure_cohort_table = exposureCohortTable,
                                             exposure_combi_table = "#exposure_combi",
                                             paired_cohort_table = pairedCohortTable,
                                             paired_cohort_summary_table = pairedCohortSummaryTable)
    DatabaseConnector::executeSql(conn, sql)

    sql <- "SELECT * FROM @cohort_database_schema.@paired_cohort_summary_table"
    sql <- SqlRender::renderSql(sql = sql,
                                cohort_database_schema = cohortDatabaseSchema,
                                paired_cohort_summary_table = pairedCohortSummaryTable)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    pairedExposureSummary <- DatabaseConnector::querySql(conn, sql)
    colnames(pairedExposureSummary) <- SqlRender::snakeCaseToCamelCase(colnames(pairedExposureSummary))
    pairedExposureSummary <- merge(pairedExposureSummary, data.frame(targetId = counts$cohortDefinitionId,
                                                                     targetName = counts$cohortName))
    pairedExposureSummary <- merge(pairedExposureSummary, data.frame(comparatorId = counts$cohortDefinitionId,
                                                                     comparatorName = counts$cohortName))
    write.csv(pairedExposureSummary, file.path(indicationFolder, "pairedExposureSummary.csv"), row.names = FALSE)

    # Drop temp tables -----------------------------------------------------------------------
    sql <- "TRUNCATE TABLE #procedure_ancestor; DROP TABLE #procedure_ancestor;"
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms, oracleTempSchema = oracleTempSchema)$sql
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

    sql <- "TRUNCATE TABLE #exposure_combi; DROP TABLE #exposure_combi;"
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms, oracleTempSchema = oracleTempSchema)$sql
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

    sql <- "TRUNCATE TABLE #nesting_cohort; DROP TABLE #nesting_cohort;"
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms, oracleTempSchema = oracleTempSchema)$sql
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)
}

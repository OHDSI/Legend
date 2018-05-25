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


#' Create the outcome cohorts
#'
#' @details
#' This function will create the outcome cohorts following the definitions included in
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
createOutcomeCohorts <- function(connectionDetails,
                                  cdmDatabaseSchema,
                                  cohortDatabaseSchema,
                                  tablePrefix = "legend",
                                  indication = "Depression",
                                  oracleTempSchema,
                                  outputFolder) {
    OhdsiRTools::logInfo("Creating outcome cohorts for indication: ", indication)

    indicationFolder <- file.path(outputFolder, indication)
    outcomeCohortTable <- paste(tablePrefix, tolower(indication), "out_cohort", sep = "_")
    if (!file.exists(indicationFolder)) {
        dir.create(indicationFolder, recursive = TRUE)
    }
    conn <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(conn))

    # Creating outcome of interest cohorts ------------------------------------------------------------------
    OhdsiRTools::logInfo("- Creating outcome of interest cohorts")
    .createCohorts(connection = conn,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   cohortDatabaseSchema = cohortDatabaseSchema,
                   cohortTable = outcomeCohortTable,
                   oracleTempSchema = oracleTempSchema,
                   outputFolder = indicationFolder)

    # Creating negative control outcome cohorts ------------------------------------------------------------
    OhdsiRTools::logInfo("- Creating negative control outcome cohorts")
    pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Legend")
    negativeControls <- read.csv(pathToCsv)
    sql <- SqlRender::loadRenderTranslateSql("NegativeControls.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             target_database_schema = cohortDatabaseSchema,
                                             target_cohort_table = outcomeCohortTable,
                                             outcome_ids = negativeControls$conceptId)
    DatabaseConnector::executeSql(conn, sql)

    # Count cohorts --------------------------------------------------------------------------------
    OhdsiRTools::logInfo("Counting outcome cohorts")
    sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohort_database_schema.@cohort_table GROUP BY cohort_definition_id"
    sql <- SqlRender::renderSql(sql,
                                cohort_database_schema = cohortDatabaseSchema,
                                cohort_table = outcomeCohortTable)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    counts <- DatabaseConnector::querySql(conn, sql)
    names(counts) <- SqlRender::snakeCaseToCamelCase(names(counts))

    pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Legend")
    outcomesOfInterest <- read.csv(pathToCsv)

    countsOutcomesOfInterest <- merge(counts, data.frame(cohortDefinitionId = outcomesOfInterest$cohortId,
                                                         cohortName = outcomesOfInterest$name))
    countsNegativeControls <- merge(counts, data.frame(cohortDefinitionId = negativeControls$conceptId,
                                                       cohortName = negativeControls$name))
    counts <- rbind(countsOutcomesOfInterest, countsNegativeControls)
    write.csv(counts, file.path(indicationFolder, "outcomeCohortCounts.csv"), row.names = FALSE)
}

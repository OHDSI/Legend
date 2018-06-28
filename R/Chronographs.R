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

#' Fetch data for constructing chronographs from the server
#'
#' @details
#' Fetch data for constructing chronographs from the server.
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
#'
#' @export
fetchChronographData <- function(connectionDetails,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 tablePrefix = "legend",
                                 indicationId = "Depression",
                                 oracleTempSchema,
                                 outputFolder) {
    OhdsiRTools::logInfo("Fetching chronograph data from the server")
    indicationFolder <- file.path(outputFolder, indicationId)
    exposureCohortTable <- paste(tablePrefix, tolower(indicationId), "exp_cohort", sep = "_")
    outcomeCohortTable <- paste(tablePrefix, tolower(indicationId), "out_cohort", sep = "_")
    pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Legend")
    hois <- read.csv(pathToCsv)
    hois <- hois[hois$indicationId == indicationId, ]
    outcomeIds <- hois$cohortId
    chronographData <- IcTemporalPatternDiscovery::getChronographData(connectionDetails = connectionDetails,
                                                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                                                      oracleTempSchema = oracleTempSchema,
                                                                      exposureIds = c(),
                                                                      outcomeIds = outcomeIds,
                                                                      exposureDatabaseSchema = cohortDatabaseSchema,
                                                                      exposureTable = exposureCohortTable,
                                                                      outcomeDatabaseSchema = cohortDatabaseSchema,
                                                                      outcomeTable = outcomeCohortTable)
    write.csv(chronographData, file.path(indicationFolder, "chronographData.csv"), row.names = FALSE)
}

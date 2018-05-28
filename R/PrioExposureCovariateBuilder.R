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


#' Create settings for adding prior exposures as covariates
#'
#' @details
#' Counts the number of prior treatments.
#'
#' @param cohortDatabaseSchema  The name of the database schema that is the location
#'                               where the data used to define the exposure cohorts is
#'                               available.
#' @param exposureEraTable       The tablename that contains the exposure eras.
#' @param windowStart            Start day of the window where covariates are captured,
#'                               relative to the index date (0 = index date).
#' @param windowEnd              End day of the window where covariates are captured,
#'                               relative to the index date (0 = index date).
#'
#' @return
#' A covariateSettings object.
#'
#' @export
createPriorExposureCovariateSettings <- function(cohortDatabaseSchema = "unknown",
                                                 exposureEraTable = "unknown",
                                                 windowStart = -999,
                                                 windowEnd = -1) {
    covariateSettings <- list(cohortDatabaseSchema = cohortDatabaseSchema,
                              exposureEraTable = exposureEraTable,
                              windowStart = windowStart,
                              windowEnd = windowEnd)
    attr(covariateSettings, "fun") <- "Legend::getDbPriorExposuresCovariateData"
    class(covariateSettings) <- "covariateSettings"
    return(covariateSettings)
}

#' @export
getDbPriorExposuresCovariateData <- function(connection,
                                             oracleTempSchema = NULL,
                                             cdmDatabaseSchema,
                                             cohortTable = "#cohort_person",
                                             cohortId = -1,
                                             cdmVersion = "5",
                                             rowIdField = "subject_id",
                                             covariateSettings,
                                             aggregated = FALSE) {
    if (aggregated)
        stop("Aggregation not supported")
    writeLines("Creating covariates based on prior exposures")
    sql <- SqlRender::loadRenderTranslateSql("GetPriorExposureCovariates.sql",
                                             packageName = "Legend",
                                             dbms = connection@dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             window_start = covariateSettings$windowStart,
                                             window_end = covariateSettings$windowEnd,
                                             row_id_field = rowIdField,
                                             cohort_temp_table = cohortTable,
                                             cohort_id = cohortId,
                                             cohort_database_schema = covariateSettings$cohortDatabaseSchema,
                                             exposure_era_table = covariateSettings$exposureEraTable)
    covariates <- DatabaseConnector::querySql.ffdf(connection, sql)
    colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
    covariateRef <- data.frame(covariateId = c(1999, 2999, 3999, 4999, 5999),
                               covariateName = paste("Prior treatments:", c("1", "2", "3", "4", "5 or more")),
                               analysisId = 999,
                               conceptId = 0)
    covariateRef <- ff::as.ffdf(covariateRef)

    # Construct analysis reference:
    analysisRef <- data.frame(analysisId = as.numeric(1),
                              analysisName = "Prior treatment",
                              domainId = "Cohort",
                              startDay = as.numeric(covariateSettings$windowStart),
                              endDay = as.numeric(covariateSettings$windowEnd),
                              isBinary = "Y",
                              missingMeansZero = "Y")
    analysisRef <- ff::as.ffdf(analysisRef)
    # Construct analysis reference:
    metaData <- list(sql = sql, call = match.call())
    result <- list(covariates = covariates,
                   covariateRef = covariateRef,
                   analysisRef = analysisRef,
                   metaData = metaData)
    class(result) <- "covariateData"
    return(result)
}

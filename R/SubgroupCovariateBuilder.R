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


#' Create settings for adding subgroup markers as covariates
#'
#' @details
#' Creates a marker covariate for each of the subgroups of interest.
#'
#' @param windowStart            Start day of the window where covariates are captured,
#'                               relative to the index date (0 = index date).
#' @param windowEnd              End day of the window where covariates are captured,
#'                               relative to the index date (0 = index date).
#' @param analysisId             A unique identifier for this analysis.
#'
#' @return
#' A covariateSettings object.
#'
#' @export
createSubgroupCovariateSettings <- function(windowStart = -365,
                                            windowEnd = -1,
                                            analysisId = 998) {
    covariateSettings <- list(windowStart = windowStart,
                              windowEnd = windowEnd,
                              analysisId = analysisId)
    attr(covariateSettings, "fun") <- "Legend::getDbSubgroupCovariateData"
    class(covariateSettings) <- "covariateSettings"
    return(covariateSettings)
}

#' @export
getDbSubgroupCovariateData <- function(connection,
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
    writeLines("Creating covariates indicating subgroups of interest")
    sql <- SqlRender::loadRenderTranslateSql("CreateSubgroups.sql",
                                             packageName = "Legend",
                                             dbms = connection@dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             window_start = covariateSettings$windowStart,
                                             window_end = covariateSettings$windowEnd,
                                             analysis_id = covariateSettings$analysisId,
                                             row_id_field = rowIdField,
                                             cohort_temp_table = cohortTable,
                                             cohort_id = cohortId)
    DatabaseConnector::executeSql(connection, sql)

    sql <- SqlRender::loadRenderTranslateSql("GetSubgroups.sql",
                                             packageName = "Legend",
                                             dbms = connection@dbms,
                                             oracleTempSchema = oracleTempSchema)
    covariates <- DatabaseConnector::querySql.ffdf(connection, sql)
    colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))


    sql <- SqlRender::loadRenderTranslateSql("DropSubgroupTempTables.sql",
                                             packageName = "Legend",
                                             dbms = connection@dbms,
                                             oracleTempSchema = oracleTempSchema)
    DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)

    covariateRef <- data.frame(covariateId = c(1000, 2000, 3000, 4000, 5000, 6000, 7000) + covariateSettings$analysisId,
                               covariateName = c("Subgroup: Renal impairment",
                                                 "Subgroup: Hepatic impairment",
                                                 "Subgroup: Pregnant women",
                                                 "Subgroup: Children (age < 18)",
                                                 "Subgroup: Elderly (age >=65)",
                                                 "Subgroup: Gender = female",
                                                 "Subgroup: Type 2 Diabetes Mellitus"),
                               analysisId = as.numeric(covariateSettings$analysisId),
                               conceptId = 0)
    covariateRef <- ff::as.ffdf(covariateRef)

    # Construct analysis reference:
    analysisRef <- data.frame(analysisId = as.numeric(covariateSettings$analysisId),
                              analysisName = "Subgroups of interest",
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

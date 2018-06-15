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

#' Compute incidence rates
#'
#' @details
#' Compute incidence rates using the CohortMethod data files.
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
computeIncidenceRates <- function(connectionDetails,
                                  cdmDatabaseSchema,
                                  cohortDatabaseSchema,
                                  tablePrefix = "legend",
                                  indication = "Depression",
                                  oracleTempSchema,
                                  outputFolder) {
    OhdsiRTools::logInfo("Computing incidence rates based on extracted data")
    indicationFolder <- file.path(outputFolder, indication)
    exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
    ffbase::load.ffdf(file.path(indicationFolder, "allCohorts")) # Loads cohorts ffdf
    ffbase::load.ffdf(file.path(indicationFolder, "allOutcomes")) # Loads outcomes ffdf
    covariateData <- FeatureExtraction::loadCovariateData(file.path(indicationFolder, "allCovariates"))
    ref <- ff::as.ram(covariateData$covariateRef[covariateData$covariateRef$analysisId == 998,])
    covSubset <- ff::as.ram(covariateData$covariates[ffbase::`%in%`(covariateData$covariates$covariateId, ff::as.ff(ref$covariateId)), ])

    cohortIds <- unique(c(exposureSummary$tCohortDefinitionId, exposureSummary$cCohortDefinitionId))
    for (cohortId in cohortIds) {
        minDate <- min(c(as.Date(as.character(exposureSummary$tprimeMinCohortDate[exposureSummary$tCohortDefinitionId == cohortId])),
                         as.Date(as.character(exposureSummary$cprimeMinCohortDate[exposureSummary$cCohortDefinitionId == cohortId]))))
        maxDate <- min(c(as.Date(as.character(exposureSummary$tprimeMaxCohortDate[exposureSummary$tCohortDefinitionId == cohortId])),
                         as.Date(as.character(exposureSummary$cprimeMaxCohortDate[exposureSummary$cCohortDefinitionId == cohortId]))))

        # Find all unique rowIds for this exposure cohort (across all comparisons)
        exposureIds <- unique(c(exposureSummary$tprimeCohortDefinitionId[exposureSummary$tCohortDefinitionId == cohortId],
                                exposureSummary$cprimeCohortDefinitionId[exposureSummary$cCohortDefinitionId == cohortId]))
        subsetRowIds <- cohorts$rowId[ffbase::`%in%`(cohorts$cohortDefinitionId, ff::as.ff(exposureIds))]
        subsetRowIds <- ffbase::unique.ff(subsetRowIds)

        # Subset cohort, outcomes, covariates (for subgroups)
        subsetCohort <- cohorts[ffbase::ffmatch(subsetRowIds, cohorts$rowId), ]
        subsetOutcomes <- outcomes[ffbase::`%in%`(outcomes$rowId, subsetRowIds), ]
        idx <- ffbase::`%in%`(covariateData$covariates$covariateId, ff::as.ff(ref$covariateId))
        idx[idx] <- ffbase::`%in%`(covariates$rowId[idx], subsetRowIds)
        subsetCovariates <- covariates[idx, ]


    }
}

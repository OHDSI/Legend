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

#' Compute incidence
#'
#' @details
#' Compute incidence using the CohortMethod data files.
#'
#' @param indicationId         A string denoting the indicationId for which the exposure cohorts should be created.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/)
#'
#' @export
computeIncidence <- function(indicationId = "Depression",
                             outputFolder) {
    ParallelLogger::logInfo("Computing incidence rates based on extracted data")
    indicationFolder <- file.path(outputFolder, indicationId)
    exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
    pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Legend")
    outcomesOfInterest <- read.csv(pathToCsv)
    # cohorts <- NULL
    outcomes <- NULL
    # ffbase::load.ffdf(file.path(indicationFolder, "allCohorts")) # Loads cohorts ffdf
    ffbase::load.ffdf(file.path(indicationFolder, "allOutcomes")) # Loads outcomes ffdf
    # ff::open.ffdf(cohorts, readonly = TRUE)
    ff::open.ffdf(outcomes, readonly = TRUE)
    on.exit({
        # ff::close.ffdf(cohorts)
        ff::close.ffdf(outcomes)
    })
    outcomes <- outcomes[ffbase::`%in%`(outcomes$outcomeId, outcomesOfInterest$cohortId), ]
    covariateData <- FeatureExtraction::loadCovariateData(file.path(indicationFolder, "allCovariates"))
    ref <- ff::as.ram(covariateData$covariateRef[covariateData$covariateRef$analysisId == 998,])
    covSubset <- covariateData$covariates[ffbase::`%in%`(covariateData$covariates$covariateId, ff::as.ff(ref$covariateId)), ]
    cohortIds <- unique(c(exposureSummary$targetId, exposureSummary$comparatorId))
    computeCohortIrs <- function(cohortId) {
        # Find all unique rowIds for this exposure cohort (across all comparisons)
        # subsetRowIds <- cohorts$rowId[cohorts$cohortDefinitionId == cohortId]
        # subsetRowIds <- ffbase::unique.ff(subsetRowIds)
        subsetCohort <- getCohort(cohortId, exposureSummary, indicationFolder)

        # Subset cohort, outcomes, covariates
        # subsetCohort <- ff::as.ram(cohorts[ffbase::ffmatch(subsetRowIds, cohorts$rowId), ])
        subsetOutcomes <- ff::as.ram(outcomes[ffbase::`%in%`(outcomes$rowId, subsetCohort$rowId), ])
        subsetCovariates <- ff::as.ram(covSubset[ffbase::`%in%`(covSubset$rowId, subsetCohort$rowId), ])

        # Compute overall and per-subgroup IRs
        irs <- computeIrs(subsetCohort, subsetOutcomes)
        irs$interactionCovariateId <- NA
        subgroupIrs <- lapply(split(subsetCovariates, subsetCovariates$covariateId), computeSubgroupIrs, cohort = subsetCohort, outcomes = subsetOutcomes)
        irs <- rbind(irs, do.call("rbind", subgroupIrs))
        irs$exposureId <- cohortId
        return(irs)
    }
    allIrs <- plyr::llply(cohortIds, computeCohortIrs, .progress = "text")
    allIrs <- do.call("rbind", allIrs)
    write.csv(allIrs, file.path(indicationFolder, "incidence.csv"), row.names = FALSE)
}

computeSubgroupIrs <- function(cohort, outcomes, subgroupCovs) {
    subgroupCohort <- cohort[cohort$rowId %in% subgroupCovs$rowId,]
    subgroupIrs <- computeIrs(subgroupCohort, outcomes)
    subgroupIrs$interactionCovariateId <- subgroupCovs$covariateId[1]
    return(subgroupIrs)
}

computeIrs <- function(cohort, outcomes) {

    computeIrForOutcome <- function(outcome, cohort) {
        outcomeId <- outcome$outcomeId[1]
        priorOutcomeRowIds <- unique(outcome$rowId[outcome$daysToEvent < 0])
        cohort$priorOutcome <- cohort$rowId %in% priorOutcomeRowIds
        outcome <- outcome[outcome$daysToEvent >= 0, ]
        outcome <- outcome[order(outcome$rowId, outcome$daysToEvent), ]
        firstOutcomePostIndex <- outcome[!duplicated(outcome$rowId), ]
        m <- merge(cohort, firstOutcomePostIndex, all.x = TRUE)
        m$eventOnTreatment <- !is.na(m$daysToEvent) & m$daysToEvent <= m$daysToCohortEnd & m$daysToEvent <= m$daysToObsEnd
        m$eventItt <- !is.na(m$daysToEvent) & m$daysToEvent <= m$daysToObsEnd
        m$timeItt <- m$daysToObsEnd
        m$timeItt[!is.na(m$daysToEvent) & (m$daysToEvent < m$timeItt)] <- m$daysToEvent[!is.na(m$daysToEvent) & (m$daysToEvent < m$timeItt)]
        m$timeOnTreatment <- m$timeItt
        m$timeOnTreatment[m$daysToCohortEnd < m$timeOnTreatment] <- m$daysToCohortEnd[m$daysToCohortEnd < m$timeOnTreatment]
        m$dummy <- 1
        result <- data.frame(outcomeId = outcomeId,
                             incidenceAnalysisId = c("On-treatment", "Intent-to-treat"),
                             outcomes = c(sum(m$eventOnTreatment[!m$priorOutcome]),
                                          sum(m$eventItt[!m$priorOutcome])),
                             days = c(sum(m$timeOnTreatment[!m$priorOutcome]),
                                      sum(m$timeItt[!m$priorOutcome])),
                             subjects = c( sum(m$dummy[!m$priorOutcome]),
                                           sum(m$dummy[!m$priorOutcome])),
                             stringsAsFactors = FALSE)
        return(result)
    }
    irs <- lapply(split(outcomes, outcomes$outcomeId), computeIrForOutcome, cohort = cohort)
    irs <- do.call("rbind", irs)
    return(irs)
}

getCohort <- function(cohortId, exposureSummary, indicationFolder) {
    cohortsFolder <- file.path(indicationFolder, "allCohorts")
    # Create one cohort with all unique rows across all comparisons including this cohort:
    getCohortFromFile <- function(i, treatment) {
        targetId <- exposureSummary$targetId[i]
        comparatorId <- exposureSummary$comparatorId[i]
        fileName <- file.path(cohortsFolder, paste0("cohorts_t", targetId, "_c", comparatorId))
        cohorts <- readRDS(fileName)
        return(cohorts[cohorts$treatment == treatment, ])
    }
    targets <- lapply(which(exposureSummary$targetId == cohortId), getCohortFromFile, treatment = 1)
    comparators <- lapply(which(exposureSummary$comparatorId == cohortId), getCohortFromFile, treatment = 0)
    cohort <- do.call("rbind", c(targets, comparators))
    cohort <- cohort[order(cohort$rowId), ]
    cohort <- cohort[!duplicated(cohort$rowId), ]
    return(cohort)
}

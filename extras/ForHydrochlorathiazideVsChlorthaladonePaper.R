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

# Code for generating numbers and plots to be presented at the OHDSI 2018 Symposium.

library(DatabaseConnector)
source("extras/LegendMedCentral/DataPulls.R")


connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = paste(Sys.getenv("shinydbServer"),
                                                            Sys.getenv("shinydbDatabase"),
                                                            sep = "/"),
                                             port = Sys.getenv("shinydbPort"),
                                             user = Sys.getenv("shinydbUser"),
                                             password = Sys.getenv("shinydbPw"),
                                             schema = Sys.getenv("shinydbSchema"))
connection <- connect(connectionDetails)

# PS plots hydrochlorathiazide vs chlorthaladone --------------------------------------------------------------
databaseIds <-  c("CCAE", "Optum", "Panther")
targetId <- 1395058
comparatorId <- 974166
exposures <- getExposures(connection = connection,
                          filterByCmResults = FALSE)
eoi <- exposures[exposures$exposureId %in% c(targetId, comparatorId), ]
targetName <- eoi$exposureName[eoi$exposureId == targetId]
comparatorName <- eoi$exposureName[eoi$exposureId == comparatorId]
loadPs <- function(databaseId) {
    ps <- getPs(connection = connection,
                databaseId = databaseId,
                targetIds = targetId,
                comparatorIds = comparatorId)
    ps$databaseId <- databaseId
    return(ps)
}
ps <- lapply(databaseIds, loadPs)
ps <- do.call("rbind", ps)

# ps$preferenceScore[idx] <- 1 - ps$preferenceScore[idx]
ps <- rbind(data.frame(databaseId = ps$databaseId,
                       x = ps$preferenceScore,
                       y = ps$targetDensity,
                       group = targetName,
                       stringsAsFactors = FALSE),
            data.frame(databaseId = ps$databaseId,
                       x = ps$preferenceScore,
                       y = ps$comparatorDensity,
                       group = comparatorName,
                       stringsAsFactors = FALSE))


ps$group <- factor(ps$group, levels = c(targetName, comparatorName))
ps$databaseId[ps$databaseId == "Panther"] <- "PanTher"
plot <- ggplot2::ggplot(ps, ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) +
    ggplot2::geom_density(stat = "identity") +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Preference score", limits = c(0, 1), breaks = c(0, 0.2, 0.5, 0.75, 1), labels = c("0", "0.25", "0.5", "0.75", "1")) +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::facet_grid(~databaseId) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   # axis.title.x = ggplot2::element_blank(),
                   # axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   # axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   legend.position = "top")
fileName <- file.path("c:/temp/papers", "ps.png")
ggplot2::ggsave(filename = fileName, plot = plot, width = 8, height = 3, dpi = 300)


# Balance plots hydrochlorathiazide vs chlorthaladone --------------------------------------------------------------
databaseIds <-  c("CCAE", "Optum", "Panther")
comparatorId <- 974166
targetId <- 1395058
matching <- FALSE

exposures <- getExposures(connection = connection,
                          filterByCmResults = FALSE)
eoi <- exposures[exposures$exposureId %in% c(targetId, comparatorId), ]
targetName <- eoi$exposureName[eoi$exposureId == targetId]
comparatorName <- eoi$exposureName[eoi$exposureId == comparatorId]
balanceFolder <- "Documents/HctzCtdBalance"
loadBal <- function(databaseId) {
    if (matching) {
        fileName <- file.path(balanceFolder, sprintf("Balance_matching_%s.rds", databaseId))
    } else {
        fileName <- file.path(balanceFolder, sprintf("Balance_%s.rds", databaseId))
    }

    bal <- readRDS(fileName)
    bal <- bal[, c("beforeMatchingStdDiff", "afterMatchingStdDiff")]
    saveRDS(bal, fileName)
    bal$absBeforeMatchingStdDiff <- abs(bal$beforeMatchingStdDiff)
    bal$absAfterMatchingStdDiff <- abs(bal$afterMatchingStdDiff)

    # bal <- getCovariateBalance(connection = connection,
    #                            databaseId = databaseId,
    #                            targetId = targetId,
    #                            comparatorId = comparatorId,
    #                            analysisId = 2)
    bal$databaseId <- databaseId
    return(bal)
}
balance <- lapply(databaseIds, loadBal)
balance <- do.call("rbind", balance)
balance$databaseId[balance$databaseId == "Panther"] <- "PanTher"
limits <- c(min(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                na.rm = TRUE),
            max(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                na.rm = TRUE))
theme <- ggplot2::element_text(colour = "#000000", size = 12)
balance$covariateCount <- 1
labels <- aggregate(covariateCount ~ databaseId, balance, sum)

labels$text <- sprintf("Number of covariates: %s", format(labels$covariateCount, big.mark = ",", scientific = FALSE))
if (matching) {
    adjLabel <- "matching"
} else {
    adjLabel <- "stratification"
}

plot <- ggplot2::ggplot(balance, ggplot2::aes(x = absBeforeMatchingStdDiff, y = absAfterMatchingStdDiff)) +
    ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16, size = 1) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_label(x = limits[1] + 0.01,
                        y = limits[2],
                        hjust = "left",
                        vjust = "top",
                        alpha = 0.8,
                        ggplot2::aes(label = text),
                        data = labels,
                        size = 3.5) +
    ggplot2::scale_x_continuous(paste("Before", adjLabel), limits = limits) +
    ggplot2::scale_y_continuous(paste("After", adjLabel), limits = limits) +
    ggplot2::facet_grid(~databaseId) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank())
if (matching) {
    fileName <- file.path("c:/temp/papers", "bal_matching.pdf")
} else {
    fileName <- file.path("c:/temp/papers", "bal.pdf")
}

ggplot2::ggsave(filename = fileName, plot = plot, width = 8, height = 3, dpi = 300)

# Dose sensitivity analysis --------------------------------------------------------------------------------------

# Chlorthalidone only those whose initial dose was 12.5 (18%)
# Hydrochlorthiazide only those whose initial dose was 25 (56%)

library(Legend)
targetId <- 1395058 #Chlorthalidone
comparatorId <- 974166 #Hydrochlorothiazide
databaseId <- "CCAE"
databaseName <- "Truven Health MarketScan Commercial Claims and Encounters Database"
databaseDescription <- "Truven Health MarketScan® Commercial Claims and Encounters Database (CCAE) represent data from individuals enrolled in United States employer-sponsored insurance health plans. The data includes adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy) as well as enrollment data from large employers and health plans who provide private healthcare coverage to employees, their spouses, and dependents. Additionally, it captures laboratory tests for a subset of the covered lives. This administrative claims database includes a variety of fee-for-service, preferred provider organizations, and capitated health plans."
options(fftempdir = "r:/fftemp")
studyFolder <- "r:/Legend"
dbms <- "pdw"
user <- NULL
pw <- NULL
server <- Sys.getenv("PDW_SERVER")
port <- Sys.getenv("PDW_PORT")
oracleTempSchema <- NULL
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)
indicationId <- "Hypertension"
cdmDatabaseSchema <- "cdm_truven_ccae_v750.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_ccae"
databaseId <- "CCAE"

outputFolder <- file.path(studyFolder, "ccae")
doseOutputFolder <- file.path(studyFolder, "ccaeDose")
indicationFolder <- file.path(outputFolder, indicationId)
cmFolder <- file.path(doseOutputFolder, indicationId, "cmOutput")

if (!file.exists(cmFolder))
    dir.create(cmFolder, recursive = TRUE)

conn <- DatabaseConnector::connect(connectionDetails)

# Create dose era table
sql <- SqlRender::readSql("extras/DoseEras.sql")
DatabaseConnector::renderTranslateExecuteSql(connection = conn,
                                             sql = sql,
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             max_gap = 30)

# Get overview of doses identified for manual inspection
sql <- "SELECT * FROM #dose_overview ORDER BY -record_count;"
overview <- DatabaseConnector::renderTranslateQuerySql(connection = conn,
                                                       sql = sql,
                                                       oracleTempSchema = oracleTempSchema)
View(overview)

# Extract subsetted cohorts
sql <- "
SELECT cohort.subject_id,
    CASE
		WHEN cohort.cohort_definition_id = @target_id
			THEN 1
		ELSE 0
		END AS treatment,
    cohort.cohort_start_date,
    DATEDIFF(DAY, dose_era.cohort_start_date, dose_era.cohort_end_date) AS days_to_cohort_end
FROM @cohort_database_schema.@paired_cohort_table cohort
INNER JOIN #dose_era dose_era
    ON cohort.subject_id = dose_era.subject_id
        AND cohort.cohort_start_date = dose_era.cohort_start_date
        AND cohort.cohort_definition_id = dose_era.cohort_definition_id
WHERE (target_id = @target_id AND comparator_id = @comparator_id)
    OR (comparator_id = @target_id AND target_id = @comparator_id);
"

pairedCohortTable <- paste(tablePrefix, tolower(indicationId), "pair_cohort", sep = "_")
subsetCohorts <- DatabaseConnector::renderTranslateQuerySql(connection = conn,
                                                      sql = sql,
                                                      oracleTempSchema = oracleTempSchema,
                                                      cohort_database_schema = cohortDatabaseSchema,
                                                      paired_cohort_table = pairedCohortTable,
                                                      target_id = targetId,
                                                      comparator_id = comparatorId,
                                                      snakeCaseToCamelCase = TRUE)
saveRDS(subsetCohorts, file.path(cmFolder, "subsetCohorts.rds"))
DatabaseConnector::disconnect(conn)

# Construct cohortMethodData object. Mostly copied from constructCohortMethodDataObject function:
useSample <- FALSE
cohortsFolder <- file.path(indicationFolder, "allCohorts")
outcomesFolder <- file.path(indicationFolder, "allOutcomes")
covariatesFolder <- file.path(indicationFolder, "allCovariates")
subsetCohorts <- readRDS(file.path(cmFolder, "subsetCohorts.rds"))
# copying cohorts
# Note: need to swap T and C:
fileName <- file.path(cohortsFolder, paste0("cohorts_t", comparatorId, "_c", targetId, ".rds"))
cohorts <- readRDS(fileName)
# Swap treatment indicator:
cohorts$treatment <- 1 - cohorts$treatment
# Subset to right dose eras:
# Taking cohort end data from dose era (may have switched dose while staying on ingredient:
cohorts$daysToCohortEnd <- NULL
writeLines("Before restriction original cohorts:")
table(cohorts$treatment)
writeLines("Before restriction dose eras:")
table(subsetCohorts$treatment)
cohorts <- merge(cohorts, subsetCohorts)
cohorts <- cohorts[order(cohorts$rowId), ]
writeLines("After restriction cohorts:")
table(cohorts$treatment)

targetPersons <- length(unique(cohorts$subjectId[cohorts$treatment == 1]))
comparatorPersons <- length(unique(cohorts$subjectId[cohorts$treatment == 0]))
targetExposures <- length(cohorts$subjectId[cohorts$treatment == 1])
comparatorExposures <- length(cohorts$subjectId[cohorts$treatment == 0])
counts <- data.frame(description = "Starting cohorts",
                     targetPersons = targetPersons,
                     comparatorPersons = comparatorPersons,
                     targetExposures = targetExposures,
                     comparatorExposures = comparatorExposures)
metaData <- list(targetId = targetId, comparatorId = comparatorId, attrition = counts)
attr(cohorts, "metaData") <- metaData

# Subsetting outcomes
ParallelLogger::logTrace("Subsetting outcomes")
outcomes <- NULL
ffbase::load.ffdf(dir = outcomesFolder)  # Loads outcomes
ff::open.ffdf(outcomes, readonly = TRUE)
idx <- ffbase::`%in%`(outcomes$rowId, ff::as.ff(cohorts$rowId))
if (ffbase::any.ff(idx)) {
    outcomes <- ff::as.ram(outcomes[idx, ])
} else {
    outcomes <- as.data.frame(outcomes[1, ])
    outcomes <- outcomes[T == F, ]
}
if (!useSample) {
    # Add injected outcomes (no signal injection when doing sampling)
    injectionSummary <- read.csv(file.path(indicationFolder, "signalInjectionSummary.csv"),
                                 stringsAsFactors = FALSE)
    injectionSummary <- injectionSummary[injectionSummary$exposureId == targetId |
                                             injectionSummary$exposureId == comparatorId, ]
    injectionSummary <- injectionSummary[injectionSummary$outcomesToInjectFile != "", ]

    if (nrow(injectionSummary) > 0) {
        # Add original (background) negative control outcomes
        bgOutcomes <- merge(outcomes, injectionSummary[, c("outcomeId", "newOutcomeId")])
        bgOutcomes$outcomeId <- bgOutcomes$newOutcomeId
        outcomes <- rbind(outcomes, bgOutcomes[, colnames(outcomes)])

        # Add additional outcomes
        synthOutcomes <- lapply(injectionSummary$outcomesToInjectFile, readRDS)
        synthOutcomes <- do.call("rbind", synthOutcomes)
        colnames(synthOutcomes)[colnames(synthOutcomes) == "cohortStartDate"] <- "eventDate"
        colnames(synthOutcomes)[colnames(synthOutcomes) == "cohortDefinitionId"] <- "outcomeId"
        synthOutcomes <- merge(synthOutcomes, cohorts[, c("rowId", "subjectId", "cohortStartDate")])
        synthOutcomes$daysToEvent <- synthOutcomes$eventDate - synthOutcomes$cohortStartDate
        outcomes <- rbind(outcomes, synthOutcomes[, colnames(outcomes)])
    }
}
metaData <- data.frame(outcomeIds = unique(outcomes$outcomeId))
attr(outcomes, "metaData") <- metaData

# Subsetting covariates
ParallelLogger::logTrace("Subsetting covariates")
covariateData <- FeatureExtraction::loadCovariateData(covariatesFolder)
idx <- ffbase::`%in%`(covariateData$covariates$rowId, ff::as.ff(cohorts$rowId))
covariates <- covariateData$covariates[idx, ]

# Filtering covariates
ParallelLogger::logTrace("Filtering covariates")
filterConcepts <- readRDS(file.path(indicationFolder, "filterConceps.rds"))
filterConcepts <- filterConcepts[filterConcepts$cohortId %in% c(targetId, comparatorId), ]
filterConceptIds <- unique(filterConcepts$filterConceptId)
if (length(filterConceptIds) == 0) {
    covariateRef <- covariateData$covariateRef
} else {
    idx <- ffbase::`%in%`(covariateData$covariateRef$conceptId, ff::as.ff(filterConceptIds))
    covariateRef <- covariateData$covariateRef[!idx, ]
    filterCovariateIds <- covariateData$covariateRef$covariateId[idx, ]
    idx <- !ffbase::`%in%`(covariates$covariateId, filterCovariateIds)
    covariates <- covariates[idx, ]
}
result <- list(cohorts = cohorts,
               outcomes = outcomes,
               covariates = covariates,
               covariateRef = covariateRef,
               analysisRef = ff::clone.ffdf(covariateData$analysisRef),
               metaData = covariateData$metaData)

class(result) <- "cohortMethodData"
folderName <- file.path(cmFolder,
                        paste0("CmData_l1_t", targetId, "_c", comparatorId))
CohortMethod::saveCohortMethodData(result, folderName, compress = TRUE)


# Apply cohort method to compute estimates
exposureSummary <- data.frame(targetId = targetId,
                              comparatorId = comparatorId,
                              databaseId = databaseId,
                              targetMinDate = as.Date("2000-01-01"),
                              targetMaxDate = as.Date("2000-01-01"),
                              comparatorMinDate = as.Date("2000-01-01"),
                              comparatorMaxDate = as.Date("2000-01-01"),
                              pairedMinDate = as.Date("2000-01-01"),
                              pairedMaxDate = as.Date("2000-01-01"))
write.csv(exposureSummary,
          file.path(doseOutputFolder, indicationId, "pairedExposureSummaryFilteredBySize.csv"),
          row.names = FALSE)
file.copy(file.path(indicationFolder, "signalInjectionSummary.csv"),
          file.path(doseOutputFolder, indicationId, "signalInjectionSummary.csv"))
runCohortMethod(doseOutputFolder, indicationId, databaseId, maxCores = 20)

# Compute covariate balance
computeCovariateBalance(outputFolder = doseOutputFolder,
                        indicationId = indicationId,
                        maxCores = 20)

# Export results
exportResults(indicationId = indicationId,
              outputFolder = doseOutputFolder,
              databaseId = databaseId,
              databaseName = databaseName,
              databaseDescription = databaseDescription,
              minCellCount = 5,
              maxCores = 20)

# Create output
source("extras/LegendMedCentral/PlotsAndTables.R")
exportFolder <- file.path(doseOutputFolder, indicationId, "export")

# Propensity scores
ps <- read.csv(file.path(exportFolder, "preference_score_dist.csv"))
colnames(ps) <- SqlRender::snakeCaseToCamelCase(colnames(ps))
ps <- ps[ps$targetId == targetId & ps$comparatorId == comparatorId, ]
plot <- plotPs(ps, "Chlorthalidone", "Hydrochlorothiazide")
ggplot2::ggsave(file.path(doseOutputFolder, "ps.png"), plot = plot, width = 6, height = 4)

# Balance
bal <- read.csv(file.path(exportFolder, "covariate_balance.csv"))
colnames(bal) <- c("databaseId",
                       "targetId",
                       "comparatorId",
                       "outcomeId",
                       "analysisId",
                       "interactionCovariateId",
                       "covariateId",
                       "beforeMatchingMeanTreated",
                       "beforeMatchingMeanComparator",
                       "beforeMatchingStdDiff",
                       "afterMatchingMeanTreated",
                       "afterMatchingMeanComparator",
                       "afterMatchingStdDiff")
bal$absBeforeMatchingStdDiff <- abs(bal$beforeMatchingStdDiff)
bal$absAfterMatchingStdDiff <- abs(bal$afterMatchingStdDiff)
bal <- bal[bal$targetId == targetId & bal$comparatorId == comparatorId & is.na(bal$outcomeId) & is.na(bal$interactionCovariateId), ]

plot <- plotCovariateBalanceScatterPlot(bal[bal$analysisId == 2, ],
                                        beforeLabel = "Before stratification",
                                        afterLabel = "After stratification",
                                        showCovariateCountLabel = TRUE,
                                        showMaxLabel = TRUE)

ggplot2::ggsave(file.path(doseOutputFolder, "balanceStratification.png"), width = 6, height = 6)

plot <- plotCovariateBalanceScatterPlot(bal[bal$analysisId == 4, ],
                                        beforeLabel = "Before matching",
                                        afterLabel = "After matching",
                                        showCovariateCountLabel = TRUE,
                                        showMaxLabel = TRUE)

ggplot2::ggsave(file.path(doseOutputFolder, "balanceMatching.png"), width = 6, height = 6)

# Calibration
estimates <- read.csv(file.path(exportFolder, "cohort_method_result.csv"))
colnames(estimates) <- SqlRender::snakeCaseToCamelCase(colnames(estimates))
estimates <- estimates[estimates$targetId == targetId & estimates$comparatorId == comparatorId, ]
ncs <- read.csv(file.path(exportFolder, "negative_control_outcome.csv"))
colnames(ncs) <- SqlRender::snakeCaseToCamelCase(colnames(ncs))
pcs <- read.csv(file.path(exportFolder, "positive_control_outcome.csv"))
colnames(pcs) <- SqlRender::snakeCaseToCamelCase(colnames(pcs))

ncs$effectSize <- 1
controls <- rbind(ncs[, c("outcomeId", "effectSize")],
                  pcs[, c("outcomeId", "effectSize")])
controls <- merge(controls, estimates)

plot <- plotScatter(controls[controls$analysisId == 1, ])
ggplot2::ggsave(file.path(doseOutputFolder, "calibrationStratification.png"), width = 14, height = 5)

plot <- plotScatter(controls[controls$analysisId == 3, ])
ggplot2::ggsave(file.path(doseOutputFolder, "calibrationMatching.png"), width = 14, height = 5)

# Estimates
outcomes <- read.csv(file.path(exportFolder, "outcome_of_interest.csv"))
colnames(outcomes) <- SqlRender::snakeCaseToCamelCase(colnames(outcomes))

estimatesForHois <- merge(estimates, outcomes[, c("outcomeId", "outcomeName")])
estimatesForHois <- estimatesForHois[estimatesForHois$analysisId %in% c(1,3), ]
estimatesForHois$analysis <- "Stratified"
estimatesForHois$analysis[estimatesForHois$analysisId == 3] <- "Matched"
write.csv(estimatesForHois, file.path(doseOutputFolder, "estimatesForHois.csv"), row.names = FALSE)

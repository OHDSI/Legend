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

# Code used in databases that have blood pressure (BP) data to see if we achieved balance on BP.
# Assumes the database connection details and output folder etc. have been set.

# Run this first: Fetch BP values from server --------------------------------------------
indicationFolder <- file.path(outputFolder, indicationId)
bpFolder <- file.path(indicationFolder, "bp")
if (!file.exists(bpFolder)) {
    dir.create(bpFolder)
}
prepareForDataFetch <- function(conn, indicationFolder) {
    pairedCohortTable <- paste(tablePrefix, tolower(indicationId), "pair_cohort", sep = "_")
    cohortsFolder <- file.path(indicationFolder, "allCohorts")
    exposureSummary <- read.csv(file.path(indicationFolder,
                                          "pairedExposureSummaryFilteredBySize.csv"))
    table <- exposureSummary[, c("targetId", "comparatorId")]
    colnames(table) <- SqlRender::camelCaseToSnakeCase(colnames(table))
    DatabaseConnector::insertTable(connection = conn,
                                   tableName = "#comparisons",
                                   data = table,
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   oracleTempSchema = oracleTempSchema)

    sql <- SqlRender::loadRenderTranslateSql("UnionExposureCohorts.sql",
                                             "Legend",
                                             dbms = connectionDetails$dbms,
                                             oracleTempSchema = oracleTempSchema,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             paired_cohort_table = pairedCohortTable)
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

    sql <- "TRUNCATE TABLE #comparisons; DROP TABLE #comparisons;"
    sql <- SqlRender::translateSql(sql = sql,
                                   targetDialect = connectionDetails$dbms,
                                   oracleTempSchema = oracleTempSchema)$sql
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

    # Just to make sure: check of rowIds are consistent with those generated before:
    sql <- "SELECT row_id, subject_id, cohort_start_date FROM #exposure_cohorts"
    sql <- SqlRender::translateSql(sql = sql,
                                   targetDialect = connectionDetails$dbms,
                                   oracleTempSchema = oracleTempSchema)$sql
    newCohorts <- DatabaseConnector::querySql(conn, sql)
    colnames(newCohorts) <- SqlRender::snakeCaseToCamelCase(colnames(newCohorts))
    newCohorts <- newCohorts[order(newCohorts$rowId,
                                   newCohorts$subjectId,
                                   newCohorts$cohortStartDate), ]
    row.names(newCohorts) <- NULL
    allCohorts <- readRDS(file.path(cohortsFolder, "allCohorts.rds"))
    allCohorts <- allCohorts[, colnames(newCohorts)]
    allCohorts <- unique(allCohorts)
    allCohorts <- allCohorts[order(allCohorts$rowId,
                                   allCohorts$subjectId,
                                   allCohorts$cohortStartDate), ]
    row.names(allCohorts) <- NULL
    if (!all.equal(allCohorts, newCohorts)) {
        stop("row IDs have changed. Hot swap failed")
    }
    ParallelLogger::logInfo("Verified that rowIds are the same")
}

conn <- DatabaseConnector::connect(connectionDetails)
sql <- "SELECT measurement_type_concept_id, COUNT(*)
FROM @cdm_database_schema.measurement
WHERE measurement_concept_id IN (3004249, 3012888)
GROUP BY measurement_type_concept_id;"
sql <- SqlRender::renderSql(sql, cdm_database_schema = cdmDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
types <- DatabaseConnector::querySql(conn, sql)

querySql(conn, "SELECT * FROM cdm_optum_panther_v735.dbo.concept WHERE concept_id = 45754907")
querySql(conn, "SELECT TOP 10 * FROM cdm_optum_panther_v735.dbo.measurement WHERE measurement_concept_id IN (3004249, 3012888)")

sql <- "SELECT MEASUREMENT_SOURCE_VALUE, COUNT(*)
FROM @cdm_database_schema.measurement
WHERE measurement_concept_id IN (3004249, 3012888)
GROUP BY MEASUREMENT_SOURCE_VALUE;"
sql <- SqlRender::renderSql(sql, cdm_database_schema = cdmDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
types <- DatabaseConnector::querySql(conn, sql)


prepareForDataFetch(conn, indicationFolder)

sql <- "SELECT row_id,
    value_as_number,
    measurement_date,
    concept_id,
    concept_name
FROM @cdm_database_schema.measurement
INNER JOIN @cdm_database_schema.concept
ON measurement_concept_id = concept_id
INNER JOIN #exposure_cohorts
ON person_id = subject_id
AND measurement_date <= cohort_start_date
AND measurement_date > DATEADD(DAY, -365, cohort_start_date)
WHERE concept_id IN (3004249, 3012888)"
sql <- SqlRender::renderSql(sql, cdm_database_schema = cdmDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
bps <- DatabaseConnector::querySql(conn, sql)
colnames(bps) <- SqlRender::snakeCaseToCamelCase(colnames(bps))
DatabaseConnector::disconnect(conn)

bps <- bps[!is.na(bps$valueAsNumber), ]
bps <- bps[order(bps$rowId, bps$measurementDate, decreasing = TRUE), ]
bps <- bps[!duplicated(paste(bps$rowId, bps$conceptId)), ]
saveRDS(bps, file.path(bpFolder, "bps.rds"))

# Run this next: Compute balance per TC --------------------------------------------------------
indicationFolder <- file.path(outputFolder, indicationId)
bpFolder <- file.path(indicationFolder, "bp")
bps <- readRDS(file.path(bpFolder, "bps.rds"))
bps <- bps[bps$valueAsNumber < 250, ]
bps <- bps[bps$valueAsNumber > 25, ]
outcomeModelReference <- readRDS(file.path(indicationFolder,
                                           "cmOutput",
                                           "outcomeModelReference1.rds"))
outcomeModelReference <- outcomeModelReference[outcomeModelReference$analysisId %in% c(1,3), ]
outcomeModelReference <- outcomeModelReference[order(outcomeModelReference$cohortMethodDataFolder, outcomeModelReference$outcomeId), ]
outcomeModelReference <- outcomeModelReference[!duplicated(paste(outcomeModelReference$targetId, outcomeModelReference$comparatorId, outcomeModelReference$analysisId)), ]
exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
outcomeModelReference <- merge(outcomeModelReference, exposureSummary[, c("targetId", "comparatorId", "targetName", "comparatorName")])
rows <- split(outcomeModelReference, 1:nrow(outcomeModelReference))

# For now: only pick one row:
rowIndex <- which(outcomeModelReference$targetName == "Hydrochlorothiazide" & outcomeModelReference$comparatorName == "Chlorthalidone")
rows <- list(rows[[rowIndex]])
compBal <- function(row, indicationFolder) {
    #row <- rows[[4759]]
    cmData <- CohortMethod::loadCohortMethodData(file.path(indicationFolder,
                                                           "cmOutput",
                                                           row$cohortMethodDataFolder),
                                                 skipCovariates = TRUE)
    sharedPs <- readRDS(file.path(indicationFolder, "cmOutput", row$sharedPsFile))
    strataPop <- CohortMethod::stratifyByPs(sharedPs, numberOfStrata = 10)
    # strataPop <- readRDS(file.path(indicationFolder, "cmOutput", row$strataFile))

    # Flip T and C if needed:
    strataPop$treatment <- 1 - strataPop$treatment
    cmData$cohorts$treatment <- 1-cmData$cohorts$treatment
    temp <- row$targetName
    row$targetName <- row$comparatorName
    row$comparatorName <- temp
    # strataPop <- CohortMethod::matchOnPs(strataPop, maxRatio = 100)
    if (nrow(strataPop) == 0) {
        return(NULL)
    }
    writeLines(paste0("Number of subjects starting ", row$targetName,": ", sum(strataPop$treatment == 1)))
    writeLines(paste0("Number of subjects starting ", row$comparatorName,": ", sum(strataPop$treatment == 0)))

    # Restricting before cohort to just those in the after cohort:
    subset <- bps[bps$rowId %in% strataPop$rowId, ]
    strataPop <- strataPop[strataPop$rowId %in% subset$rowId, ]
    cmData$cohorts <- cmData$cohorts[cmData$cohorts$rowId %in% subset$rowId, ]
    writeLines("After restricting to those with blood pressure measurements:")
    writeLines(paste0("Number of subjects starting ", row$targetName,": ", sum(strataPop$treatment == 1)))
    writeLines(paste0("Number of subjects starting ", row$comparatorName,": ", sum(strataPop$treatment == 0)))

    # Create histogram:
    m <- merge(subset, strataPop[, c("rowId", "treatment", "stratumId")])
    m$group <- as.character(row$targetName)
    m$group[m$treatment == 0] <- as.character(row$comparatorName)
    m$group <- factor(m$group, levels = c(as.character(row$targetName),
                                          as.character(row$comparatorName)))
    m$conceptName[m$conceptName == "BP diastolic"] <- "Diastolic"
    m$conceptName[m$conceptName == "BP systolic"] <- "Systolic"
    plot <- ggplot2::ggplot(m, ggplot2::aes(x = valueAsNumber, group = group, color = group, fill = group)) +
        ggplot2::geom_histogram(position = "identity", binwidth = 10) +
        ggplot2::facet_grid(conceptName~.) +
        ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
        ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
        ggplot2::xlab("Blood pressure") +
        ggplot2::ylab("Count") +
        ggplot2::xlim(c(50,200)) +
        ggplot2::theme(legend.position = "top",
                       legend.title = ggplot2::element_blank())
    before <- plot + ggplot2::facet_grid(conceptName~.)
    after <- plot + ggplot2::facet_grid(conceptName~stratumId)
    ggplot2::ggsave(file.path(outputFolder, sprintf("Before_%s_%s_histogram.png", as.character(row$targetName), as.character(row$comparatorName))), before, width = 5, height = 4, dpi = 300)
    ggplot2::ggsave(file.path(outputFolder, sprintf("After_%s_%s_histogram.png", as.character(row$targetName), as.character(row$comparatorName))), after, width = 11, height = 5, dpi = 300)

    # Density plot with custom smoothing:
    before <- data.frame()
    after <- data.frame()
    for (conceptName in c("Diastolic", "Systolic")) {
        for (group in c(as.character(row$targetName), as.character(row$comparatorName))) {
            d <- density(m$valueAsNumber[m$conceptName == conceptName & m$group == group], bw = 5, from = 50, to = 250, n = 210)
            before <- rbind(before,
                            data.frame(x = d$x,
                                       y = d$y,
                                       conceptName = conceptName,
                                       group = group))
            for (stratumId in unique(m$stratumId)) {
                d <- density(m$valueAsNumber[m$conceptName == conceptName & m$group == group & m$stratumId == stratumId], bw = 5, from = 50, to = 250, n = 210)
                after <- rbind(after,
                               data.frame(x = d$x,
                                          y = d$y,
                                          conceptName = conceptName,
                                          group = group,
                                          stratumId = stratumId))
            }
        }
    }
    plot <- ggplot2::ggplot(before, ggplot2::aes(x = x, y = y, group = group, color = group, fill = group)) +
        ggplot2::geom_area(position = "identity") +
        ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
        ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
        ggplot2::facet_grid(conceptName~.) +
        ggplot2::xlab("Blood pressure") +
        ggplot2::ylab("Density") +
        ggplot2::xlim(c(50,200)) +
        ggplot2::theme(legend.position = "top",
                       legend.title = ggplot2::element_blank())
    ggplot2::ggsave(file.path(outputFolder, sprintf("Before_%s_%s_density.png", as.character(row$targetName), as.character(row$comparatorName))), plot, width = 5, height = 4, dpi = 300)


    plot <- ggplot2::ggplot(after, ggplot2::aes(x = x, y = y, group = group, color = group, fill = group)) +
        ggplot2::geom_area(position = "identity") +
        ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
        ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
        ggplot2::facet_grid(conceptName~stratumId) +
        ggplot2::xlab("Blood pressure") +
        ggplot2::ylab("Density") +
        ggplot2::xlim(c(50,200)) +
        ggplot2::theme(legend.position = "top",
                       legend.title = ggplot2::element_blank())
    ggplot2::ggsave(file.path(outputFolder, sprintf("After_%s_%s_density.png", as.character(row$targetName), as.character(row$comparatorName))), plot, width = 11, height = 5, dpi = 300)

    # Use RItools to see if balance results consistent:
    # library(RItools)
    # m <- merge(data.frame(rowId = subset$rowId[subset$conceptName == "BP diastolic"],
    #                       dbp = subset$valueAsNumber[subset$conceptName == "BP diastolic"]),
    #            data.frame(rowId = subset$rowId[subset$conceptName == "BP systolic"],
    #                       sbp = subset$valueAsNumber[subset$conceptName == "BP systolic"]))
    # m <- merge(m, strataPop[, c("rowId", "treatment", "stratumId")])
    # xBalance(treatment ~ sbp + dbp, strata = factor(m$stratumId), data = m, report = c("all"))
    # xBalance(treatment ~ sbp + dbp, data = m, report = c("all"))

    # Compute balance
    cmData$covariates <- ff::as.ffdf(data.frame(rowId = subset$rowId,
                                                covariateId = subset$conceptId,
                                                covariateValue = subset$valueAsNumber))
    subsetRef <- unique(subset[, c("conceptId", "conceptName")])
    cmData$covariateRef <- ff::as.ffdf(data.frame(covariateId = subsetRef$conceptId,
                                                  covariateName = subsetRef$conceptName))
    bal <- CohortMethod::computeCovariateBalance(strataPop, cmData)
    bal$targetId <- row$targetId
    bal$targetName <- row$targetName
    bal$comparatorId <- row$comparatorId
    bal$comparatorName <- row$comparatorName
    bal$analysisId <- row$analysisId

    allBalFile <- file.path(indicationFolder,
                            "balance",
                            sprintf("Bal_t%s_c%s_a2.rds", row$targetId, row$comparatorId))
    allBal <- readRDS(allBalFile)
    bal$maxStdDiffOther <- max(allBal$afterMatchingStdDiff, na.rm = TRUE)
    bal$minStdDiffOther <- min(allBal$afterMatchingStdDiff, na.rm = TRUE)
    return(bal)
}
# cluster <- ParallelLogger::makeCluster(5)
# rows <- split(outcomeModelReference, 1:nrow(outcomeModelReference))
# bal <- ParallelLogger::clusterApply(cluster,
#                                     rows,
#                                     compBal,
#                                     indicationFolder = indicationFolder,
#                                     bps = bps)
# ParallelLogger::stopCluster(cluster)

bal <- plyr::llply(rows,
                   compBal,
                   indicationFolder = indicationFolder)
bal <- do.call("rbind", bal)
write.csv(bal, file.path(bpFolder, "balance.csv"), row.names = FALSE)


# # Plot overall distribution of values ------------------------------
# library(ggplot2)
# bps <- readRDS(file.path(bpFolder, "bps.rds"))
# bps <- bps[bps$valueAsNumber < 250, ]
# bps <- bps[bps$valueAsNumber > 25, ]
# ggplot(bps, aes(x = valueAsNumber)) +
#     geom_histogram(binwidth = 1) +
#     scale_x_continuous(limits = c(40,200), breaks = seq(40,200, 10)) +
#     facet_grid(conceptName~.)
# ggsave(filename = file.path(bpFolder, "bpDist.png"), width = 8, height = 8)
#
# # Plot distribution per PS strata -----------------------------------------------
# targetId <- 1
# comparatorId <- 2745
# options(fftempdir = "a:/fftemp")
# studyFolder <- "b:/Legend"
# outputFolder <- file.path(studyFolder, "panther")
# indicationId <- "Hypertension"
# indicationFolder <- file.path(outputFolder, indicationId)
# bpFolder <- file.path(indicationFolder, "bp")
# bps <- readRDS(file.path(bpFolder, "bps.rds"))
# # bps <- bps[bps$valueAsNumber < 250, ]
# # bps <- bps[bps$valueAsNumber > 25, ]
# outcomeModelReference <- readRDS(file.path(indicationFolder,
#                                            "cmOutput",
#                                            "outcomeModelReference1.rds"))
# outcomeModelReference <- outcomeModelReference[outcomeModelReference$analysisId %in% c(1,3), ]
# outcomeModelReference <- outcomeModelReference[order(outcomeModelReference$cohortMethodDataFolder, outcomeModelReference$outcomeId), ]
# outcomeModelReference <- outcomeModelReference[!duplicated(paste(outcomeModelReference$targetId, outcomeModelReference$comparatorId, outcomeModelReference$analysisId)), ]
# exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
# outcomeModelReference <- merge(outcomeModelReference, exposureSummary[, c("targetId", "comparatorId", "targetName", "comparatorName")])
#
# row <- outcomeModelReference[outcomeModelReference$targetId == targetId &
#                                  outcomeModelReference$comparatorId == comparatorId &
#                                  outcomeModelReference$analysisId == 1, ]
#
# strataFile <- row$strataFile
# strata <- readRDS(file.path(indicationFolder, "cmOutput", strataFile))
# saveRDS(strata, file.path(bpFolder, strataFile))
# cmDataFolder <- row$cohortMethodDataFolder
# cmData <- CohortMethod::loadCohortMethodData(file.path(indicationFolder, "cmOutput", cmDataFolder),
#                                              skipCovariates = TRUE)
# nrow(cmData$cohorts)
# nrow(strata)
# # cmData$cohorts <- cmData$cohorts[cmData$cohorts$rowId %in% strata$rowId, ]
#
# subset <- bps[bps$rowId %in% cmData$cohorts$rowId, ]
# strataPop <- strata[strata$rowId %in% subset$rowId, ]
# # Limit before subjects to those in after:
# cmData$cohorts <- cmData$cohorts[cmData$cohorts$rowId %in% strataPop$rowId, ]
#
# cmData$covariates <- ff::as.ffdf(data.frame(rowId = subset$rowId,
#                                             covariateId = subset$conceptId,
#                                             covariateValue = subset$valueAsNumber))
# subsetRef <- unique(subset[, c("conceptId", "conceptName")])
# cmData$covariateRef <- ff::as.ffdf(data.frame(covariateId = subsetRef$conceptId,
#                                               covariateName = subsetRef$conceptName))
# bal <- CohortMethod::computeCovariateBalance(strataPop, cmData)
# bal

options(fftempdir = "d:/fftemp")

indicationFolder <- file.path(outputFolder, indicationId)
bpFolder <- file.path(indicationFolder, "bp")
if (!file.exists(bpFolder)) {
    dir.create(bpFolder)
}

# Fetch BP values ---------------------------------------------------------------------------
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

# Compute balance per TC -------------------------------------------------------------
options(fftempdir = "a:/fftemp")
studyFolder <- "b:/Legend"
outputFolder <- file.path(studyFolder, "panther")
indicationId <- "Hypertension"
indicationFolder <- file.path(outputFolder, indicationId)
bpFolder <- file.path(indicationFolder, "bp")
bps <- readRDS(file.path(bpFolder, "bps.rds"))
outcomeModelReference <- readRDS(file.path(indicationFolder,
                                           "cmOutput",
                                           "outcomeModelReference1.rds"))
outcomeModelReference <- outcomeModelReference[outcomeModelReference$analysisId %in% c(1,3), ]
outcomeModelReference <- outcomeModelReference[order(outcomeModelReference$cohortMethodDataFolder, outcomeModelReference$outcomeId), ]
outcomeModelReference <- outcomeModelReference[!duplicated(paste(outcomeModelReference$targetId, outcomeModelReference$comparatorId, outcomeModelReference$analysisId)), ]
exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
outcomeModelReference <- merge(outcomeModelReference, exposureSummary[, c("targetId", "comparatorId", "targetName", "comparatorName")])
# which(outcomeModelReference$targetName == "Losartan" & outcomeModelReference$comparatorName == "Olmesartan")
compBal <- function(row, indicationFolder) {
    #row <- rows[[1]]
    cmData <- CohortMethod::loadCohortMethodData(file.path(indicationFolder,
                                                           "cmOutput",
                                                           row$cohortMethodDataFolder),
                                                 skipCovariates = TRUE)
    strataPop <- readRDS(file.path(indicationFolder, "cmOutput", row$strataFile))
    if (nrow(strataPop) == 0) {
        return(NULL)
    }
    # allBal <- CohortMethod::computeCovariateBalance(strataPop, cmData)

    subset <- bps[bps$rowId %in% cmData$cohorts$rowId, ]
    cmData$cohorts <- cmData$cohorts[cmData$cohorts$rowId %in% subset$rowId, ]
    strataPop <- strataPop[strataPop$rowId %in% subset$rowId, ]
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

rows <- split(outcomeModelReference, 1:nrow(outcomeModelReference))
bal <- plyr::llply(rows,
                   compBal,
                   indicationFolder = indicationFolder,
                   .progress = "text")
bal <- do.call("rbind", bal)
write.csv(bal, file.path(bpFolder, "balance.csv"), row.names = FALSE)



hist(bps$valueAsNumber[bps$conceptName == "BP diastolic"])
hist(bps$valueAsNumber[bps$conceptName == "BP systolic"])


library(ggplot2)
bps <- readRDS(file.path(bpFolder, "bps.rds"))
bps <- bps[bps$valueAsNumber < 250, ]
bps <- bps[bps$valueAsNumber > 25, ]
ggplot(bps, aes(x = valueAsNumber)) +
    geom_histogram(binwidth = 1) +
    scale_x_continuous(limits = c(40,200), breaks = seq(40,200, 10)) +
    facet_grid(conceptName~.)
ggsave(filename = file.path(bpFolder, "bpDist.png"), width = 8, height = 8)

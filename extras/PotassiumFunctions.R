downloadPotassiumData <- function(connectionDetails,
                                  indicationFolder,
                                  kFolder,
                                  indicationId,
                                  cdmDatabaseSchema,
                                  cohortDatabaseSchema,
                                  tablePrefix,
                                  oracleTempSchema = NULL) {
    if (!file.exists(kFolder)) {
        dir.create(kFolder)
    }

    prepareForDataFetch <- function(conn, indicationFolder, indicationId, cohortDatabaseSchema, tablePrefix, oracleTempSchema) {
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

    prepareForDataFetch(conn, indicationFolder, indicationId, cohortDatabaseSchema, tablePrefix, oracleTempSchema)

    sql <- "SELECT row_id,
    value_as_number,
    measurement_date
FROM @cdm_database_schema.measurement
INNER JOIN #exposure_cohorts
ON person_id = subject_id
AND measurement_date <= cohort_start_date
AND measurement_date > DATEADD(DAY, -365, cohort_start_date)
WHERE measurement_concept_id = 4173269"
    sql <- SqlRender::renderSql(sql, cdm_database_schema = cdmDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    kBefore <- DatabaseConnector::querySql(conn, sql)
    colnames(kBefore) <- SqlRender::snakeCaseToCamelCase(colnames(kBefore))

    sql <- "SELECT row_id,
    value_as_number,
    measurement_date
FROM @cdm_database_schema.measurement
INNER JOIN #exposure_cohorts
ON person_id = subject_id
AND measurement_date <= DATEADD(DAY, 90, cohort_start_date)
AND measurement_date >= DATEADD(DAY, 30, cohort_start_date)
WHERE measurement_concept_id = 4173269"
    sql <- SqlRender::renderSql(sql, cdm_database_schema = cdmDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    kAfter <- DatabaseConnector::querySql(conn, sql)
    colnames(kAfter) <- SqlRender::snakeCaseToCamelCase(colnames(kAfter))

    DatabaseConnector::disconnect(conn)

    # Taking last of before measurement:
    kBefore <- kBefore[!is.na(kBefore$valueAsNumber), ]
    kBefore <- kBefore[order(kBefore$rowId, kBefore$measurementDate, decreasing = TRUE), ]
    kBefore <- kBefore[!duplicated(kBefore$rowId), ]

    # Taking last of after measurement:
    kAfter <- kAfter[!is.na(kAfter$valueAsNumber), ]
    kAfter <- kAfter[order(kAfter$rowId, kAfter$measurementDate, decreasing = TRUE), ]
    kAfter <- kAfter[!duplicated(kAfter$rowId), ]

    k <- merge(data.frame(rowId = kBefore$rowId,
                          beforeDate = kBefore$measurementDate,
                          beforeValue = kBefore$valueAsNumber),
               data.frame(rowId = kAfter$rowId,
                          afterDate = kAfter$measurementDate,
                          afterValue = kAfter$valueAsNumber))

    saveRDS(k, file.path(kFolder, "k.rds"))
}

plotPotassium <- function(row, indicationFolder, kFolder) {
    # row <- tc
    kFolder <- file.path(indicationFolder, "k")
    k <- readRDS(file.path(kFolder, "k.rds"))
    k <- k[k$beforeValue > 1 & k$beforeValue < 10 & k$afterValue > 1 & k$afterValue < 10, ]

    outcomeModelReference <- readRDS(file.path(indicationFolder,
                                               "cmOutput",
                                               "outcomeModelReference1.rds"))
    outcomeModelReference <- outcomeModelReference[outcomeModelReference$targetId == row$targetId &
                                                       outcomeModelReference$comparatorId == row$comparatorId &
                                                       outcomeModelReference$analysisId == 1, ]
    outcomeModelReference <- outcomeModelReference[1, ]
    cmData <- CohortMethod::loadCohortMethodData(file.path(indicationFolder,
                                                           "cmOutput",
                                                           outcomeModelReference$cohortMethodDataFolder),
                                                 skipCovariates = TRUE)
    sharedPs <- readRDS(file.path(indicationFolder, "cmOutput", outcomeModelReference$sharedPsFile))
    strataPop <- CohortMethod::stratifyByPs(sharedPs, numberOfStrata = 10)
    # strataPop <- CohortMethod::matchOnPs(sharedPs, caliper = 0.2, caliperScale = "standardized logit", maxRatio = 100)
    if (nrow(strataPop) == 0) {
        return(NULL)
    }

    # Restricting before cohort to just those in the after cohort:
    # subset <- bps[bps$rowId %in% strataPop$rowId, ]
    subset <- k[k$rowId %in% cmData$cohorts$rowId, ]
    strataPop <- strataPop[strataPop$rowId %in% subset$rowId, ]
    cmData$cohorts <- cmData$cohorts[cmData$cohorts$rowId %in% subset$rowId, ]

    m <- merge(subset, strataPop[, c("rowId", "treatment", "stratumId")])
    m$difference <- m$afterValue - m$beforeValue

    fit <- aov(difference ~ treatment + as.factor(stratumId), data = m)
    summary(fit)

    m$exposure[m$treatment == 1] <- as.character(row$targetName)
    m$exposure[m$treatment == 0] <- as.character(row$comparatorName)
    m$exposure <- factor(m$exposure, levels = c(as.character(row$targetName), as.character(row$comparatorName)))

    # Save for George:
    data <- m[, c("beforeValue", "afterValue", "exposure", "stratumId")]
    write.csv(data, file.path(kFolder, "kData.csv"), row.names = FALSE)

    m$stratumId <- paste("Stratum", m$stratumId)
    m$stratumId <- factor(m$stratumId, levels = paste("Stratum", 1:10))

    plot <- ggplot2::ggplot(m, ggplot2::aes(x = difference, group = exposure, color = exposure, fill = exposure)) +
        ggplot2::geom_vline(xintercept = 0, size = 1) +
        # ggplot2::geom_density() +
        ggplot2::geom_histogram(position = "identity", binwidth = 0.5) +
        ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
        ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
        ggplot2::xlab("Change in potassium level (after - before)") +
        # ggplot2::ylab("Density") +
        ggplot2::ylab("Number of subjects") +
        ggplot2::facet_wrap(~stratumId, nrow = 2) +
        ggplot2::theme(legend.position = "top",
                       legend.title = ggplot2::element_blank())

    ggplot2::ggsave(filename = file.path(kFolder, "Change.png"), plot = plot, width = 12, height = 8, dpi = 300)

    aggregate(rowId ~ stratumId, data = m, length)

    vizData <- rbind(data.frame(exposure = m$exposure,
                                stratumId = m$stratumId,
                                time = "Before",
                                value = m$beforeValue,
                                stringsAsFactors = FALSE),
                     data.frame(exposure = m$exposure,
                                stratumId = m$stratumId,
                                time = "After",
                                value = m$afterValue,
                                stringsAsFactors = FALSE))

    plot <- ggplot2::ggplot(vizData, ggplot2::aes(x = value, group = exposure, color = exposure, fill = exposure)) +
        ggplot2::geom_density() +
        ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
        ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
        ggplot2::xlab("Potassium level") +
        ggplot2::ylab("Density") +
        ggplot2::xlim(c(2.5,5.5)) +
        ggplot2::facet_grid(time~stratumId) +
        ggplot2::theme(legend.position = "top",
                       legend.title = ggplot2::element_blank())

    ggplot2::ggsave(filename = file.path(kFolder, "BeforeAfter.png"), plot = plot, width = 12, height = 5, dpi = 300)

    # before <- data.frame()
    # after <- data.frame()
    # for (conceptName in c("Diastolic", "Systolic")) {
    #     for (group in c(as.character(row$targetName), as.character(row$comparatorName))) {
    #         d <- density(m$valueAsNumber[m$conceptName == conceptName & m$group == group], bw = 5, from = 50, to = 250, n = 210)
    #         before <- rbind(before,
    #                         data.frame(x = d$x,
    #                                    y = d$y,
    #                                    conceptName = conceptName,
    #                                    group = group))
    #         for (stratumId in unique(m$stratumId)) {
    #             d <- density(m$valueAsNumber[m$conceptName == conceptName & m$group == group & m$stratumId == stratumId], bw = 5, from = 50, to = 250, n = 210)
    #             after <- rbind(after,
    #                            data.frame(x = d$x,
    #                                       y = d$y,
    #                                       conceptName = conceptName,
    #                                       group = group,
    #                                       stratumId = stratumId))
    #         }
    #     }
    # }
    # plotBp <- function(vizData, stratified, fileName) {
    #     plot <- ggplot2::ggplot(vizData, ggplot2::aes(x = x, y = y, group = group, color = group, fill = group)) +
    #         ggplot2::geom_area(position = "identity") +
    #         ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
    #         ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
    #         ggplot2::xlab("Blood pressure") +
    #         ggplot2::ylab("Density") +
    #         ggplot2::xlim(c(50,200)) +
    #         ggplot2::theme(legend.position = "top",
    #                        legend.title = ggplot2::element_blank())
    #     if (stratified) {
    #         plot <- plot + ggplot2::facet_grid(conceptName~stratumId)
    #     } else {
    #         plot <- plot + ggplot2::facet_grid(conceptName~.)
    #     }
    #     width <- if (stratified) {11} else {5}
    #     ggplot2::ggsave(fileName, plot, width = width, height = 4, dpi = 400)
    # }
    # plotBp(before, stratified = FALSE, fileName = file.path(kFolder, sprintf("Before_%s_%s.png", as.character(row$targetName), as.character(row$comparatorName))))
    # plotBp(after, stratified = TRUE, fileName = file.path(kFolder, sprintf("After_%s_%s.png", as.character(row$targetName), as.character(row$comparatorName))))
    #
    # # Flip T and C:
    # before$group <- factor(before$group, levels = c(as.character(row$comparatorName), as.character(row$targetName)))
    # after$group <- factor(after$group, levels = c(as.character(row$comparatorName), as.character(row$targetName)))
    # plotBp(before, stratified = FALSE, fileName = file.path(kFolder, sprintf("Before_%s_%s.png", as.character(row$comparatorName), as.character(row$targetName))))
    # plotBp(after, stratified = TRUE, fileName = file.path(kFolder, sprintf("After_%s_%s.png", as.character(row$comparatorName), as.character(row$targetName))))

    # Compute balance
    cmData$covariates <- ff::as.ffdf(data.frame(rowId = subset$rowId,
                                                covariateId = subset$conceptId,
                                                covariateValue = subset$valueAsNumber))
    subsetRef <- unique(subset[, c("conceptId", "conceptName")])
    cmData$covariateRef <- ff::as.ffdf(data.frame(covariateId = subsetRef$conceptId,
                                                  covariateName = subsetRef$conceptName))
    bal <- CohortMethod::computeCovariateBalance(strataPop, cmData)
    resultRow <- merge(resultRow, bal)
    # balanceFile <- file.path(kFolder, sprintf("Balance_%s_%s.csv", as.character(row$targetName), as.character(row$comparatorName)))
    # write.csv(bal, balanceFile, row.names = FALSE)
    return(resultRow)
}

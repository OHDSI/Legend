library(DatabaseConnector)

connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = paste(Sys.getenv("legendServer"),
                                                            Sys.getenv("legendDatabase"),
                                                            sep = "/"),
                                             port = Sys.getenv("legendPort"),
                                             user = Sys.getenv("legendUser"),
                                             password = Sys.getenv("legendPw"),
                                             schema = Sys.getenv("legendSchema"))
conn <- connect(connectionDetails)
batchSize <- 1e+06
createTables <- FALSE
resumeCount <- 0


querySql(conn, "SELECT * FROM pg_indexes WHERE schemaname = 'legend';")
# executeSql(conn, "GRANT SELECT ON cm_interaction_result TO legend;")
# executeSql(conn, "DROP TABLE cm_interaction_result;")
# executeSql(conn, "ALTER TABLE cohort_method_analysis ALTER COLUMN definition TYPE text;")
# executeSql(conn, "ALTER TABLE covariate ALTER COLUMN covariate_name TYPE text;")

exportFolder <- "R:/Legend/exports/Hypertension/Mdcd"

# Fix attrition table (fix at source here: https://github.com/OHDSI/Legend/commit/8c3f4668da55d078131c8b8509361d2bb717d3a8 )
x <- read.csv(file.path(exportFolder, "attrition.csv"))
x <- x[!is.na(x$exposure_id), ]
write.csv(x, file.path(exportFolder, "attrition.csv"), row.names = FALSE)
rm(x)

# Fix covariate_balance table (fix at source here: https://github.com/OHDSI/Legend/commit/60f95baa0c25f8a9829688f2a36618b799dd95c4 )
x <- read.csv(file.path(exportFolder, "covariate_balance.csv"))
x <- x[!is.na(x$target_id), ]
write.csv(x, file.path(exportFolder, "covariate_balance.csv"), row.names = FALSE)
rm(x)

files <- list.files(exportFolder, pattern = ".*csv")

# resumeCount <- querySql(conn, 'SELECT COUNT(*) FROM legend.covariate_balance')[1,1] createTables <-
# FALSE i <- which(files == 'covariate_balance.csv')

infinityToNa <- function(data) {
    # Replace infinity with NA, because OHDSI Postgres server doesn't like infinity:
    if (nrow(data) > 0) {
        for (j in 1:ncol(data)) {
            if (is.numeric(data[, j])) {
                idx <- is.infinite(data[, j])
                if (any(idx)) {
                    data[idx, j] <- NA
                }
            }
        }
    }
    return(data)
}

keys <- list(indication = c("indication_id"),
             cohort_method_analysis = c("analysis_id"),
             covariate_analysis = c("covariate_analysis_id"),
             incidence_analysis = c("incidence_analysis_id"),
             single_exposure_of_interest = c("exposure_id"),
             combi_exposure_of_interest =  c("exposure_id"),
             exposure_group =  c("exposure_id", "exposure_group"),
             outcome_of_interest =  c("outcome_id"),
             negative_control_outcome = c("outcome_id"),
             positive_control_outcome = c("outcome_id"),
             database = c("database_id"),
             exposure_summary = c("database_id", "exposure_id"),
             comparison_summary = c("database_id", "target_id"),
             attrition = c("database_id", "exposure_id"),
             cm_follow_up_dist = c("database_id", "target_id"),
             covariate = c("database_id", "covariate_id"),
             cohort_method_result = c("database_id", "target_id"),
             cm_interaction_result = c("database_id", "target_id"),
             chronograph = c("database_id", "exposure_id"),
             incidence = c("database_id", "exposure_id"),
             covariate_balance = c("database_id", "target_id"),
             preference_score_dist = c("database_id", "target_id"),
             kaplan_meier_dist = c("database_id", "target_id"),
             propensity_model = c("database_id", "target_id"))

deleteExistingData <- function(data, tableName, dropped) {
    key <- keys[[tableName]]
    toDrop <- unique(data[, key, drop = FALSE])
    if (!is.null(toDrop$database_id)) {
        toDrop$database_id <- as.character(toDrop$database_id)
    }
    if (!is.null(dropped)) {
        dropped$alreadyDropped <- TRUE
        toDrop <- merge(toDrop, dropped, all.x = TRUE)
        toDrop <- toDrop[is.na(toDrop$alreadyDropped), ]
        toDrop$alreadyDropped <- NULL
        dropped$alreadyDropped <- NULL
    }
    if (nrow(toDrop) > 0) {
        createSqlStatement <- function(i) {
            sql <- paste0("DELETE FROM ", tableName, " WHERE ", paste(paste0(key, " = '", toDrop[i, ], "'"), collapse = " AND "), ";")
        }
        sql <- sapply(1:nrow(toDrop), createSqlStatement)
        sql <- paste(sql, collapse = "\n")
        sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
        executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)
    }
    dropped <- unique(rbind(dropped, toDrop))
    return(dropped)
}

for (i in 1:length(files)) {
    file <- files[i]
    tableName <- gsub(".csv$", "", file)
    writeLines(paste("Uploading table", tableName))
    start <- Sys.time()
    fileCon <- file(file.path(exportFolder, file), "r")
    data <- read.csv(fileCon, nrows = batchSize)
    columnNames <- colnames(data)
    data <- infinityToNa(data)
    first <- createTables
    insertedCount <- 0
    dropped <- NULL
    while (nrow(data) != 0) {
        if (insertedCount >= resumeCount) {
            if (!createTables) {
                writeLines("- Deleting existing data with same keys")
                dropped <- deleteExistingData(data, tableName, dropped)
            }
            writeLines("- Inserting data")
            DatabaseConnector::insertTable(connection = conn,
                                           tableName = tableName,
                                           data = data,
                                           dropTableIfExists = first,
                                           createTable = first,
                                           tempTable = FALSE)
        }
        insertedCount <- insertedCount + nrow(data)
        writeLines(paste("- Inserted ", insertedCount, "rows"))
        data <- read.csv(fileCon, nrows = batchSize, header = FALSE, col.names = columnNames)
        data <- infinityToNa(data)
        first <- FALSE
    }
    close(fileCon)

    delta <- Sys.time() - start
    writeLines(paste("- Uploading took", signif(delta, 3), attr(delta, "units")))
}

sql <- "CREATE INDEX idx_attrition ON attrition (database_id, exposure_id, target_id, comparator_id, outcome_id, analysis_id);"
DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

sql <- "CREATE INDEX idx_covariate ON covariate (database_id, covariate_id);"
DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

sql <- "CREATE INDEX idx_cm_follow_up_dist ON cm_follow_up_dist (database_id, target_id, comparator_id, outcome_id, analysis_id);"
DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

sql <- "CREATE INDEX idx_cohort_method_result ON cohort_method_result (database_id, target_id, comparator_id, outcome_id, analysis_id);"
DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

sql <- "CREATE INDEX idx_cm_interaction_result ON cm_interaction_result (database_id, target_id, comparator_id, outcome_id, analysis_id);"
DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

sql <- "CREATE INDEX idx_covariate_balance ON covariate_balance (database_id, target_id, comparator_id);"
DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

sql <- "CREATE INDEX idx_preference_score_dist ON preference_score_dist (database_id, target_id, comparator_id);"
DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

sql <- "CREATE INDEX idx_kaplan_meier_dist ON kaplan_meier_dist (database_id, target_id, comparator_id, outcome_id, analysis_id);"
DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

sql <- "CREATE INDEX idx_propensity_model ON propensity_model (database_id, target_id, comparator_id);"
DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)


disconnect(conn)

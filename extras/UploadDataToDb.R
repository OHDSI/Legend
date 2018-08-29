library(DatabaseConnector)
# connectionDetails <- createConnectionDetails(dbms = "postgresql",
#                                              server = "localhost/ohdsi",
#                                              user = "postgres",
#                                              password = Sys.getenv("pwPostgres"),
#                                              schema = "legend")
connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = paste(Sys.getenv("legendServer"), Sys.getenv("legendDatabase"), sep = "/"),
                                             port = Sys.getenv("legendPort"),
                                             user = Sys.getenv("legendUser"),
                                             password = Sys.getenv("legendPw"),
                                             schema = Sys.getenv("legendSchema"))
conn <- connect(connectionDetails)
batchSize <- 1000000
createTables <- TRUE
resumeCount <- 0


# exportFolder <- file.path(outputFolder, "export")
exportFolder <- "c:/legend/mdcr"
files <- list.files(exportFolder, pattern = ".*csv")

# resumeCount <- querySql(conn, "SELECT COUNT(*) FROM legend.covariate_balance")[1,1]
# createTables <- FALSE
i <- which(files == "covariate_balance.csv")
for (i in 12:length(files)) {
    file <- files[i]
    tableName <- gsub(".csv$", "", file)
    writeLines(paste("Uploading table", tableName))
    start <- Sys.time()
    fileCon = file(file.path(exportFolder, file), "r")
    data <- read.csv(fileCon, nrows = batchSize)
    # Replace infinity with NA, because OHDSI Postgres server doesn't like infinity:
    for (j in 1:ncol(data)) {
        if (is.numeric(data[, j])) {
           idx <- is.infinite(data[, j])
           if (any(idx)) {
               data[idx, j] <- NA
           }
        }
    }
    columnNames <- colnames(data)
    first <- createTables
    insertedCount <- 0
    while (nrow(data) != 0) {
        if (insertedCount >= resumeCount) {
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

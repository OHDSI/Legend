library(DatabaseConnector)
connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = "localhost/ohdsi",
                                             user = "postgres",
                                             password = Sys.getenv("pwPostgres"),
                                             schema = "legend")
conn <- connect(connectionDetails)
batchSize <- 1000000


# exportFolder <- file.path(outputFolder, "export")
exportFolder <- "c:/legend/mdcd"
files <- list.files(exportFolder, pattern = ".*csv")
for (i in 1:length(files)) {
    file <- files[i]
    tableName <- gsub(".csv$", "", file)
    writeLines(paste("Uploading table", tableName))
    start <- Sys.time()
    fileCon = file(file.path(exportFolder, file), "r")
    data <- read.csv(fileCon, nrows = batchSize)
    columnNames <- colnames(data)
    first <- TRUE
    while (nrow(data) != 0) {
        DatabaseConnector::insertTable(connection = conn,
                                       tableName = tableName,
                                       data = data,
                                       dropTableIfExists = first,
                                       createTable = first,
                                       tempTable = FALSE)
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

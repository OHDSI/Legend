connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = "localhost/ohdsi",
                                             user = "postgres",
                                             password = Sys.getenv("pwPostgres"),
                                             schema = "legend")
conn <- connect(connectionDetails)


exportFolder <- file.path(outputFolder, "export")
files <- list.files(exportFolder, pattern = ".*csv")
for (i in 1:length(files)) {
    file <- files[i]
    tableName <- gsub(".csv$", "", file)
    writeLines(paste("Uploading table", tableName))
    start <- Sys.time()
    data = read.csv(file.path(exportFolder, file))
    DatabaseConnector::insertTable(connection = conn,
                                   tableName = tableName,
                                   data = data,
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = FALSE,
                                   oracleTempSchema = oracleTempSchema)
    delta <- Sys.time() - start
    writeLines(paste("- Uploading took", signif(delta, 3), attr(delta, "units")))
}

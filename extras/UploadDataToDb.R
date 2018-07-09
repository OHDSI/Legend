connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = "localhost/ohdsi",
                                             user = "postgres",
                                             password = Sys.getenv("pwPostgres"),
                                             schema = "legend")
conn <- connect(connectionDetails)
batchSize <- 1000000


# exportFolder <- file.path(outputFolder, "export")
exportFolder <- "c:/legend/mdcr"
files <- list.files(exportFolder, pattern = ".*csv")
for (i in 2:length(files)) {
    file <- files[i]
    tableName <- gsub(".csv$", "", file)
    writeLines(paste("Uploading table", tableName))
    start <- Sys.time()
    # data <- read.csv(file.path(exportFolder, file))
    # DatabaseConnector::insertTable(connection = conn,
    #                                tableName = tableName,
    #                                data = data,
    #                                dropTableIfExists = TRUE,
    #                                createTable = TRUE,
    #                                tempTable = FALSE,
    #                                progressBar = TRUE)
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
disconnect(conn)

filename <- file.path(exportFolder, "covariate_balance.csv" )
system.time(
x <- length(count.fields(filename, skip = 1))
)


system.time({
    testcon <- file(filename,open="r")
    readsizeof <- 100000
    nooflines <- 0
    ( while((linesread <- length(readLines(testcon,readsizeof))) > 0 )
        nooflines <- nooflines +linesread )
    close(testcon)
})

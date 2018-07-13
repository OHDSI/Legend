library(DatabaseConnector)
source("DataPulls.R")

connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = "localhost/ohdsi",
                                             user = "postgres",
                                             password = Sys.getenv("pwPostgres"),
                                             schema = "legend")
connection <- connect(connectionDetails)

exposures <- getExposures(connection)
outcomes <- getOutcomes(connection)

# Install historical version of dependencies for reproducability:
# devtools::install_github("ohdsi/CohortMethod", ref = "v3.1.0")
# devtools::install_github("ohdsi/EmpiricalCalibration", ref = "v1.4.0")


library(CensoringSensAnalysis)
options(fftempdir = "r:/fftemp")
maxCores <- parallel::detectCores()
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

targetId <- 1395058 # Chlorthalidone
comparatorId <- 974166 # Hydrochlorathiazide
newTargetId <- 15093
newComparatorId <- 15094


# CCAE settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_truven_ccae_v750.dbo"
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "mschuemie_temp"
outputFolder <- file.path(studyFolder, "ccae")
indicationFolder <- file.path(outputFolder, "Hypertension")
sensAnalysisFolder <- file.path(outputFolder, "sensAnalysis")

execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        oracleTempSchema = oracleTempSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        indicationFolder = indicationFolder,
        sensAnalysisFolder = sensAnalysisFolder)


CohortDiagnostics::launchCohortExplorer(connectionDetails = connectionDetails,
                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                        cohortDatabaseSchema = cohortDatabaseSchema,
                                        cohortTable = cohortTable,
                                        cohortId = newTargetId)

CohortDiagnostics::launchCohortExplorer(connectionDetails = connectionDetails,
                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                        cohortDatabaseSchema = cohortDatabaseSchema,
                                        cohortTable = cohortTable,
                                        cohortId = newComparatorId)


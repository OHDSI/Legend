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

library(Legend)
options(fftempdir = "r:/fftemp")
maxCores <- 30
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

# CCAE settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_truven_ccae_v750.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_ccae"
databaseId <- "CCAE"
databaseName <- "Truven Health MarketScan Commercial Claims and Encounters Database"
outputFolder <- file.path(studyFolder, "ccae")

# MDCD settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_truven_mdcd_v699.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_mdcd"
databaseId <- "MDCD"
databaseName <- "Truven Health MarketScan® Multi-State Medicaid Database"
outputFolder <- file.path(studyFolder, "mdcd")

# MDCR settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_truven_mdcr_v751.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_mdcr"
databaseId <- "MDCR"
databaseName <- "Truven Health MarketScan Medicare Supplemental and Coordination of Benefits Database"
outputFolder <- file.path(studyFolder, "mdcr")

# Optum settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_optum_extended_dod_v734.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_optum"
databaseId <- "Optum"
databaseName <- "Optum’s  Clinformatics® Extended Data Mart"
outputFolder <- file.path(studyFolder, "optum")

# Synpuf settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_synpuf_v667.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_synpuf"
databaseId <- "Synpuf"
databaseName <- "Medicare Claims Synthetic Public Use Files (SynPUFs)"
outputFolder <- file.path(studyFolder, "synpuf")


indicationId <- "Depression"

indicationId <- "Hypertension"

mailSettings <- list(from = Sys.getenv("mailAddress"),
                     to = c(Sys.getenv("mailAddress")),
                     smtp = list(host.name = "smtp.gmail.com", port = 465,
                                 user.name = Sys.getenv("mailAddress"),
                                 passwd = Sys.getenv("mailPassword"), ssl = TRUE),
                     authenticate = TRUE,
                     send = TRUE)

OhdsiRTools::runAndNotify({
    execute(connectionDetails = connectionDetails,
            cdmDatabaseSchema = cdmDatabaseSchema,
            oracleTempSchema = oracleTempSchema,
            cohortDatabaseSchema = cohortDatabaseSchema,
            outputFolder = outputFolder,
            indicationId = indicationId,
            tablePrefix = tablePrefix,
            createExposureCohorts = TRUE,
            createOutcomeCohorts = TRUE,
            fetchAllDataFromServer = TRUE,
            synthesizePositiveControls = TRUE,
            generateAllCohortMethodDataObjects = TRUE,
            runCohortMethod = TRUE,
            computeIncidence = TRUE,
            fetchChronographData = TRUE,
            computeCovariateBalance = TRUE,
            maxCores = maxCores)
},  mailSettings = mailSettings, label = "Legend")


createExposureCohorts(connectionDetails = connectionDetails,
                      cdmDatabaseSchema = cdmDatabaseSchema,
                      cohortDatabaseSchema = cohortDatabaseSchema,
                      tablePrefix = tablePrefix,
                      indicationId = indicationId,
                      oracleTempSchema = oracleTempSchema,
                      outputFolder = outputFolder)

filterByExposureCohortsSize(outputFolder = outputFolder,
                            indicationId = indicationId)

createOutcomeCohorts(connectionDetails = connectionDetails,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     cohortDatabaseSchema = cohortDatabaseSchema,
                     tablePrefix = tablePrefix,
                     indicationId = indicationId,
                     oracleTempSchema = oracleTempSchema,
                     outputFolder = outputFolder)




fetchAllDataFromServer(connectionDetails = connectionDetails,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       oracleTempSchema = oracleTempSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       tablePrefix = tablePrefix,
                       indicationId = indicationId,
                       outputFolder = outputFolder)

synthesizePositiveControls(connectionDetails = connectionDetails,
                           cdmDatabaseSchema = cdmDatabaseSchema,
                           oracleTempSchema = oracleTempSchema,
                           cohortDatabaseSchema = cohortDatabaseSchema,
                           tablePrefix = tablePrefix,
                           indicationId = indicationId,
                           outputFolder = outputFolder,
                           maxCores = maxCores)

generateAllCohortMethodDataObjects(outputFolder = outputFolder,
                                   indicationId = indicationId)

runCohortMethod(outputFolder = outputFolder,
                indicationId = indicationId,
                maxCores = maxCores)



computeIncidence(outputFolder = outputFolder,
                 indicationId = indicationId)

fetchChronographData(connectionDetails = connectionDetails,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     oracleTempSchema = oracleTempSchema,
                     cohortDatabaseSchema = cohortDatabaseSchema,
                     tablePrefix = tablePrefix,
                     indicationId = indicationId,
                     outputFolder = outputFolder)

computeCovariateBalance(outputFolder = outputFolder,
                        indicationId = indicationId,
                        maxCores = maxCores)

exportResults(outputFolder = outputFolder,
              databaseId = databaseId,
              databaseName = databaseName,
              minCellCount = 5,
              maxCores = maxCores)

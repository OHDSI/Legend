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
options(fftempdir = "c:/fftemp")  # Folder where temp files will be stored
maxCores <- 3                     # Max number of cores to use
studyFolder <- "c:/Legend"        # Main study folder

dbms <- "postgresql"              # Type of database plaform
user <- "postgres"                # Database server user name
pw <- Sys.getenv("pwPostgres")    # Database server password
server <- "localhost/ohdsi"       # Database server address
port <- 5432                      # Database server port
oracleTempSchema <- NULL          # When using Oracle: specify a folder where temporary tables can be created. Set to NULL for other platforms.

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Synpuf settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_synpuf" # Name of your CDM schema. For SQL Server, include both database and schema (e.g. "cdm.dbo").
cohortDatabaseSchema <- "scratch" # Name of a schema where intermediary results can be written. For SQL Server, include both database and schema (e.g. "scratch.dbo").
tablePrefix <- "legend"           # A prefix to be used for all names of tables created in the cohortDatabaseSchema.
databaseId <- "Synpuf"            # Some short name for the database.
outputFolder <- file.path(studyFolder, databaseId) # The subfolder in the main study folder where the results for this database will be written.

indicationId <- "Depression"

# Feasibility assessment: Phenotypes ---------------------------------------------
assessPhenotypes(connectionDetails = connectionDetails,
                 cdmDatabaseSchema = cdmDatabaseSchema,
                 oracleTempSchema = oracleTempSchema,
                 cohortDatabaseSchema = cohortDatabaseSchema,
                 outputFolder = outputFolder,
                 indicationId = indicationId,
                 tablePrefix = tablePrefix,
                 databaseId = databaseId)

# Feasibility assessment: Propensity models --------------------------------------
assessPropensityModels(connectionDetails = connectionDetails,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       oracleTempSchema = oracleTempSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       outputFolder = outputFolder,
                       indicationId = indicationId,
                       tablePrefix = tablePrefix,
                       databaseId = databaseId,
                       maxCores = maxCores)

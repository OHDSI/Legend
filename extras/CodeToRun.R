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
options(fftempdir = "c:/fftemp")
# options('fftempdir' = 'S:/fftemp')
maxCores <- 30
studyFolder <- "c:/Legend"
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
cdmDatabaseSchema <- "cdm_truven_ccae_v697.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_ccae"
databaseName <- "CCAE"
outputFolder <- file.path(studyFolder, "ccae")

# MDCD settings ----------------------------------------------------------------
cdmDatabaseSchema <- "CDM_Truven_MDCD_V699.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_mdcd"
databaseName <- "MDCD"
outputFolder <- file.path(studyFolder, "mdcd")

# MDCR settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_truven_mdcr_v698.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_mdcr"
databaseName <- "MDCR"
outputFolder <- file.path(studyFolder, "mdcr")

# Optum settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_optum_extended_dod_v695.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_optum"
databaseName <- "Optum"
outputFolder <- file.path(studyFolder, "optum")

# Synpuf settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_synpuf_v667.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_synpuf"
databaseName <- "Synpuf"
outputFolder <- file.path(studyFolder, "synpuf")


indication <- "Depression"


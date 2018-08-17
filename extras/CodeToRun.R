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
Sys.setenv(USE_MPP_BULK_LOAD = "TRUE")

indicationId <- "Depression"

indicationId <- "Hypertension"

# CCAE settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_truven_ccae_v750.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_ccae"
databaseId <- "CCAE"
databaseName <- "Truven Health MarketScan Commercial Claims and Encounters Database"
databaseDescription <- "Truven Health MarketScan® Commercial Claims and Encounters Database (CCAE) represent data from individuals enrolled in United States employer-sponsored insurance health plans. The data includes adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy) as well as enrollment data from large employers and health plans who provide private healthcare coverage to employees, their spouses, and dependents. Additionally, it captures laboratory tests for a subset of the covered lives. This administrative claims database includes a variety of fee-for-service, preferred provider organizations, and capitated health plans."
outputFolder <- file.path(studyFolder, "ccae")

# MDCD settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_truven_mdcd_v699.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_mdcd"
databaseId <- "MDCD"
databaseName <- "Truven Health MarketScan® Multi-State Medicaid Database"
databaseDescription <- "Truven Health MarketScan® Multi-State Medicaid Database (MDCD) adjudicated US health insurance claims for Medicaid enrollees from multiple states and includes hospital discharge diagnoses, outpatient diagnoses and procedures, and outpatient pharmacy claims as well as ethnicity and Medicare eligibility. Members maintain their same identifier even if they leave the system for a brief period however the dataset lacks lab data. [For further information link to RWE site for Truven MDCD."
outputFolder <- file.path(studyFolder, "mdcd")

# MDCR settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_truven_mdcr_v751.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_mdcr"
databaseId <- "MDCR"
databaseName <- "Truven Health MarketScan Medicare Supplemental and Coordination of Benefits Database"
databaseDescription <- "Truven Health MarketScan® Medicare Supplemental and Coordination of Benefits Database (MDCR) represents health services of retirees in the United States with primary or Medicare supplemental coverage through privately insured fee-for-service, point-of-service, or capitated health plans. These data include adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy). Additionally, it captures laboratory tests for a subset of the covered lives."
outputFolder <- file.path(studyFolder, "mdcr")

# Optum settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_optum_extended_dod_v734.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_optum"
databaseId <- "Optum"
databaseName <- "Optum’s Clinformatics® Extended Data Mart"
databaseDescription <- "Optum Clinformatics Extended DataMart is an adjudicated US administrative health claims database for members of private health insurance, who are fully insured in commercial plans or in administrative services only (ASOs), Legacy Medicare Choice Lives (prior to January 2006), and Medicare Advantage (Medicare Advantage Prescription Drug coverage starting January 2006). The population is primarily representative of commercial claims patients (0-65 years old) with some Medicare (65+ years old) however ages are capped at 90 years. It includes data captured from administrative claims processed from inpatient and outpatient medical services and prescriptions as dispensed, as well as results for outpatient lab tests processed by large national lab vendors who participate in data exchange with Optum. This dataset also provides date of death (month and year only) for members with both medical and pharmacy coverage from the Social Security Death Master File (however after 2011 reporting frequency changed due to changes in reporting requirements) and location information for patients is at the US state level."
outputFolder <- file.path(studyFolder, "optum")

# Synpuf settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_synpuf_v667.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_synpuf"
databaseId <- "Synpuf"
databaseName <- "Medicare Claims Synthetic Public Use Files (SynPUFs)"
databaseDescription <- "Medicare Claims Synthetic Public Use Files (SynPUFs) were created to allow interested parties to gain familiarity using Medicare claims data while protecting beneficiary privacy. These files are intended to promote development of software and applications that utilize files in this format, train researchers on the use and complexities of Centers for Medicare and Medicaid Services (CMS) claims, and support safe data mining innovations. The SynPUFs were created by combining randomized information from multiple unique beneficiaries and changing variable values. This randomization and combining of beneficiary information ensures privacy of health information."
outputFolder <- file.path(studyFolder, "synpuf")



# Feasibility assessment ---------------------------------------------------------
assessPhenotypes(connectionDetails = connectionDetails,
                 cdmDatabaseSchema = cdmDatabaseSchema,
                 oracleTempSchema = oracleTempSchema,
                 cohortDatabaseSchema = cohortDatabaseSchema,
                 outputFolder = outputFolder,
                 indicationId = indicationId,
                 tablePrefix = tablePrefix,
                 databaseId = databaseId)

assessPropensityModels(connectionDetails = connectionDetails,
                       cdmDatabaseSchema = cdmDatabaseSchema,
                       oracleTempSchema = oracleTempSchema,
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       outputFolder = outputFolder,
                       indicationId = indicationId,
                       tablePrefix = tablePrefix,
                       databaseId = databaseId,
                       maxCores = maxCores)

# Run main study -----------------------------------------------------------------
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
            createExposureCohorts = FALSE,
            createOutcomeCohorts = FALSE,
            fetchAllDataFromServer = FALSE,
            synthesizePositiveControls = FALSE,
            generateAllCohortMethodDataObjects = TRUE,
            runCohortMethod = TRUE,
            computeIncidence = TRUE,
            fetchChronographData = TRUE,
            computeCovariateBalance = TRUE,
            maxCores = maxCores)
}, mailSettings = mailSettings, label = "Legend")


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
                                   indicationId = indicationId,
                                   maxCores = maxCores)

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

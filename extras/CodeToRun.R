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

# This code is used to run LEGEND on various databases.

library(Legend)
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
# Choose one:

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

# CPRD settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_cprd_v730.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_cprd"
databaseId <- "CPRD"
databaseName <- "Clinical Practice Research Datalink"
databaseDescription <- "The Clinical Practice Research Datalink (CPRD) is a governmental, not-for-profit research service, jointly funded by the NHS National Institute for Health Research (NIHR) and the Medicines and Healthcare products Regulatory Agency (MHRA), a part of the Department of Health, United Kingdom (UK). CPRD consists of data collected from UK primary care for all ages. This includes conditions, observations, measurements, and procedures that the general practitioner is made aware of in additional to any prescriptions as prescribed by the general practitioner. In addition to primary care, there are also linked secondary care records for a small number of people.\nThe major data elements contained within this database are outpatient prescriptions given by the general practitioner (coded with Multilex codes) and outpatient clinical, referral, immunization or test events that the general practitioner knows about (coded in Read or ICD10 or LOINC codes). The database also contains the patients’ year of births and any date of deaths."
outputFolder <- file.path(studyFolder, "cprd")

# IMS Germany settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_ims_germany_da_v689.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_imsg"
databaseId <- "IMSG"
databaseName <- "QuintilesIMS Disease Analyzer (DA) Germany"
databaseDescription <- "The QuintilesIMS Disease Analyzer (DA) Germany database consists of data collected from physician practices and medical centers for all ages. Mostly primary care physician data however some data from specialty practices (where practices are electronically connected to each other) and some lab data is included. Key attributes include demographics, prescriptions as prescribed at brand level, diagnosis, lab measurements, actions (e.g. referrals, sick notes)."
outputFolder <- file.path(studyFolder, "imsg")

# JMDC settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_jmdc_v715.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_jmdc"
databaseId <- "JMDC"
databaseName <- "Japan Medical Data Center"
databaseDescription <- "Japan Medical Data Center (JDMC) database consists of data from 60 Society-Managed Health Insurance plans covering workers aged 18 to 65 and their dependents (children younger than 18 years old and elderly people older than 65 years old). JMDC data includes membership status of the insured people and claims data provided by insurers under contract (e.g. patient-level demographic information, inpatient and outpatient data inclusive of diagnosis and procedures, and prescriptions as dispensed claims information). Claims data are derived from monthly claims issued by clinics, hospitals and community pharmacies; for claims only the month and year are provided however prescriptions, procedures, admission, discharge, and start of medical care as associated with a full date.\nAll diagnoses are coded using ICD-10. All prescriptions refer to national Japanese drug codes, which have been linked to ATC. Procedures are encoded using local procedure codes, which the vendor has mapped to ICD-9 procedure codes. The annual health checkups report a standard battery of measurements (e.g. BMI), which are not coded but clearly described."
outputFolder <- file.path(studyFolder, "jmdc")

# Optum Panther settings ----------------------------------------------------------------
cdmDatabaseSchema <- "cdm_optum_panther_v735.dbo"
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "legend_panther"
databaseId <- "Panther"
databaseName <- "Optum© de-identified Electronic Health Record Dataset"
databaseDescription <- "Optum© de-identified Electronic Health Record Dataset represents Humedica’s Electronic Health Record data a medical records database. The medical record data includes clinical information, inclusive of prescriptions as prescribed and administered, lab results, vital signs, body measurements, diagnoses, procedures, and information derived from clinical Notes using Natural Language Processing (NLP)."
outputFolder <- file.path(studyFolder, "panther")

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
execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        oracleTempSchema = oracleTempSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        outputFolder = outputFolder,
        indicationId = indicationId,
        databaseId = databaseId,
        databaseName = databaseName,
        databaseDescription = databaseDescription,
        tablePrefix = tablePrefix,
        createExposureCohorts = FALSE,
        createOutcomeCohorts = FALSE,
        fetchAllDataFromServer = FALSE,
        synthesizePositiveControls = FALSE,
        generateAllCohortMethodDataObjects = FALSE,
        runCohortMethod = TRUE,
        computeIncidence = FALSE,
        fetchChronographData = FALSE,
        computeCovariateBalance = TRUE,
        exportToCsv = TRUE,
        maxCores = maxCores)

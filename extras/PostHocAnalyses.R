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


# Censor at initiation of ACIi when looking at angioedema -------------------------
indicationFolder <- file.path(outputFolder, indicationId)
cmFolder <- file.path(indicationFolder, "cmOutput")
outcomeModelReference <- readRDS(file.path(indicationFolder,
                                            "cmOutput",
                                            "outcomeModelReference1.rds"))
targetId <- 4
comparatorId <- 15 # THZ
# comparatorId <- 10 # dCCB
outcomeId <- 32
censorId <- 1
analysisId <- 1

# Get censor dates from database
pairedCohortTable <- paste(tablePrefix, tolower(indicationId), "pair_cohort", sep = "_")
pathToCsv <- system.file("settings", "ExposuresOfInterest.csv", package = "Legend")
exposuresOfInterest <- read.csv(pathToCsv)
exposuresOfInterest <- exposuresOfInterest[exposuresOfInterest$indicationId == indicationId, ]
# exposuresOfInterest <- exposuresOfInterest[exposuresOfInterest$cohortId == censorId, ]
censorDescendantIds <- exposuresOfInterest$includedConceptIds
censorDescendantIds <- as.numeric(unlist(strsplit(as.character(censorDescendantIds), ";")))
censorDescendantIds <- c(censorDescendantIds,
                         exposuresOfInterest$conceptId[exposuresOfInterest$includedConceptIds == ""])
censorDescendantIds <- unique(censorDescendantIds)
sql <- "SELECT DATEDIFF(DAY, cohort_start_date, MIN(drug_era_start_date)) AS days_to_censor,
  cohort_start_date,
  subject_id
FROM @cohort_database_schema.@paired_cohort_table e
INNER JOIN @cdm_database_schema.drug_era de
ON e.subject_id = de.person_id
  AND drug_era_start_date > cohort_start_date
WHERE e.target_id = @target_id
  AND e.comparator_id = @comparator_id
  AND drug_concept_id IN(@censor_descendant_ids)
GROUP BY subject_id,
  cohort_start_date;
"
sql <- SqlRender::renderSql(sql,
                            cohort_database_schema = cohortDatabaseSchema,
                            paired_cohort_table = pairedCohortTable,
                            cdm_database_schema = cdmDatabaseSchema,
                            target_id = targetId,
                            comparator_id = comparatorId,
                            censor_descendant_ids = censorDescendantIds)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms, oracleTempSchema = oracleTempSchema)$sql
conn <- DatabaseConnector::connect(connectionDetails)
censorDates <- DatabaseConnector::querySql(conn, sql)
DatabaseConnector::disconnect(conn)
colnames(censorDates) <- SqlRender::snakeCaseToCamelCase(colnames(censorDates))
saveRDS(censorDates, file.path(indicationFolder, "censorDates.rds"))

# Create new strataPop object with censoring
censorDates <- readRDS(file.path(indicationFolder, "censorDates.rds"))
omr <- outcomeModelReference[outcomeModelReference$targetId == targetId &
                                 outcomeModelReference$comparatorId == comparatorId &
                                 outcomeModelReference$outcomeId == outcomeId &
                                 outcomeModelReference$analysisId == analysisId, ]
cmData <- CohortMethod::loadCohortMethodData(file.path(cmFolder, omr$cohortMethodDataFolder))
cohorts <- cmData$cohorts[, c("rowId", "subjectId", "cohortStartDate")]
censorDates <- merge(censorDates, cohorts)

stratPop <- readRDS(file.path(cmFolder, omr$strataFile))
stratPop <- merge(stratPop, censorDates[, c("rowId", "daysToCensor")], all.x = TRUE)
stratPop$daysToCensor[is.na(stratPop$daysToCensor)] <- 999999999
idx <- stratPop$survivalTime > stratPop$daysToCensor #+ 1
sum(stratPop$treatment[idx] == 1) / sum(stratPop$treatment == 1)
sum(stratPop$treatment[idx] == 0) / sum(stratPop$treatment == 0)
sum(stratPop$outcomeCount[idx] != 0) / sum(stratPop$outcomeCount != 0)

stratPop$survivalTime[idx] <- stratPop$daysToCensor[idx] #+ 1
stratPop$outcomeCount[idx] <- 0

om <- CohortMethod::fitOutcomeModel(population = stratPop, modelType = "cox", stratified = TRUE)
om
summary(om)
stratPop$stratumId <- NULL
CohortMethod::plotKaplanMeier(stratPop)

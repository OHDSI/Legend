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

# This is code that is used by the LEGEND coordinating center, for example to upload the results
# to the LEGEND database, and to perform meta-analyses across databases.

library(Legend)
connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = paste(Sys.getenv("legendServer"),
                                                            Sys.getenv("legendDatabase"),
                                                            sep = "/"),
                                             port = Sys.getenv("legendPort"),
                                             user = Sys.getenv("legendUser"),
                                             password = Sys.getenv("legendPw"),
                                             schema = Sys.getenv("legendSchema"))
maxCores <- parallel::detectCores()

# Upload data from a database for an indication -----------------------------------
exportFolder <- "R:/Legend/exports/Hypertension/Panther"

uploadResultsToDatabase(connectionDetails = connectionDetails,
                        exportFolder = exportFolder,
                        createTables = FALSE,
                        staging = FALSE,
                        deletePriorData = TRUE,
                        skipBigTables = TRUE)



# Perform meta-analysis across databases ------------------------------------------
exportFolders <- c("R:/Legend/exports/Hypertension/Panther",
                   "R:/Legend/exports/Hypertension/Optum",
                   "R:/Legend/exports/Hypertension/Ccae_part1",
                   "R:/Legend/exports/Hypertension/Ccae_part2",
                   "R:/Legend/exports/Hypertension/Ccae_part3",
                   "R:/Legend/exports/Hypertension/Mdcd",
                   "R:/Legend/exports/Hypertension/Mdcr",
                   "R:/Legend/exports/Hypertension/Jmdc",
                   "R:/Legend/exports/Hypertension/Imsg",
                   "R:/Legend/exports/Hypertension/Cumc",
                   "R:/Legend/exports/Hypertension/NHIS_NSC")

maExportFolder <- "R:/Legend/exports/Hypertension/MetaAnalysis"

doMetaAnalysis(exportFolders = exportFolders,
               maExportFolder = maExportFolder,
               maxCores = maxCores)

# Assorted database maintenance queries -------------------------------------------
conn <- connect(connectionDetails)
querySql(conn, "SELECT * FROM pg_indexes WHERE schemaname = 'legend';")
querySql(conn, "REINDEX INDEX idx_kaplan_meier_dist;")
querySql(conn, "SELECT DISTINCT database_id FROM cohort_method_result;")
querySql(conn, "SELECT DISTINCT database_id FROM cohort_method_result_staging;")

# Get database size:
sql <- "select t1.datname AS db_name,
       pg_size_pretty(pg_database_size(t1.datname)) as db_size
from pg_database t1
where t1.datname <> 'rdsadmin'
order by pg_database_size(t1.datname) desc;"
querySql(conn, sql)

querySql(conn, "SELECT COUNT(*), database_id FROM cohort_method_result GROUP BY database_id;")
querySql(conn, "SELECT COUNT(*), database_id FROM cohort_method_result_staging GROUP BY database_id;")

querySql(conn, "SELECT COUNT(*), database_id FROM PREFERENCE_SCORE_DIST_STAGING GROUP BY database_id;")

querySql(conn, "SELECT * FROM outcome_of_interest WHERE outcome_id IN (39, 40, 44);")

querySql(conn, "SELECT * FROM cohort_method_result WHERE database_id = 'CCAE'
         AND target_id = 17 LIMIT 100;")

# executeSql(conn, "DROP INDEX idx_covariate_balance")

executeSql(conn, "ALTER TABLE database_staging ALTER COLUMN description TYPE text;")

# executeSql(conn, "GRANT SELECT ON cm_interaction_result TO legend;")


# Drop intereraction and chronograph data:
# executeSql(conn, "DROP TABLE cm_interaction_result;")
# executeSql(conn, "DROP TABLE chronograph;")
querySql(conn, "SELECT COUNT(*) FROM covariate_balance WHERE interaction_covariate_id IS NOT NULL;")
executeSql(conn, "DELETE FROM covariate_balance WHERE interaction_covariate_id IS NOT NULL;")
executeSql(conn, "ALTER TABLE covariate_balance DROP COLUMN interaction_covariate_id;")
querySql(conn, "SELECT COUNT(*) FROM incidence WHERE interaction_covariate_id IS NOT NULL;")
executeSql(conn, "DELETE FROM incidence WHERE interaction_covariate_id IS NOT NULL;")
executeSql(conn, "ALTER TABLE incidence DROP COLUMN interaction_covariate_id;")

executeSql(conn, "DELETE FROM covariate_balance WHERE database_id = 'Panther';")


# executeSql(conn, "DELETE FROM kaplan_meier_dist WHERE database_id = 'Panther';")
# executeSql(conn, "ALTER TABLE cohort_method_analysis ALTER COLUMN definition TYPE text;")
executeSql(conn, "ALTER TABLE covariate_staging ALTER COLUMN covariate_name TYPE text;")

executeSql(conn, "ALTER TABLE cohort_method_result_staging ALTER COLUMN target_days TYPE bigint;")
executeSql(conn, "ALTER TABLE cohort_method_result_staging ALTER COLUMN comparator_days TYPE bigint;")


# executeSql(conn, "DELETE FROM covariate_balance WHERE database_id = 'Panther';")
# executeSql(conn, "DELETE FROM kaplan_meier_dist WHERE database_id = 'Panther';")

# Create exposure_ids table ------------------------------------------
executeSql(conn, "DROP TABLE exposure_ids;")

executeSql(conn, "CREATE TABLE exposure_ids AS SELECT DISTINCT exposure_id FROM (
      SELECT DISTINCT target_id AS exposure_id FROM cohort_method_result
      UNION ALL
      SELECT DISTINCT comparator_id AS exposure_id FROM cohort_method_result
    ) tmp;")


# Unstage --------------------------------------------------------------------
unstageDb(connectionDetails = connectionDetails,
          schema = Sys.getenv("legendSchema"))


# Fix CCAE partial upload -------------------------------------
tables <- c("exposure_summary", "comparison_summary", "attrition", "cm_follow_up_dist", "cohort_method_result")
dbs <- c("R:/Legend/exports/Hypertension/Ccae_part1",
"R:/Legend/exports/Hypertension/Ccae_part2",
"R:/Legend/exports/Hypertension/Ccae_part3")
for (db in dbs) {
    files <- list.files(db, ".csv$", full.names = TRUE)
    unlink(files)
}
for (db in dbs) {
    zipFile <- list.files(db, ".zip$", full.names = TRUE)[1]
    utils::unzip(zipfile = zipFile, files = paste0(tables, ".csv"), exdir = db)
}

exportFolder <- dbs[3]
deletePriorData <- FALSE

# Drop deleted outcomes ---------------------------------------------------------


executeSql(conn, "DELETE FROM OUTCOME_OF_INTEREST WHERE outcome_id IN (39,40,44);")
executeSql(conn, "DELETE FROM CM_FOLLOW_UP_DIST WHERE outcome_id IN (39,40,44);")
executeSql(conn, "DELETE FROM COHORT_METHOD_RESULT WHERE outcome_id IN (39,40,44);")
executeSql(conn, "DELETE FROM covariate_balance WHERE outcome_id IN (39,40,44);")
executeSql(conn, "DELETE FROM incidence WHERE outcome_id IN (39,40,44);")
executeSql(conn, "DELETE FROM kaplan_meier_dist WHERE outcome_id IN (39,40,44);")


# Fix reversed PS plots -------------------------------------------------------
exportFolders <- c("R:/Legend/exports/Hypertension/Mdcd",
                   "R:/Legend/exports/Hypertension/Panther",
                   "R:/Legend/exports/Hypertension/Optum",
                   "R:/Legend/exports/Hypertension/Ccae_part1",
                   "R:/Legend/exports/Hypertension/Ccae_part2",
                   "R:/Legend/exports/Hypertension/Ccae_part3",
                   "R:/Legend/exports/Hypertension/Mdcr",
                   "R:/Legend/exports/Hypertension/Jmdc",
                   "R:/Legend/exports/Hypertension/Imsg")
for (i in 1:length(exportFolders)) {
    # i = 1
    exportFolder <- exportFolders[i]
    writeLines(paste("Fixing reversed PS plots in", exportFolder))
    csvFiles <- list.files(exportFolder, ".csv$", full.names = TRUE)
    unlink(csvFiles)
    zipFile <- list.files(exportFolder, ".zip$", full.names = TRUE)[1]
    utils::unzip(zipfile = zipFile, files = "preference_score_dist.csv", exdir = exportFolder)
    ps <- read.csv(file.path(exportFolder, "preference_score_dist.csv"))
    ps$preference_score[ps$target_id > ps$comparator_id] <- 1 - ps$preference_score[ps$target_id > ps$comparator_id]
    write.csv(ps, file.path(exportFolder, "preference_score_dist.csv"), row.names = FALSE)
}

for (i in 1:length(exportFolders)) {
    exportFolder <- exportFolders[i]
    writeLines(paste("Uploading fixed PS plots in", exportFolder))
    deletePriorData <- TRUE
    if (grepl("_part[^1]", exportFolder)) {
        deletePriorData <- FALSE
    }
    uploadResultsToDatabase(connectionDetails = connectionDetails,
                            exportFolder = exportFolder,
                            createTables = FALSE,
                            staging = FALSE,
                            deletePriorData = deletePriorData)
}

# Reupload one table ------------------------------------------
# tableName <- "kaplan_meier_dist.csv"
tableName <- "covariate_balance.csv"
# exportFolders <- c("R:/Legend/exports/Hypertension/Optum",
#                    "R:/Legend/exports/Hypertension/Ccae_part1",
#                    "R:/Legend/exports/Hypertension/Ccae_part2",
#                    "R:/Legend/exports/Hypertension/Ccae_part3",
#                    "R:/Legend/exports/Hypertension/Mdcd",
#                    "R:/Legend/exports/Hypertension/Mdcr",
#                    "R:/Legend/exports/Hypertension/Jmdc",
#                    "R:/Legend/exports/Hypertension/Imsg",
#                    "R:/Legend/exports/Hypertension/Cumc",
#                    "R:/Legend/exports/Hypertension/NHIS_NSC")
exportFolders <- c("R:/Legend/exports/Hypertension/Optum")


for (i in 1:length(exportFolders)) {
    exportFolder <- exportFolders[i]

    writeLines(paste("Extracting", tableName, "in", exportFolder))
    csvFiles <- list.files(exportFolder, ".csv$", full.names = TRUE)
    unlink(csvFiles)
    zipFile <- list.files(exportFolder, ".zip$", full.names = TRUE)[1]
    utils::unzip(zipfile = zipFile, files = tableName, exdir = exportFolder)

    deletePriorData <- TRUE
    if (grepl("_part[^1]", exportFolder)) {
      deletePriorData <- FALSE
    }
    writeLines(paste("Uploading data from ", exportFolder, "to", tableName))
    uploadResultsToDatabase(connectionDetails = connectionDetails,
                            exportFolder = exportFolder,
                            createTables = FALSE,
                            staging = FALSE,
                            deletePriorData = deletePriorData,
                            skipBigTables = FALSE)

    writeLines(paste("Cleaning up CSV files in  ", exportFolder))
    csvFiles <- list.files(exportFolder, ".csv$", full.names = TRUE)
    unlink(csvFiles)
}


exportFolder <- "s:/temp/"
file <- "covariate_balance.csv"
createTables = FALSE
staging = FALSE
deletePriorData = FALSE
skipBigTables = FALSE

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
exportFolder <- "R:/Legend/exports/Hypertension/Mdcr"

uploadResultsToDatabase(connectionDetails = connectionDetails,
                        exportFolder = exportFolder,
                        createTables = FALSE,
                        staging = FALSE,
                        deletePriorData = TRUE)



# Perform meta-analysis across databases ------------------------------------------
exportFolders <- c("R:/Legend/exports/Hypertension/Panther",
                   "R:/Legend/exports/Hypertension/Optum",
                   "R:/Legend/exports/Hypertension/Ccae_part1",
                   "R:/Legend/exports/Hypertension/Ccae_part2",
                   "R:/Legend/exports/Hypertension/Ccae_part3",
                   "R:/Legend/exports/Hypertension/Mdcd",
                   "R:/Legend/exports/Hypertension/Mdcr",
                   "R:/Legend/exports/Hypertension/Jmdc",
                   "R:/Legend/exports/Hypertension/Imsg")

maExportFolder <- "R:/Legend/exports/Hypertension/MetaAnalysis"

doMetaAnalysis(exportFolders = exportFolders,
               maExportFolder = maExportFolder,
               maxCores = maxCores)

# Assorted database maintenance queries -------------------------------------------
conn <- connect(connectionDetails)
querySql(conn, "SELECT * FROM pg_indexes WHERE schemaname = 'legend';")
querySql(conn, "REINDEX INDEX idx_kaplan_meier_dist;")
querySql(conn, "SELECT DISTINCT database_id FROM cohort_method_result;")
querySql(conn, "SELECT * FROM cohort_method_result WHERE database_id = 'CCAE'
         AND target_id = 17 LIMIT 100;")

# executeSql(conn, "GRANT SELECT ON cm_interaction_result TO legend;")
# executeSql(conn, "DROP TABLE cm_interaction_result;")
executeSql(conn, "DELETE FROM kaplan_meier_dist WHERE database_id = 'MDCR';")
# executeSql(conn, "ALTER TABLE cohort_method_analysis ALTER COLUMN definition TYPE text;")
executeSql(conn, "ALTER TABLE covariate_staging ALTER COLUMN covariate_name TYPE text;")

executeSql(conn, "ALTER TABLE cohort_method_result_staging ALTER COLUMN target_days TYPE bigint;")
executeSql(conn, "ALTER TABLE cohort_method_result_staging ALTER COLUMN comparator_days TYPE bigint;")


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

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
# limitations under the License.library(DatabaseConnector)

#' Upload results to the database server
#'
#' @description
#' Upload the exported results of a database-indication run into the database. Assumes the data conforms to
#' the LEGEND data model.
#'
#' @param connectionDetails                    An object of type \code{connectionDetails} as created
#'                                             using the
#'                                             \code{\link[DatabaseConnector]{createConnectionDetails}}
#'                                             function in the DatabaseConnector package.
#' @param exportFolder                         Name of local folder where the export results are stored; make sure to use
#'                                             forward slashes (/). Do not use a folder on a network
#'                                             drive since this greatly impacts performance.
#' @param createTables                         Create the tables on the server? This will drop the tables
#'                                             if they already exist prior to creating them.
#' @param staging                              If TRUE, the table names on the server will have the postfix "_staging" to
#'                                             allow new tables to be created while old ones are in use.
#'
#' @export
uploadResultsToDatabase <- function(connectionDetails,
                                    exportFolder,
                                    createTables = FALSE,
                                    staging = FALSE,
                                    deletePriorData = TRUE) {
    conn <- connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(conn))
    batchSize <- 1e+07

    ParallelLogger::addDefaultFileLogger(file.path(exportFolder, "uploadLog.txt"))
    files <- list.files(exportFolder, pattern = ".*csv")

    infinityToNa <- function(data) {
        # Replace infinity with NA, because OHDSI Postgres server doesn't like infinity:
        if (nrow(data) > 0) {
            for (j in 1:ncol(data)) {
                if (is.numeric(data[, j])) {
                    idx <- is.infinite(data[, j])
                    if (any(idx)) {
                        data[idx, j] <- NA
                    }
                }
            }
        }
        return(data)
    }

    keys <- list(indication = c("indication_id"),
                 cohort_method_analysis = c("analysis_id"),
                 covariate_analysis = c("covariate_analysis_id"),
                 incidence_analysis = c("incidence_analysis_id"),
                 single_exposure_of_interest = c("exposure_id"),
                 combi_exposure_of_interest =  c("exposure_id"),
                 exposure_group =  c("exposure_id", "exposure_group"),
                 outcome_of_interest =  c("outcome_id"),
                 negative_control_outcome = c("outcome_id"),
                 positive_control_outcome = c("outcome_id"),
                 database = c("database_id"),
                 exposure_summary = c("database_id"),
                 comparison_summary = c("database_id"),
                 attrition = c("database_id"),
                 cm_follow_up_dist = c("database_id"),
                 covariate = c("database_id"),
                 cohort_method_result = c("database_id"),
                 cm_interaction_result = c("database_id"),
                 chronograph = c("database_id"),
                 incidence = c("database_id"),
                 covariate_balance = c("database_id"),
                 preference_score_dist = c("database_id"),
                 kaplan_meier_dist = c("database_id"),
                 propensity_model = c("database_id"))

    deleteExistingData <- function(data, tableName, sqlTableName, dropped) {
        key <- keys[[tableName]]
        toDrop <- unique(data[, key, drop = FALSE])
        if (!is.null(toDrop$database_id)) {
            toDrop$database_id <- as.character(toDrop$database_id)
        }
        if (!is.null(toDrop$exposure_group)) {
            toDrop$exposure_group <- as.character(toDrop$exposure_group)
        }
        if (!is.null(dropped)) {
            dropped$alreadyDropped <- TRUE
            toDrop <- merge(toDrop, dropped, all.x = TRUE)
            toDrop <- toDrop[is.na(toDrop$alreadyDropped), ]
            toDrop$alreadyDropped <- NULL
            dropped$alreadyDropped <- NULL
        }
        if (nrow(toDrop) > 0) {
            createSqlStatement <- function(i) {
                sql <- paste0("DELETE FROM ",
                              sqlTableName,
                              " WHERE ",
                              paste(paste0(key, " = '", toDrop[i, ], "'"), collapse = " AND "),
                              ";")
            }
            sql <- sapply(1:nrow(toDrop), createSqlStatement)
            sql <- paste(sql, collapse = "\n")
            sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
            executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)
        }
        dropped <- unique(rbind(dropped, toDrop))
        return(dropped)
    }

    for (i in 1:length(files)) {
        file <- files[i]
        tableName <- gsub(".csv$", "", file)
        if (staging) {
            sqlTableName <- paste0(tableName, "_staging")
        } else {
            sqlTableName <- tableName
        }
        ParallelLogger::logInfo("Uploading ", file, " to ", sqlTableName)
        start <- Sys.time()
        fileCon <- file(file.path(exportFolder, file), "r")
        data <- read.csv(fileCon, nrows = batchSize)
        columnNames <- colnames(data)
        data <- infinityToNa(data)
        first <- createTables
        insertedCount <- 0
        dropped <- NULL
        while (nrow(data) != 0) {
            if (!createTables && deletePriorData) {
                ParallelLogger::logInfo("- Deleting existing data with same keys")
                dropped <- deleteExistingData(data = data,
                                              tableName = tableName,
                                              sqlTableName = sqlTableName,
                                              dropped = dropped)
            }
            ParallelLogger::logInfo("- Inserting data")
            DatabaseConnector::insertTable(connection = conn,
                                           tableName = sqlTableName,
                                           data = data,
                                           dropTableIfExists = first,
                                           createTable = first,
                                           tempTable = FALSE)
            insertedCount <- insertedCount + nrow(data)
            ParallelLogger::logInfo(paste("- Inserted ", insertedCount, "rows"))
            data <- read.csv(fileCon, nrows = batchSize, header = FALSE, col.names = columnNames)
            data <- infinityToNa(data)
            first <- FALSE
        }
        close(fileCon)

        delta <- Sys.time() - start
        ParallelLogger::logInfo(paste("- Uploading took", signif(delta, 3), attr(delta, "units")))
    }
}

#' Create indices in the LEGEND result database
#'
#' @description
#' Create indices on the large tables in the LEGEND evidence model for faster searching.
#'
#' @param connectionDetails                    An object of type \code{connectionDetails} as created
#'                                             using the
#'                                             \code{\link[DatabaseConnector]{createConnectionDetails}}
#'                                             function in the DatabaseConnector package.
#' @param staging                              If TRUE, the table names on the server will have the postfix "_staging" to
#'                                             allow new tables to be created while old ones are in use.
#'
#' @export
createIndicesOnDatabase <- function(connectionDetails,
                                    staging = FALSE) {
    conn <- connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(conn))

    sql <- "CREATE INDEX idx_attrition ON attrition (database_id, exposure_id, target_id, comparator_id, outcome_id, analysis_id);"
    if (staging) {
        sql <- gsub(" \\(", "_staging (", gsub(" ON ", "_staging ON ", sql))
    }
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

    sql <- "CREATE INDEX idx_covariate ON covariate (database_id, covariate_id);"
    if (staging) {
        sql <- gsub(" \\(", "_staging (", gsub(" ON ", "_staging ON ", sql))
    }
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

    sql <- "CREATE INDEX idx_cm_follow_up_dist ON cm_follow_up_dist (database_id, target_id, comparator_id, outcome_id, analysis_id);"
    if (staging) {
        sql <- gsub(" \\(", "_staging (", gsub(" ON ", "_staging ON ", sql))
    }
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

    sql <- "CREATE INDEX idx_cohort_method_result ON cohort_method_result (database_id, target_id, comparator_id, outcome_id, analysis_id);"
    if (staging) {
        sql <- gsub(" \\(", "_staging (", gsub(" ON ", "_staging ON ", sql))
    }
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

    sql <- "CREATE INDEX idx_t_cohort_method_result ON cohort_method_result (target_id);"
    if (staging) {
        sql <- gsub(" \\(", "_staging (", gsub(" ON ", "_staging ON ", sql))
    }
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

    sql <- "CREATE INDEX idx_c_cohort_method_result ON cohort_method_result (comparator_id);"
    if (staging) {
        sql <- gsub(" \\(", "_staging (", gsub(" ON ", "_staging ON ", sql))
    }
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

    sql <- "CREATE INDEX idx_cm_interaction_result ON cm_interaction_result (database_id, target_id, comparator_id, outcome_id, analysis_id);"
    if (staging) {
        sql <- gsub(" \\(", "_staging (", gsub(" ON ", "_staging ON ", sql))
    }
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

    sql <- "CREATE INDEX idx_covariate_balance ON covariate_balance (database_id, target_id, comparator_id);"
    if (staging) {
        sql <- gsub(" \\(", "_staging (", gsub(" ON ", "_staging ON ", sql))
    }
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

    sql <- "CREATE INDEX idx_preference_score_dist ON preference_score_dist (database_id, target_id, comparator_id);"
    if (staging) {
        sql <- gsub(" \\(", "_staging (", gsub(" ON ", "_staging ON ", sql))
    }
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

    sql <- "CREATE INDEX idx_kaplan_meier_dist ON kaplan_meier_dist (database_id, target_id, comparator_id, outcome_id, analysis_id);"
    if (staging) {
        sql <- gsub(" \\(", "_staging (", gsub(" ON ", "_staging ON ", sql))
    }
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)

    sql <- "CREATE INDEX idx_propensity_model ON propensity_model (database_id, target_id, comparator_id);"
    if (staging) {
        sql <- gsub(" \\(", "_staging (", gsub(" ON ", "_staging ON ", sql))
    }
    DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)
}

#' Drop the old tables, and remove _staging postfix from staging tables.
#'
#' @param connectionDetails                    An object of type \code{connectionDetails} as created
#'                                             using the
#'                                             \code{\link[DatabaseConnector]{createConnectionDetails}}
#'                                             function in the DatabaseConnector package.
#' @param schema                               The schema holding the LEGEND results.
#'
#'
#' @export
unstageDb <- function(connectionDetails, schema = "legend") {
    conn <- connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(conn))

    tables <- DatabaseConnector::getTableNames(conn, schema)

    stagingTables <- tables[grepl("_STAGING$", tables)]
    toDrop <- gsub("_STAGING$", "", stagingTables)
    for (i in 1:length(toDrop)) {
      tableName <- toDrop[i]
      ParallelLogger::logInfo("Dropping table ", tableName)
      sql <- paste("DROP TABLE", tableName)
      DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)
    }

    for (i in 1:length(stagingTables)) {
        tableName <- stagingTables[i]
        newTableName <- gsub("_STAGING$", "", tableName)
        ParallelLogger::logInfo("Renaming table ", tableName, " to ", newTableName)
        sql <- sprintf("ALTER TABLE %s RENAME TO %s;", tableName, newTableName)
        DatabaseConnector::executeSql(conn, sql, progressBar = FALSE)
    }
}

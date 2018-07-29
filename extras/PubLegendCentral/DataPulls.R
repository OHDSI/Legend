getExposureName <- function(connection, exposureId) {
  sql <- "SELECT exposure_name FROM single_exposure_of_interest WHERE exposure_id = @exposure_id
UNION ALL SELECT exposure_name FROM combi_exposure_of_interest WHERE exposure_id = @exposure_id"
  sql <- SqlRender::renderSql(sql, exposure_id = exposureId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  exposureName <- querySql(connection, sql)  
  return(exposureName[1,1])
}

getOutcomeName <- function(connection, outcomeId) {
  sql <- "SELECT outcome_name FROM outcome_of_interest WHERE outcome_id = @outcome_id"
  sql <- SqlRender::renderSql(sql, outcome_id = outcomeId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  outcomeName <- querySql(connection, sql)  
  return(outcomeName[1,1])
}

getExposures <- function(connection) {
  sql <- "SELECT exposure_id, exposure_name, indication_id FROM single_exposure_of_interest
  UNION ALL SELECT exposure_id, exposure_name, indication_id FROM combi_exposure_of_interest"
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  exposures <- querySql(connection, sql)  
  colnames(exposures) <- SqlRender::snakeCaseToCamelCase(colnames(exposures))
  return(exposures)
}

getOutcomes <- function(connection) {
  sql <- "SELECT outcome_id, outcome_name, indication_id FROM outcome_of_interest"
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  outcomes <- querySql(connection, sql)  
  colnames(outcomes) <- SqlRender::snakeCaseToCamelCase(colnames(outcomes))
  return(outcomes)
}

getAnalyses <- function(connection) {
  sql <- "SELECT analysis_id, description FROM cohort_method_analysis"
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  analyses <- querySql(connection, sql)  
  colnames(analyses) <- SqlRender::snakeCaseToCamelCase(colnames(analyses))
  return(analyses)
}

getTcoDbs <- function(connection, 
                      targetIds = c(), 
                      comparatorIds = c(), 
                      outcomeIds = c(), 
                      databaseIds = c(), 
                      operator = "AND") {
  sql <- "SELECT target_id, comparator_id, outcome_id, database_id FROM cohort_method_result WHERE analysis_id = 1"
  parts <- c()
  if (length(targetIds) != 0) {
    parts <- c(parts, paste0("target_id IN (", paste(targetIds, collapse = ", "), ")")) 
  }
  if (length(comparatorIds) != 0) {
    parts <- c(parts, paste0("comparator_id IN (", paste(comparatorIds, collapse = ", "), ")")) 
  }
  if (length(outcomeIds) != 0) {
    parts <- c(parts, paste0("outcome_id IN (", paste(outcomeIds, collapse = ", "), ")")) 
  }
  if (length(databaseIds) != 0) {
    parts <- c(parts, paste0("database_id IN ('", paste(databaseIds, collapse = "', '"), "')")) 
  }
  if (length(parts) != 0) {
    if (operator == "AND") {
      sql <- paste(sql, "AND", paste(parts, collapse = " AND ")) 
    } else {
      sql <- paste(sql, "AND", paste(parts, collapse = " OR ")) 
    }
  }
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  tcoDbs <- querySql(connection, sql)  
  colnames(tcoDbs) <- SqlRender::snakeCaseToCamelCase(colnames(tcoDbs))
  return(tcoDbs)
}

getMainResults <- function(connection, 
                           targetIds = c(), 
                           comparatorIds = c(), 
                           outcomeIds = c(), 
                           databaseIds = c()) {
  sql <- "SELECT * FROM cohort_method_result"
  parts <- c()
  if (length(targetIds) != 0) {
    parts <- c(parts, paste0("target_id IN (", paste(targetIds, collapse = ", "), ")")) 
  }
  if (length(comparatorIds) != 0) {
    parts <- c(parts, paste0("comparator_id IN (", paste(comparatorIds, collapse = ", "), ")")) 
  }
  if (length(outcomeIds) != 0) {
    parts <- c(parts, paste0("outcome_id IN (", paste(outcomeIds, collapse = ", "), ")")) 
  }
  if (length(databaseIds) != 0) {
    parts <- c(parts, paste0("database_id IN ('", paste(databaseIds, collapse = "', '"), "')")) 
  }
  if (length(parts) != 0) {
    sql <- paste(sql, "WHERE", paste(parts, collapse = " AND ")) 
  }
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  results <- querySql(connection, sql)  
  colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
  return(results)
}

createTitle <- function(tcoDbs) {
  tcoDbs$targetName <- exposures$exposureName[match(tcoDbs$targetId, exposures$exposureId)]
  tcoDbs$comparatorName <- exposures$exposureName[match(tcoDbs$comparatorId, exposures$exposureId)]
  tcoDbs$outcomeName <- outcomes$outcomeName[match(tcoDbs$outcomeId, outcomes$outcomeId)]
  
  titles <- paste("A Comparison of",
                  tcoDbs$targetName,
                  "to",
                  tcoDbs$comparatorName,
                  "for the Risk of",
                  tcoDbs$outcomeName,
                  "in the",
                  tcoDbs$databaseId,"Database.")
  return(titles)
}

getCovariateBalance <- function(connection, targetId, comparatorId, databaseId) {
  sql <- "SELECT covariate.covariate_id, 
              covariate_name, 
              covariate_analysis_id,
              target_mean_before,
              comparator_mean_before,
              std_diff_before,
              target_mean_after,
              comparator_mean_after,
              std_diff_after
          FROM covariate_balance
          INNER JOIN covariate
          ON covariate_balance.covariate_id = covariate.covariate_id
            AND covariate_balance.database_id = covariate.database_id
          WHERE target_id = @target_id
            AND comparator_id = @comparator_id
            AND covariate.database_id = '@database_id'
            AND interaction_covariate_id IS NULL"
  sql <- SqlRender::renderSql(sql, 
                              target_id = targetId,
                              comparator_id = comparatorId,
                              database_id = databaseId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  balance <- querySql(connection, sql)  
  colnames(balance) <- c("covariateId",
                         "covariateName",
                         "analysisId",
                         "beforeMatchingMeanTreated",
                         "beforeMatchingMeanComparator", 
                         "beforeMatchingStdDiff", 
                         "afterMatchingMeanTreated",
                         "afterMatchingMeanComparator",
                         "afterMatchingStdDiff")
  return(balance)
}

getPs <- function(connection, targetId, comparatorId, databaseId) {
  sql <- "SELECT preference_score,
            target_density,
            comparator_density
          FROM preference_score_dist
          WHERE target_id = @target_id
          AND comparator_id = @comparator_id
          AND database_id = '@database_id'"
  sql <- SqlRender::renderSql(sql, 
                              target_id = targetId,
                              comparator_id = comparatorId,
                              database_id = databaseId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  ps <- querySql(connection, sql)  
  colnames(ps) <- SqlRender::snakeCaseToCamelCase(colnames(ps))
  return(ps)
}

getKaplanMeier <- function(connection, targetId, comparatorId, outcomeId, databaseId, analysisId) {
  sql <- "SELECT time,
    target_at_risk,
    comparator_at_risk,
    target_survival,
    target_survival_lb,
    target_survival_ub,
    comparator_survival,
    comparator_survival_lb,
    comparator_survival_ub
  FROM kaplan_meier_dist
  WHERE target_id = @target_id
  AND comparator_id = @comparator_id
  AND outcome_id = @outcome_id
  AND database_id = '@database_id'
  AND analysis_id = @analysis_id"
  sql <- SqlRender::renderSql(sql, 
                              target_id = targetId,
                              comparator_id = comparatorId,
                              outcome_id = outcomeId,
                              database_id = databaseId,
                              analysis_id = analysisId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  ps <- querySql(connection, sql)  
  colnames(ps) <- SqlRender::snakeCaseToCamelCase(colnames(ps))
  return(ps)
}

getAttrition <- function(connection, targetId, comparatorId, outcomeId, analysisId, databaseId) {
  sql <- "SELECT exposure_id,
    sequence_number,
    description,
    subjects
  FROM attrition
  WHERE (target_id = -1 OR target_id = @target_id)
    AND (comparator_id = -1 OR comparator_id = @comparator_id)
    AND (outcome_id = -1 OR outcome_id = @outcome_id)
    AND (exposure_id = @target_id OR exposure_id = @comparator_id)  
    AND (analysis_id = -1 OR analysis_id = @analysis_id)
    AND database_id = '@database_id'"
  sql <- SqlRender::renderSql(sql, 
                              target_id = targetId,
                              comparator_id = comparatorId,
                              outcome_id = outcomeId,
                              analysis_id = analysisId,
                              database_id = databaseId)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connection@dbms)$sql
  attrition <- querySql(connection, sql) 
  colnames(attrition) <- SqlRender::snakeCaseToCamelCase(colnames(attrition))
  targetAttrition <- attrition[attrition$exposureId == targetId, ]
  comparatorAttrition <- attrition[attrition$exposureId == comparatorId, ]
  colnames(targetAttrition)[colnames(targetAttrition) == "subjects"] <- "targetPersons"
  targetAttrition$exposureId <- NULL
  colnames(comparatorAttrition)[colnames(comparatorAttrition) == "subjects"] <- "comparatorPersons"
  comparatorAttrition$exposureId <- NULL
  attrition <- merge(targetAttrition, comparatorAttrition)
  attrition <- attrition[order(attrition$sequenceNumber), ] 
  return(attrition)
}

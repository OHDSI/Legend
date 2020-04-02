# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of CensoringSensAnalysis
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

#' @export
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    oracleTempSchema,
                    cohortDatabaseSchema,
                    cohortTable,
                    indicationFolder = indicationFolder,
                    sensAnalysisFolder = sensAnalysisFolder,
                    targetId,
                    comparatorId,
                    createCohorts = TRUE,
                    computeNewEstimates = TRUE) {
  if (!file.exists(sensAnalysisFolder)) {
    dir.create(sensAnalysisFolder)
  }


  if (createCohorts) {
    ParallelLogger::logInfo("Creating cohorts")
    connection <- DatabaseConnector::connect(connectionDetails)
    .createCohorts(connection = connection,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   oracleTempSchema = oracleTempSchema,
                   cohortDatabaseSchema = cohortDatabaseSchema,
                   cohortTable = cohortTable,
                   outputFolder = sensAnalysisFolder)
    DatabaseConnector::disconnect(connection)
  }

  if (computeNewEstimates) {
    ParallelLogger::logInfo("Computing new estimates")
    fetchNewCohorts(connectionDetails,
                    cohortDatabaseSchema,
                    cohortTable,
                    newTargetId,
                    newComparatorId,
                    sensAnalysisFolder)

    combineOldAndNewData(targetId,
                         comparatorId,
                         newTargetId,
                         newComparatorId,
                         indicationFolder,
                         sensAnalysisFolder)

    computeNewEstimates(sensAnalysisFolder)

    calibrateResults(indicationFolder, sensAnalysisFolder)
  }
}

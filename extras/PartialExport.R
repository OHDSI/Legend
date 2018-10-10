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

# Since a LOT of time is needed to compile and export diagnostics, here
# we export everything but the diagnostics

minCellCount = 5

indicationFolder <- file.path(outputFolder, indicationId)
exportFolder <- file.path(indicationFolder, "exportPartial")
if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
}
ParallelLogger::addDefaultFileLogger(file.path(exportFolder, "exportLog.txt"))
Legend:::exportIndication(indicationId = indicationId,
                 outputFolder = outputFolder,
                 exportFolder = exportFolder,
                 databaseId = databaseId)

Legend:::exportAnalyses(indicationId = indicationId,
               outputFolder = outputFolder,
               exportFolder = exportFolder,
               databaseId = databaseId)

Legend:::exportExposures(indicationId = indicationId,
                outputFolder = outputFolder,
                exportFolder = exportFolder,
                databaseId = databaseId)

Legend:::exportOutcomes(indicationId = indicationId,
               outputFolder = outputFolder,
               exportFolder = exportFolder,
               databaseId = databaseId)

Legend:::exportMetadata(indicationId = indicationId,
               outputFolder = outputFolder,
               exportFolder = exportFolder,
               databaseId = databaseId,
               databaseName = databaseName,
               databaseDescription = databaseDescription,
               minCellCount = minCellCount)

Legend:::exportMainResults(indicationId = indicationId,
                  outputFolder = outputFolder,
                  exportFolder = exportFolder,
                  databaseId = databaseId,
                  minCellCount = minCellCount,
                  maxCores = maxCores)

# Skipping diagnostics

# Add all to zip file -------------------------------------------------------------------------------
ParallelLogger::logInfo("Adding results to zip file")
zipName <- file.path(exportFolder, paste0("Results", indicationId, databaseId, ".zip"))
files <- list.files(exportFolder, pattern = ".*\\.csv$")
oldWd <- setwd(exportFolder)
DatabaseConnector::createZipFile(zipFile = zipName, files = files)
ParallelLogger::logInfo("Results are ready for sharing at:", zipName)
setwd(oldWd)

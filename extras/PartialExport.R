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

source("extras/BetaBlockerSensitivityAnalyses/Functions.R")

# PanTher settings ------------------------------------------
sourceFolder <- "D:/Legend/panther/Hypertension/"
targetFolder <- "D:/Legend/pantherBb/Hypertension/"
fileName <- "D:/Legend/BbSaResults_PanTher.csv"

# Run sensitivity analysis --------------------------------------
if (!file.exists(targetCmFolder)) {
    dir.create(targetCmFolder, recursive = TRUE)
}

subsetOmr(sourceFolder, targetFolder)

restrictCmDatas(sourceFolder, targetFolder)

runCohortMethod(targetFolder)

calibrateResults(sourceFolder, targetFolder)

createPrettyTable(sourceFolder, targetFolder, fileName)

source("extras/BetaBlockerSensitivityAnalyses/Functions.R")

options(fftempdir = "d:/fftemp")

# PanTher settings ------------------------------------------
sourceFolder <- "D:/Legend/panther/Hypertension"
targetFolder <- "D:/Legend/pantherBb/Hypertension"
fileName <- "D:/Legend/BbSaResults_PanTher.csv"

# Run sensitivity analysis --------------------------------------
if (!file.exists(targetFolder)) {
    dir.create(targetFolder, recursive = TRUE)
}

subsetOmr(sourceFolder, targetFolder)

restrictCmDatas(sourceFolder, targetFolder)

runCohortMethod(targetFolder)

calibrateResults(sourceFolder, targetFolder)

createPrettyTable(sourceFolder, targetFolder, fileName)

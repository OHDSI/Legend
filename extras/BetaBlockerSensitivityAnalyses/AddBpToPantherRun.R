source("extras/BetaBlockerSensitivityAnalyses/Functions.R")

options(fftempdir = "d:/fftemp")

# PanTher settings ------------------------------------------
sourceFolder <- "D:/Legend/panther/Hypertension"
targetFolder <- "D:/Legend/pantherBb/HypertensionBp"
fileName <- "D:/Legend/BbBpResults_PanTher.csv"


# Run sensitivity analysis --------------------------------------
if (!file.exists(targetFolder)) {
    dir.create(targetFolder, recursive = TRUE)
}

subsetOmr(sourceFolder, targetFolder)

addBpToCmDatas(sourceFolder, targetFolder)

runCohortMethod(targetFolder)

calibrateResults(sourceFolder, targetFolder)

compareNewToOldEstimates(sourceFolder, targetFolder, fileName)

createPrettyTable(sourceFolder, targetFolder, fileName)

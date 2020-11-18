source("extras/BetaBlockerSensitivityAnalyses/Functions.R")

options(fftempdir = "d:/fftemp")

# PanTher settings ------------------------------------------
sourceFolder <- "D:/Legend/panther/Hypertension"
targetFolder <- "D:/Legend/pantherBb/Hypertension"
fileName <- "D:/Legend/BbSaResults_PanTher.csv"

# Optum settings ------------------------------------------
sourceFolder <- "D:/Legend/optum/Hypertension"
targetFolder <- "D:/Legend/optumBb/Hypertension"
fileName <- "D:/Legend/BbSaResults_Optum.csv"

# Ccae settings ------------------------------------------
sourceFolder <- "D:/Legend/ccae/Hypertension"
targetFolder <- "D:/Legend/ccaeBb/Hypertension"
fileName <- "D:/Legend/BbSaResults_Ccae.csv"

# Run sensitivity analysis --------------------------------------
if (!file.exists(targetFolder)) {
    dir.create(targetFolder, recursive = TRUE)
}

subsetOmr(sourceFolder, targetFolder)

restrictCmDatas(sourceFolder, targetFolder)

runCohortMethod(targetFolder)

calibrateResults(sourceFolder, targetFolder)

compareNewToOldEstimates(sourceFolder, targetFolder, fileName)

createPrettyTable(sourceFolder, targetFolder, fileName)

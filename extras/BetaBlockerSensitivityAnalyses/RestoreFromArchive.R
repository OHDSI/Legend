library(ArchiveR)
source("extras/BetaBlockerSensitivityAnalyses/Functions.R")

# subset <- read.csv("extras/BetaBlockerSensitivityAnalyses/ComparisonsBb.csv")
# cmDataFolders <- sprintf("CmData_l1_t%s_c%s", subset$targetId, subset$comparatorId)
# Restore Optum files ----------------------------------------------

s3Folder <- "mschuemi/LegendHypertension/optum/Hypertension"
localFolder <- "D:/Legend/optum/Hypertension/"
if (!file.exists(file.path(localFolder, "cmOutput"))) {
    dir.create(file.path(localFolder, "cmOutput"), recursive = TRUE)
}
if (!file.exists(file.path(localFolder, "export"))) {
    dir.create(file.path(localFolder, "export"), recursive = TRUE)
}

restoreFile(s3File = paste(s3Folder, "cmOutput", "outcomeModelReference1.rds", sep = "/"),
            localFolder = file.path(localFolder, "cmOutput"))
restoreFile(s3File = paste(s3Folder, "cmOutput", "outcomeModelReference2.rds", sep = "/"),
            localFolder = file.path(localFolder, "cmOutput"))
restoreFile(s3File = paste(s3Folder, "cmOutput", "outcomeModelReference4.rds", sep = "/"),
            localFolder = file.path(localFolder, "cmOutput"))
restoreFile(s3File = paste(s3Folder, "pairedExposureSummaryFilteredBySize.csv", sep = "/"),
            localFolder = localFolder)
restoreFile(s3File = paste(s3Folder, "signalInjectionSummary.csv", sep = "/"),
            localFolder = localFolder)
restoreFile(s3File = paste(s3Folder, "export", "cohort_method_result.csv", sep = "/"),
            localFolder = file.path(localFolder, "export"))

tempFolder <- tempdir()
subsetOmr(localFolder, tempFolder)
toGenerate <- readRDS(file.path(tempFolder, "toGenerate.rds"))
unlink(tempFolder, recursive = TRUE)
cmDataFolders <- unique(toGenerate$cohortMethodDataFolder[toGenerate$part %in% c(1, 2)])

cmDataFolder = cmDataFolders[1]
for (cmDataFolder in cmDataFolders) {
    writeLines(paste("Restoring", cmDataFolder))
    restoreFolder(s3Folder = paste(s3Folder, "cmOutput", cmDataFolder, sep = "/"),
                  localFolder = file.path(localFolder, "cmOutput", cmDataFolder))
}

ArchiveR::listFiles("mschuemi/LegendHypertension/optum/Hypertension")


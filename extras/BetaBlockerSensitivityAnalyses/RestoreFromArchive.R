library(ArchiveR)

subset <- read.csv("extras/BetaBlockerSensitivityAnalyses/ComparisonsBb.csv")
cmDataFolders <- sprintf("CmData_l1_t%s_c%s", subset$targetId, subset$comparatorId)
# Restore Optum files ----------------------------------------------

s3Folder <- "mschuemi/LegendHypertension/optum/Hypertension"
localFolder <- "D:/Legend/optumBb/Hypertension/"
if (!file.exists(file.path(localFolder, "cmOutput"))) {
    dir.create(file.path(localFolder, "cmOutput"), recursive = TRUE)
}

cmDataFolder = cmDataFolders[1]
for (cmDataFolder in cmDataFolders) {
    writeLines(paste("Restoring", cmDataFolder))
    restoreFolder(s3Folder = paste(s3Folder, "cmOutput", cmDataFolder, sep = "/"),
                  localFolder = file.path(localFolder, "cmOutput", cmDataFolder))
}
restoreFile(s3File = paste(s3Folder, "cmOutput", "outcomeModelReference1.rds", sep = "/"),
            localFolder = localFolder)
restoreFile(s3File = paste(s3Folder, "cmOutput", "outcomeModelReference2.rds", sep = "/"),
            localFolder = localFolder)
restoreFile(s3File = paste(s3Folder, "cmOutput", "outcomeModelReference3.rds", sep = "/"),
            localFolder = localFolder)
restoreFile(s3File = paste(s3Folder, "pairedExposureSummaryFilteredBySize.csv", sep = "/"),
            localFolder = localFolder)
restoreFile(s3File = paste(s3Folder, "signalInjectionSummary.csv", sep = "/"),
            localFolder = localFolder)


ArchiveR::listFiles("mschuemi/LegendHypertension/optum/Hypertension")


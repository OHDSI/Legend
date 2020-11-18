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

# Restore CCAE files ----------------------------------------------

s3Folder <- "mschuemi/LegendHypertension/ccae_part%s/Hypertension"
localFolder <- "D:/Legend/ccae/Hypertension/"
if (!file.exists(file.path(localFolder, "cmOutput"))) {
    dir.create(file.path(localFolder, "cmOutput"), recursive = TRUE)
}
if (!file.exists(file.path(localFolder, "export"))) {
    dir.create(file.path(localFolder, "export"), recursive = TRUE)
}
restoreAndMerge <- function(path) {
    tempFiles <- c()
    for (i in 1:3) {
        tempFiles[i] <- tempfile()
        restoreFile(s3File = paste(sprintf(s3Folder, i), path, sep = "/"),
                localFile = tempFiles[i])
    }
    if (grepl(".rds$", path)) {
        objects <- lapply(tempFiles, readRDS)
        objects <- do.call(rbind, objects)
        saveRDS(objects, file.path(localFolder, path))
    } else if (grepl(".csv$", path)) {
        objects <- lapply(tempFiles, readr::read_csv)
        objects <- do.call(rbind, objects)
        readr::write_csv(objects, file.path(localFolder, path))
    }
    unlink(tempFiles, recursive = TRUE)
}

restoreAndMerge("cmOutput/outcomeModelReference1.rds")
restoreAndMerge("cmOutput/outcomeModelReference2.rds")
restoreAndMerge("cmOutput/outcomeModelReference4.rds")
restoreAndMerge("pairedExposureSummaryFilteredBySize.csv")
restoreAndMerge("signalInjectionSummary.csv")

tempFolder <- tempdir()
subsetOmr(localFolder, tempFolder)
toGenerate <- readRDS(file.path(tempFolder, "toGenerate.rds"))
unlink(tempFolder, recursive = TRUE)
cmDataFolders <- unique(toGenerate$cohortMethodDataFolder[toGenerate$part %in% c(1, 2)])

restored <- c()
for (i in 1:3){
    cmDataFolder = cmDataFolders[1]
    for (cmDataFolder in cmDataFolders) {
        remoteFiles <- ArchiveR::listFiles(paste(sprintf(s3Folder, i), "cmOutput", cmDataFolder, sep = "/"), maxFiles = 1)
        if (nrow(remoteFiles) > 0) {
            writeLines(paste("Restoring", cmDataFolder))
            restoreFolder(s3Folder = paste(sprintf(s3Folder, i), "cmOutput", cmDataFolder, sep = "/"),
                          localFolder = file.path(localFolder, "cmOutput", cmDataFolder))
            restored <- c(restored, cmDataFolder)
        }
    }
}
all(cmDataFolders %in% restored)




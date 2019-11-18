# This code is used to recompute hazard ratios for select TCOs, using blood pressure in the propensity model
source("extras/BloodPressureFunctions.R")

indicationFolder <- file.path(outputFolder, indicationId)
bpFolder <- file.path(indicationFolder, "bp")

analysisId <- 1 # Stratification, on-treatment
# analysisId <- 3 # Matching, on-treatment


# Download blood pressure data -----------------------------------------------------------------
downloadBloodPressureData(connectionDetails = connectionDetails,
                          indicationFolder = indicationFolder,
                          bpFolder = bpFolder,
                          indicationId = indicationId,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          tablePrefix = tablePrefix,
                          oracleTempSchema = oracleTempSchema)

# Select TCs of interest -----------------------------------------------------------------------
exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
idx <- which(exposureSummary$targetName == "Hydrochlorothiazide" & exposureSummary$comparatorName == "Chlorthalidone")
classNames <- c("ACE inhibitors", "Angiotensin receptor blockers (ARBs)", "Thiazide or thiazide-like diuretics", "Dihydropyridine calcium channel blockers (dCCB)", "Non-dihydropyridine calcium channel blockers (ndCCB)", "Beta blockers - cardioselective")
exposures <- read.csv(system.file("settings", "ExposuresOfInterest.csv", package = "Legend"))
classIds <- exposures$cohortId[exposures$name %in% classNames]
combinations <- read.csv(file.path(indicationFolder, "exposureCombis.csv"))
classCombiIds <- combinations$cohortDefinitionId[combinations$exposureId1 %in% classIds &
                                                     combinations$exposureId2 %in% classIds ]
idx <- c(idx, which(exposureSummary$targetId %in% c(classIds, classCombiIds) &
                        exposureSummary$comparatorId %in% c(classIds, classCombiIds)))
tcs <- exposureSummary[idx, ]

# Subset outcomeModelReference for speed ----------------------------------------------------------
outcomeModelReference1 <- readRDS(file.path(indicationFolder,
                                            "cmOutput",
                                            "outcomeModelReference1.rds"))
outcomeModelReference2 <- readRDS(file.path(indicationFolder,
                                            "cmOutput",
                                            "outcomeModelReference2.rds"))
outcomeModelReference3 <- readRDS(file.path(indicationFolder,
                                            "cmOutput",
                                            "outcomeModelReference3.rds"))
outcomeModelReference <- rbind(outcomeModelReference1, outcomeModelReference2, outcomeModelReference3)
outcomeModelReference <- merge(outcomeModelReference, tcs[, c("targetId", "comparatorId")])
outcomeModelReference <- outcomeModelReference[outcomeModelReference$analysisId == analysisId, ]
saveRDS(outcomeModelReference, file.path(bpFolder, "outcomeModelReference.rds"))

# Create blood pressure distribution plots ----------------------------------------------------
ParallelLogger::addDefaultFileLogger(file.path(bpFolder, "log.txt"))
cluster <- ParallelLogger::makeCluster(10)
balance <- ParallelLogger::clusterApply(cluster, split(tcs, 1:nrow(tcs)), plotBalance, indicationFolder = indicationFolder, bpFolder = bpFolder, analysisId = analysisId)
ParallelLogger::stopCluster(cluster)
# balance <- plyr::llply(split(tcs, 1:nrow(tcs)), plotBalance, indicationFolder = indicationFolder, bpFolder = bpFolder)
balance <- do.call("rbind", balance)
balance$targetType <- "Single"
balance$targetType[balance$targetId %in% combinations$cohortDefinitionId] <- "Combination"
balance$comparatorType <- "Single"
balance$comparatorType[balance$comparatorId %in% combinations$cohortDefinitionId] <- "Combination"
write.csv(balance, file.path(bpFolder, "Balance.csv"), row.names = FALSE)

# Add blood pressure to covariates and refit propensity models --------------------------------
cluster <- ParallelLogger::makeCluster(10)
balance <- ParallelLogger::clusterApply(cluster, split(tcs, 1:nrow(tcs)), refitPropensityModel, indicationFolder = indicationFolder, bpFolder = bpFolder, analysisId = analysisId)
ParallelLogger::stopCluster(cluster)
# balance <- plyr::llply(split(tcs, 1:nrow(tcs)), refitPropensityModel, indicationFolder = indicationFolder, bpFolder = bpFolder)
balance <- do.call("rbind", balance)
balance$targetType <- "Single"
balance$targetType[balance$targetId %in% combinations$cohortDefinitionId] <- "Combination"
balance$comparatorType <- "Single"
balance$comparatorType[balance$comparatorId %in% combinations$cohortDefinitionId] <- "Combination"
write.csv(balance, file.path(bpFolder, "BalanceAdjustBp.csv"), row.names = FALSE)

# Recompute hazard ratios using new propensity models ------------------------------------------
cluster <- ParallelLogger::makeCluster(10)
dummy <- ParallelLogger::clusterApply(cluster, split(tcs, 1:nrow(tcs)), computeAdjustedHrs, indicationFolder = indicationFolder, bpFolder = bpFolder, indicationId = indicationId, analysisId = analysisId)
ParallelLogger::stopCluster(cluster)

# plyr::l_ply(split(tcs, 1:nrow(tcs)), computeAdjustedHrs, indicationFolder = indicationFolder, bpFolder = bpFolder)
fileNames <- file.path(bpFolder, sprintf("HrsData_%s_%s_%s.rds", tcs$targetName, tcs$comparatorName, analysisId))
length(fileNames)
fileNames <- fileNames[file.exists(fileNames)]
length(fileNames)
hrs <- lapply(fileNames, function(fileName) readRDS(fileName))
hrs <- do.call("rbind", hrs)
hrs <- merge(hrs, tcs[, c("targetId", "targetName", "comparatorId", "comparatorName")])
hrs$targetType <- "Single"
hrs$targetType[hrs$targetId %in% combinations$cohortDefinitionId] <- "Combination"
hrs$comparatorType <- "Single"
hrs$comparatorType[hrs$comparatorId %in% combinations$cohortDefinitionId] <- "Combination"
write.csv(hrs, file.path(bpFolder, "HrsData_all.csv"), row.names = FALSE)

row <- tcs[tcs$targetId == 974166 & tcs$comparatorId == 1395058, ]

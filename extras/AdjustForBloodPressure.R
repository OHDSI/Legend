# This code is used to recompute hazard ratios for select TCOs, using blood pressure in the propensity model
source("extras/BloodPressureFunctions.R")

indicationFolder <- file.path(outputFolder, indicationId)
bpFolder <- file.path(indicationFolder, "bp")


# Download blood pressure data -----------------------------------------------------------------
downloadBloodPressureData(connectionDetails, indicationFolder, bpFolder)

# Select TCs of interest -----------------------------------------------------------------------
exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
idx <- which(exposureSummary$targetName == "Hydrochlorothiazide" & exposureSummary$comparatorName == "Chlorthalidone")
classes <- c("ACE inhibitors", "Angiotensin receptor blockers (ARBs)", "Thiazide or thiazide-like diuretics", "Dihydropyridine calcium channel blockers (dCCB)", "Nodihydropyridine calcium channel blockers (ndCCB)")
idx <- c(idx, which(exposureSummary$targetName %in% classes & exposureSummary$comparatorName %in% classes))
tcs <- exposureSummary[idx, ]

# Create blood pressure distribution plots ----------------------------------------------------
balance <- plyr::llply(split(tcs, 1:nrow(tcs)), plotBalance, indicationFolder = indicationFolder, bpFolder = bpFolder)
balance <- do.call("rbind", balance)
write.csv(balance, file.path(bpFolder, "Balance.csv"), row.names = FALSE)

# Add blood pressure to covariates and refit propensity models --------------------------------
balance <- plyr::llply(split(tcs, 1:nrow(tcs)), refitPropensityModel, indicationFolder = indicationFolder, bpFolder = bpFolder)
balance <- do.call("rbind", balance)
write.csv(balance, file.path(bpFolder, "BalanceAdjustBp.csv"), row.names = FALSE)

# Recompute hazard ratios using new propensity models ------------------------------------------
plyr::l_ply(split(tcs, 1:nrow(tcs)), computeAdjustedHrs, indicationFolder = indicationFolder, bpFolder = bpFolder)

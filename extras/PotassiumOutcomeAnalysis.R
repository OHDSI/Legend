# This code is used to recompute hazard ratios for select TCOs, using blood pressure in the propensity model
source("extras/PotassiumFunctions.R")

indicationFolder <- file.path(outputFolder, indicationId)
kFolder <- file.path(indicationFolder, "k")


# Download blood pressure data -----------------------------------------------------------------
downloadPotassiumData(connectionDetails = connectionDetails,
                      indicationFolder = indicationFolder,
                      kFolder = kFolder,
                      indicationId = indicationId,
                      cdmDatabaseSchema = cdmDatabaseSchema,
                      cohortDatabaseSchema = cohortDatabaseSchema,
                      tablePrefix = tablePrefix,
                      oracleTempSchema = oracleTempSchema)

# Select TCs of interest -----------------------------------------------------------------------
exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
idx <- which(exposureSummary$targetName == "Hydrochlorothiazide" & exposureSummary$comparatorName == "Chlorthalidone")
tc <- exposureSummary[idx, ]


library(Legend)
options(fftempdir = "c:/fftemp")
maxCores <- parallel::detectCores()
studyFolder <- "d:/Legend"
dbms <- "pdw"
user <- NULL
pw <- NULL
server <- Sys.getenv("PDW_SERVER")
port <- Sys.getenv("PDW_PORT")
oracleTempSchema <- NULL
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)
indicationId <- "Hypertension"
cdmDatabaseSchema <- "cdm_optum_panther_v735.dbo" # Retired
cdmDatabaseSchema <- "cdm_optum_panther_v811.dbo"
cohortDatabaseSchema <- "scratch.dbo"
outputFolder <- file.path(studyFolder, "panther")


source("extras/LspsAnalyses/Functions.R")

indicationFolder <- file.path(outputFolder, indicationId)
lspsFolder <- file.path(indicationFolder, "lsps")
if (!file.exists(lspsFolder)) {
    dir.create(lspsFolder)
}

analysisId <- 3 # Matching, on-treatment

# Download blood pressure data -----------------------------------------------------------------
downloadBloodPressureData(connectionDetails = connectionDetails,
                          indicationFolder = indicationFolder,
                          lspsFolder = lspsFolder,
                          indicationId = indicationId,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          tablePrefix = tablePrefix,
                          oracleTempSchema = oracleTempSchema)

# Select TCs of interest -----------------------------------------------------------------------
exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
idx <- which(exposureSummary$targetName == "Hydrochlorothiazide" & exposureSummary$comparatorName == "Lisinopril")
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
saveRDS(outcomeModelReference, file.path(lspsFolder, "outcomeModelReference.rds"))

# Add blood pressure to covariates and refit propensity models --------------------------------
refitPropensityModelUsingBp(row = tcs[1, ],
                            indicationFolder = indicationFolder,
                            lspsFolder = lspsFolder,
                            analysisId = analysisId)

# Recompute hazard ratios using BP-adjusted propensity models ------------------------------------------
computeAdjustedHrs(row = tcs[1, ],
                   indicationFolder = indicationFolder,
                   lspsFolder = lspsFolder,
                   indicationId = indicationId,
                   analysisId = analysisId)

# Recompute hazard ratios using no propensity models --------------------------------------------
computeUnadjustedHrs(row = tcs[1,],
                     indicationFolder = indicationFolder,
                     lspsFolder = lspsFolder,
                     indicationId = indicationId,
                     analysisId = analysisId)

# Create propensity models using manually selected covariates --------------------------------
fitManualPropensityModel(row = tcs[1,],
                         indicationFolder = indicationFolder,
                         lspsFolder = lspsFolder,
                         newPsFolder = file.path(lspsFolder, "manual"),
                         analysisId = analysisId)

# Recompute hazard ratios using manual propensity models --------------------------------------------
computeAdjustedHrs(row = tcs[1,],
                   indicationFolder = indicationFolder,
                   lspsFolder = lspsFolder,
                   newPsFolder = file.path(lspsFolder, "manual"),
                   indicationId = indicationId,
                   analysisId = analysisId,
                   type = "Manually selected\ncovariates")

# Create propensity models using manually selected covariates + blood pressure ----------------------
fitManualPropensityModel(row = tcs[1,],
                         indicationFolder = indicationFolder,
                         lspsFolder = lspsFolder,
                         newPsFolder = file.path(lspsFolder, "manualPlusBp"),
                         analysisId = analysisId,
                         addBloodPressure = TRUE)

# Recompute hazard ratios using manual propensity models + blood pressure ---------------------------
computeAdjustedHrs(row = tcs[1,],
                   indicationFolder = indicationFolder,
                   lspsFolder = lspsFolder,
                   newPsFolder = file.path(lspsFolder, "manualPlusBp"),
                   indicationId = indicationId,
                   analysisId = analysisId,
                   type = "Manually selected\ncovariates + BP")

# Create propensity models using all covariates except T2DM -----------------------------------
refitPropensityModelWithoutT2dm(row = tcs[1, ],
                                indicationFolder = indicationFolder,
                                lspsFolder = lspsFolder,
                                newPsFolder = file.path(lspsFolder, "noT2dm"),
                                analysisId = analysisId,
                                connectionDetails = connectionDetails,
                                cdmDatabaseSchema = cdmDatabaseSchema)

# Recompute hazard ratios using all covariates except T2DM ---------------------------
computeAdjustedHrs(row = tcs[1,],
                   indicationFolder = indicationFolder,
                   lspsFolder = lspsFolder,
                   newPsFolder = file.path(lspsFolder, "noT2dm"),
                   indicationId = indicationId,
                   analysisId = analysisId,
                   type = "All covariates\nexcept T2DM")

# Create propensity models using manually selected covariates except T2DM --------------------------------
fitManualPropensityModel(row = tcs[1,],
                         indicationFolder = indicationFolder,
                         lspsFolder = lspsFolder,
                         newPsFolder = file.path(lspsFolder, "manualNoT2dm"),
                         analysisId = analysisId,
                         removeT2dm = TRUE)

# Recompute hazard ratios using manual propensity models except T2DM --------------------------------------------
computeAdjustedHrs(row = tcs[1,],
                   indicationFolder = indicationFolder,
                   lspsFolder = lspsFolder,
                   newPsFolder = file.path(lspsFolder, "manualNoT2dm"),
                   indicationId = indicationId,
                   analysisId = analysisId,
                   type = "Manually selected\ncovariates except T2DM")

# Combine across analyses and cleanup --------------------------------------------------------------
row <- tcs[1, ]
# fileName <- file.path(lspsFolder, sprintf("HrsDataBpAdj_%s_%s_%s.csv", row$targetName, row$comparatorName, analysisId))
# estimatesBpAdj <- read.csv(fileName, stringsAsFactors = FALSE)
fileName <- file.path(lspsFolder, sprintf("HrsDataNoAdj_%s_%s_%s.csv", row$targetName, row$comparatorName, analysisId + 4))
estimatesNoAdj <- read.csv(fileName, stringsAsFactors = FALSE)
# estimatesNoAdj <- estimatesNoAdj[estimatesNoAdj$type != "Original", ]
fileName <- file.path(lspsFolder, "manual", sprintf("HrsDataBpAdj_%s_%s_%s.csv", row$targetName, row$comparatorName, analysisId))
estimatesManualAdj <- read.csv(fileName, stringsAsFactors = FALSE)
estimatesManualAdj <- estimatesManualAdj[estimatesManualAdj$type != "Original", ]
# fileName <- file.path(lspsFolder, "manualPlusBp", sprintf("HrsDataBpAdj_%s_%s_%s.csv", row$targetName, row$comparatorName, analysisId))
# estimatesManualPlusbpAdj <- read.csv(fileName, stringsAsFactors = FALSE)
# estimatesManualPlusbpAdj <- estimatesManualPlusbpAdj[estimatesManualPlusbpAdj$type != "Original", ]
fileName <- file.path(lspsFolder, "manualNoT2dm", sprintf("HrsDataBpAdj_%s_%s_%s.csv", row$targetName, row$comparatorName, analysisId))
estimatesManualNoT2dm <- read.csv(fileName, stringsAsFactors = FALSE)
estimatesManualNoT2dm <- estimatesManualNoT2dm[estimatesManualNoT2dm$type != "Original", ]
estimatesManualNoT2dm$type <- "Manually selected\ncovariates except T2DM"
fileName <- file.path(lspsFolder, "noT2dm", sprintf("HrsDataBpAdj_%s_%s_%s.csv", row$targetName, row$comparatorName, analysisId))
estimatesNoT2dm <- read.csv(fileName, stringsAsFactors = FALSE)
estimatesNoT2dm <- estimatesNoT2dm[estimatesNoT2dm$type != "Original", ]

# estimates <- rbind(estimatesBpAdj, estimatesNoAdj, estimatesManualAdj, estimatesManualPlusbpAdj)
estimates <- rbind(estimatesNoAdj, estimatesManualAdj, estimatesManualNoT2dm, estimatesNoT2dm)
estimates$y <- NULL
estimates <- estimates[estimates$targetId == tcs[1, "targetId"], ]
write.csv(estimates, fileName <- file.path(lspsFolder, "HrsForLspsPaper.csv"))

for (type in unique(estimates$type)) {
    subset <- estimates[estimates$type == type &
                            !is.na(estimates$targetEffectSize) &
                            estimates$targetEffectSize == 1.0 &
                            estimates$estimate == "Uncalibrated", ]
    fileName <- file.path(lspsFolder, sprintf("Ncs_%s_%s_%s.png", row$targetName, row$comparatorName, gsub("[ \n]", "_", type)))
    EmpiricalCalibration::plotCalibrationEffect(logRrNegatives = subset$logRr,
                                                seLogRrNegatives = subset$seLogRr,
                                                xLabel = "Hazard Ratio",
                                                showCis = TRUE,
                                                title = type,
                                                fileName = fileName)

}
# fileName <- file.path(lspsFolder, "Balance.csv")
# balOriginal <- read.csv(fileName, stringsAsFactors = FALSE)
# balOriginal$type <- "Original"
# fileName <- file.path(lspsFolder, "BalanceAfterMatchingUsingBp_Hydrochlorothiazide_Lisinopril.csv")
# balBpAdj <- read.csv(fileName, stringsAsFactors = FALSE)
# balBpAdj$type <- "Adjusting for\nblood pressure"
# fileName <- file.path(lspsFolder, "manual", "BpBalanceAfterMatchingUsingManualPs_Hydrochlorothiazide_Lisinopril.csv")
# balManualAdj <- read.csv(fileName, stringsAsFactors = FALSE)
# balManualAdj$type <- "Manually selected\ncovariates"
# fileName <- file.path(lspsFolder, "manualPlusBp", "BpBalanceAfterMatchingUsingManualPs_Hydrochlorothiazide_Lisinopril.csv")
# balManualPlusBpAdj <- read.csv(fileName, stringsAsFactors = FALSE)
# balManualPlusBpAdj$type <- "Manually selected\ncovariates + BP"
#
# balOriginal <- balOriginal[, colnames(balBpAdj)]
# bal <- rbind(balOriginal, balBpAdj, balManualAdj, balManualPlusBpAdj)
# write.csv(bal, fileName <- file.path(lspsFolder, "BpBalanceLspsPaper.csv"))

fileName <- file.path(indicationFolder, "balance", sprintf("Bal_t%d_c%d_a%d.rds", row$targetId, row$comparatorId, 4)) # Analysis 4 is ITT, same balance as 3 (on treatment)
balOriginal <- readRDS(fileName)
balOriginal <- balOriginal[balOriginal$covariateId == 201826210, ]
balOriginal$type <- "All covariates"

fileName <- file.path(lspsFolder, "noT2dm", "AllBalanceAfterMatching_Hydrochlorothiazide_Lisinopril.csv")
balNoT2dm <- read.csv(fileName, stringsAsFactors = FALSE)
balNoT2dm <- balNoT2dm[balNoT2dm$covariateId == 201826210, ]
balNoT2dm$type <- "All covariates except T2DM"


fileName <- file.path(lspsFolder, "manual", "AllBalanceAfterMatchingUsingManualPs_Hydrochlorothiazide_Lisinopril.csv")
balManualAdj <- read.csv(fileName, stringsAsFactors = FALSE)
balManualAdj <- balManualAdj[balManualAdj$covariateId == 201826210, ]
balManualAdj$type <- "Manually selected covariates"

fileName <- file.path(lspsFolder, "manualNoT2dm", "AllBalanceAfterMatchingUsingManualPs_Hydrochlorothiazide_Lisinopril.csv")
balManualNoT2dm <- read.csv(fileName, stringsAsFactors = FALSE)
balManualNoT2dm <- balManualNoT2dm[balManualNoT2dm$covariateId == 201826210, ]
balManualNoT2dm$type <- "Manually selected covariates except T2DM"

balOriginal <- balOriginal[, colnames(balNoT2dm)]
bal <- rbind(balOriginal, balNoT2dm, balManualAdj, balManualNoT2dm)
write.csv(bal, fileName <- file.path(lspsFolder, "T2dmBalanceLspsPaper.csv"))


# Compute expected systematic error ---------------------------------------
estimates <- read.csv("C:/Users/MSCHUEMI/Desktop/LSPS/HrsForLspsPaper.csv")

computeEse <- function(subset) {
    print(subset$type[1])
    ncs <- subset[!is.na(subset$targetEffectSize) & subset$targetEffectSize == 1 & subset$estimate == "Uncalibrated", ]
    null <- EmpiricalCalibration::fitMcmcNull(ncs$logRr, ncs$seLogRr)
    expectedSystematicError <- EmpiricalCalibration::computeExpectedSystematicError(null)
    return(dplyr::tibble(type = subset$type[1],
                         mu = null[1],
                         sd = 1/sqrt(null[2]),
                         ese = expectedSystematicError$expectedSystematicError,
                         eseLb = expectedSystematicError$lb95ci,
                         eseUb = expectedSystematicError$lb95ub))
}

eses <- lapply(split(estimates, estimates$type), computeEse)
eses <- dplyr::bind_rows(eses)
eses
library(ggplot2)
eses$type[eses$type == "Original"] <- "All covariates"
eses$type[eses$type == "Adjusting for\nblood pressure"] <- "LSPS + BP"
ggplot(eses, aes(y = type, x = ese, xmin = eseLb, xmax = eseUb)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point() +
    geom_errorbarh() +
    scale_x_continuous("Expected systematic error", limits = c(0, max(eses$eseUb))) +
    scale_y_discrete("")
ggsave("C:/Users/MSCHUEMI/Desktop/LSPS/ESE.png", width = 4, height = 3)

hois <- read.csv("inst/settings/OutcomesOfInterest.csv")
hois <- hois[hois$indicationId == "Hypertension", ]
hois <- data.frame(outcomeId = hois$cohortId,
                   outcomeName = hois$name)
estimates <- merge(estimates, hois)

# hoi <- estimates[estimates$outcomeName == "Stroke" & estimates$estimate == "Uncalibrated", ]
hoi <- estimates[estimates$outcomeName == "Acute myocardial infarction" & estimates$estimate == "Uncalibrated", ]
# hoi <- estimates[estimates$outcomeName == "Hospitalization with heart failure" & estimates$estimate == "Uncalibrated", ]
hoi <- estimates[estimates$outcomeName == "Chronic kidney disease" & estimates$estimate == "Uncalibrated", ]

hoi$type[hoi$type == "Original"] <- "All covariates"
hoi$type[hoi$type == "Adjusting for\nblood pressure"] <- "LSPS + BP"

ggplot(hoi, aes(y = type, x = rr, xmin = ci95lb, xmax = ci95ub)) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_point() +
    geom_errorbarh() +
    scale_x_continuous("Hazard Ratio") +
    scale_y_discrete("") +
    ggtitle(hoi$outcomeName[1])
ggsave("C:/Users/MSCHUEMI/Desktop/LSPS/HR.png", width = 4, height = 3)

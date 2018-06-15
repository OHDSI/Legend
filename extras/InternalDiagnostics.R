# Overview of PS plots --------------------------------------------------------------------------------------
indicationFolder <- file.path(outputFolder, indication)
diagnosticsFolder <- file.path(indicationFolder, "internalDiagnostics")
if (!file.exists(diagnosticsFolder)) {
    dir.create(diagnosticsFolder)
}
exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
outcomeModelReference <- readRDS(file.path(indicationFolder, "cmOutput", "outcomeModelReference.rds"))
# datas <- list()
# for (i in 1:nrow(exposureSummary)) {
preparePlot <- function(i, exposureSummary, outcomeModelReference) {
    treatmentId <- exposureSummary$tprimeCohortDefinitionId[i]
    comparatorId <- exposureSummary$cprimeCohortDefinitionId[i]
    idx <- outcomeModelReference$targetId == treatmentId &
        outcomeModelReference$comparatorId == comparatorId &
        outcomeModelReference$analysisId == 1
    psFileName <- outcomeModelReference$sharedPsFile[idx][1]
    if (file.exists(psFileName)) {
        ps <- readRDS(psFileName)
        if (min(ps$propensityScore) < max(ps$propensityScore)) {
            ps <- CohortMethod:::computePreferenceScore(ps)

            d1 <- density(ps$preferenceScore[ps$treatment == 1], from = 0, to = 1, n = 100)
            d0 <- density(ps$preferenceScore[ps$treatment == 0], from = 0, to = 1, n = 100)

            d <- data.frame(x = c(d1$x, d0$x), y = c(d1$y, d0$y), treatment = c(rep(1, length(d1$x)),
                                                                                rep(0, length(d0$x))))
            d$y <- d$y/max(d$y)
            d$treatmentName <- as.character(exposureSummary$tName[i])
            d$comparatorName <- as.character(exposureSummary$cName[i])
            # datas[[length(datas) + 1]] <- d
            result <- d

            d$x <- 1 - d$x
            d$treatment <- 1 - d$treatment
            d$treatmentName <- as.character(exposureSummary$cName[i])
            d$comparatorName <- as.character(exposureSummary$tName[i])
            # datas[[length(datas) + 1]] <- d
            result <- rbind(result, d)

            return(result)
        }
    }
    return(NULL)
}

data <- plyr::llply(1:nrow(exposureSummary), preparePlot, exposureSummary = exposureSummary, outcomeModelReference = outcomeModelReference, .progress = "text")
data <- do.call("rbind", data)
saveRDS(data, file.path(diagnosticsFolder, "ps.rds"))
# data <- ps
data$GROUP <- "Target"
data$GROUP[data$treatment == 0] <- "Comparator"
data$GROUP <- factor(data$GROUP, levels = c("Target", "Comparator"))
library(ggplot2)
plot <- ggplot(data, aes(x = x,
                         y = y,
                         color = GROUP,
                         group = GROUP,
                         fill = GROUP)) +
    geom_density(stat = "identity") +
    scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
    scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
    scale_x_continuous("Preference score", limits = c(0, 1)) + scale_y_continuous("Density") +
    facet_grid(treatmentName ~ comparatorName) +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 8, angle = 90, vjust = 0),
          strip.text.y = element_text(size = 8, angle = 0, hjust = 0),
          panel.spacing = unit(0.1, "lines"),
          legend.position = "none")
ggsave(plot = plot, filename = file.path(diagnosticsFolder, "allPs.png"), width = 15, height = 9, dpi = 500)


# Calibration plots -----------------------------------------------------------------------------------
indicationFolder <- file.path(outputFolder, indication)
diagnosticsFolder <- file.path(indicationFolder, "internalDiagnostics")
if (!file.exists(diagnosticsFolder)) {
    dir.create(diagnosticsFolder)
}
calibrationFolder <- file.path(diagnosticsFolder, "calibration")
if (!file.exists(calibrationFolder)) {
    dir.create(calibrationFolder)
}

indicationFolder <- file.path(outputFolder, indication)
exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
analysesSum <- read.csv(file.path(indicationFolder, "analysisSummary.csv"))
pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Legend")
negativeControls <- read.csv(pathToCsv)
negativeControlIds <- negativeControls$conceptId
signalInjectionSum <- read.csv(file.path(indicationFolder, "signalInjectionSummary.csv"))
sample <- 1:nrow(exposureSummary)
sample <- sample(nrow(exposureSummary), 100, replace = FALSE)
for (i in sample) {
    treatmentId <- exposureSummary$tprimeCohortDefinitionId[i]
    comparatorId <- exposureSummary$cprimeCohortDefinitionId[i]
    treatmentConceptId <- exposureSummary$tCohortDefinitionId[i]
    comparatorConceptId <- exposureSummary$cCohortDefinitionId[i]
    treatmentName <- exposureSummary$tName[i]
    comparatorName <- exposureSummary$cName[i]
    for (analysisId in unique(analysesSum$analysisId)) {
        estimates <- analysesSum[analysesSum$analysisId == analysisId & analysesSum$targetId == treatmentId &
                                     analysesSum$comparatorId == comparatorId, ]

        negControls <- estimates[estimates$outcomeId %in% negativeControlIds, ]
        fileName <- file.path(calibrationFolder, paste0("negControls_a",
                                                        analysisId,
                                                        "_t",
                                                        treatmentId,
                                                        "_c",
                                                        comparatorId,
                                                        ".png"))
        title <- paste(treatmentName, "vs.", comparatorName, "- analysis ", analysisId)
        EmpiricalCalibration::plotCalibrationEffect(logRrNegatives = negControls$logRr,
                                                    seLogRrNegatives = negControls$seLogRr,
                                                    title = title,
                                                    xLabel = "Hazard ratio",
                                                    fileName = fileName)
        fileName <- file.path(calibrationFolder, paste0("negControls_a",
                                                        analysisId,
                                                        "_t",
                                                        comparatorId,
                                                        "_c",
                                                        treatmentId,
                                                        ".png"))
        title <- paste(comparatorName, "vs.", treatmentName, "- analysis ", analysisId)
        EmpiricalCalibration::plotCalibrationEffect(logRrNegatives = -negControls$logRr,
                                                    seLogRrNegatives = negControls$seLogRr,
                                                    title = title,
                                                    xLabel = "Hazard ratio",
                                                    fileName = fileName)

        injectedSignals <- signalInjectionSum[signalInjectionSum$exposureId == treatmentConceptId &
                                                  signalInjectionSum$injectedOutcomes != 0, ]
        negativeControlIdSubsets <- unique(injectedSignals$outcomeId)
        injectedSignals <- data.frame(outcomeId = injectedSignals$newOutcomeId,
                                      trueLogRr = log(injectedSignals$targetEffectSize))
        negativeControls <- data.frame(outcomeId = negativeControlIds, trueLogRr = 0)
        data <- rbind(injectedSignals, negativeControls)
        data <- merge(data, estimates[, c("outcomeId", "logRr", "seLogRr")])
        if (length(unique(data$trueLogRr)) > 1) {
            fileName <- file.path(calibrationFolder, paste0("negPosControls_a",
                                                            analysisId,
                                                            "_t",
                                                            treatmentId,
                                                            "_c",
                                                            comparatorId,
                                                            ".png"))
            title <- paste(treatmentName, "vs.", comparatorName, "- analysis ", analysisId)
            EmpiricalCalibration::plotCiCalibrationEffect(logRr = data$logRr,
                                                          seLogRr = data$seLogRr,
                                                          trueLogRr = data$trueLogRr,
                                                          title = title,
                                                          xLabel = "Hazard ratio",
                                                          fileName = fileName)

        }

        injectedSignals <- signalInjectionSum[signalInjectionSum$exposureId == comparatorConceptId &
                                                  signalInjectionSum$injectedOutcomes != 0, ]
        negativeControlIdSubsets <- unique(injectedSignals$outcomeId)
        injectedSignals <- data.frame(outcomeId = injectedSignals$newOutcomeId,
                                      trueLogRr = log(injectedSignals$targetEffectSize))
        negativeControls <- data.frame(outcomeId = negativeControlIds,
                                       trueLogRr = rep(0, length(negativeControlIds)))
        data <- rbind(injectedSignals, negativeControls)
        data <- merge(data, estimates[, c("outcomeId", "logRr", "seLogRr")])
        if (length(unique(data$trueLogRr)) > 1) {
            fileName <- file.path(calibrationFolder, paste0("negPosControls_a",
                                                            analysisId,
                                                            "_t",
                                                            comparatorId,
                                                            "_c",
                                                            treatmentId,
                                                            ".png"))
            title <- paste(comparatorName, "vs.", treatmentName, "- analysis ", analysisId)
            EmpiricalCalibration::plotCiCalibrationEffect(logRr = -data$logRr,
                                                          seLogRr = data$seLogRr,
                                                          trueLogRr = data$trueLogRr,
                                                          title = title,
                                                          xLabel = "Hazard ratio",
                                                          fileName = fileName)
        }
    }
}

# Prior treatments distribution -----------------------------------------------------
indicationFolder <- file.path(outputFolder, indication)
diagnosticsFolder <- file.path(indicationFolder, "internalDiagnostics")
if (!file.exists(diagnosticsFolder)) {
    dir.create(diagnosticsFolder)
}
exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
outcomeModelReference <- readRDS(file.path(indicationFolder, "cmOutput", "outcomeModelReference.rds"))
datas <- list()
for (i in 1:nrow(exposureSummary)) {
    targetId <- exposureSummary$tprimeCohortDefinitionId[i]
    comparatorId <- exposureSummary$cprimeCohortDefinitionId[i]
    cmDataFolder <- outcomeModelReference$cohortMethodDataFolder[outcomeModelReference$targetId == targetId &
                                                                     outcomeModelReference$comparatorId == comparatorId][1]
    cmData <- CohortMethod::loadCohortMethodData(cmDataFolder)
    ref <- ff::as.ram(cmData$covariateRef[cmData$covariateRef$analysisId == 999,])
    covSubset <- ff::as.ram(cmData$covariates[ffbase::`%in%`(cmData$covariates$covariateId, ff::as.ff(ref$covariateId)), ])
    covSubset <- merge(covSubset, cmData$cohorts[, c("rowId", "treatment")])
    counts <- aggregate(covariateValue ~ covariateId + treatment, covSubset, sum)
    counts <- merge(counts, ref[, c("covariateId", "covariateName")])
    counts <- rbind(counts[, c("covariateName", "treatment", "covariateValue")],
                    data.frame(covariateName = "Prior treatments: 0",
                               treatment = c(1,0),
                               covariateValue = c(sum(cmData$cohorts$treatment == 1) - sum(counts$covariateValue[counts$treatment == 1]),
                                                  sum(cmData$cohorts$treatment == 0) - sum(counts$covariateValue[counts$treatment == 0]))))
    counts$covariateName <- as.character(counts$covariateName)
    counts <- counts[order(counts$treatment, counts$covariateName), ]
    counts$targetName <- exposureSummary$tName[i]
    counts$comparatorName <- exposureSummary$cName[i]
    datas[[length(datas) + 1]] <- counts
}
data <- do.call("rbind", datas)
write.csv(data, file.path(diagnosticsFolder, "PriorExposureCounts.csv"), row.names = FALSE)


# Check subgroup covariates -----------------------------------------------------
indicationFolder <- file.path(outputFolder, indication)
diagnosticsFolder <- file.path(indicationFolder, "internalDiagnostics")
if (!file.exists(diagnosticsFolder)) {
    dir.create(diagnosticsFolder)
}
covariateData <- FeatureExtraction::loadCovariateData(file.path(indicationFolder, "allCovariates"))
covariateData$analysisRef

ref <- ff::as.ram(covariateData$covariateRef[covariateData$covariateRef$analysisId == 998,])
covSubset <- ff::as.ram(covariateData$covariates[ffbase::`%in%`(covariateData$covariates$covariateId, ff::as.ff(ref$covariateId)), ])
counts <- aggregate(covariateValue ~ covariateId, covSubset, sum)
counts <- merge(counts, ref[, c("covariateId", "covariateName")])
covariateData$metaData$populationSize

# Propensiy models --------------------------------------------------------------------------------------
indicationFolder <- file.path(outputFolder, indication)
diagnosticsFolder <- file.path(indicationFolder, "internalDiagnostics")
if (!file.exists(diagnosticsFolder)) {
    dir.create(diagnosticsFolder)
}
exposureSummary <- read.csv(file.path(indicationFolder, "pairedExposureSummaryFilteredBySize.csv"))
outcomeModelReference <- readRDS(file.path(indicationFolder, "cmOutput", "outcomeModelReference.rds"))

# for (i in 1:nrow(exposureSummary)) {
getModel <- function(i, exposureSummary, outcomeModelReference) {
    treatmentId <- exposureSummary$tprimeCohortDefinitionId[i]
    comparatorId <- exposureSummary$cprimeCohortDefinitionId[i]
    idx <- outcomeModelReference$targetId == treatmentId &
        outcomeModelReference$comparatorId == comparatorId &
        outcomeModelReference$analysisId == 1
    psFileName <- outcomeModelReference$sharedPsFile[idx][1]
    if (file.exists(psFileName)) {
        ps <- readRDS(psFileName)
        metaData <- attr(ps, "metaData")
        if (is.null(metaData$psError)) {
            cmDataFile <- outcomeModelReference$cohortMethodDataFolder[idx][1]
            cmData <- CohortMethod::loadCohortMethodData(cmDataFile)
            model <- CohortMethod::getPsModel(ps, cmData)
            ff::close.ffdf(cmData$covariates)
            ff::close.ffdf(cmData$covariateRef)
            ff::close.ffdf(cmData$analysisRef)
            # Truncate to first 25 covariates:
            if (nrow(model) > 25) {
              model <- model[1:25, ]
            }
        } else {
            model <- data.frame(coefficient = NA,
                                covariateId = NA,
                                covariateName = paste("Error:", metaData$psError))
        }
        targetName <- exposureSummary$tName[i]
        comparatorName <- exposureSummary$cName[i]
        model$targetId <- treatmentId
        model$targetName <- targetName
        model$comparatorId <- comparatorId
        model$comparatorName <- comparatorName
        model$comparison <- paste(targetName, comparatorName, sep = " vs. ")
        return(model)
    }
    return(NULL)
}

data <- plyr::llply(1:nrow(exposureSummary), getModel, exposureSummary = exposureSummary, outcomeModelReference = outcomeModelReference, .progress = "text")

# data <- plyr::llply(40:60, getModel, exposureSummary = exposureSummary, outcomeModelReference = outcomeModelReference, .progress = "text")

data <- do.call("rbind", data)
write.csv(data, file.path(diagnosticsFolder, "propensityModels.csv"), row.names = FALSE)


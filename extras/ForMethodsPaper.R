# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of Legend
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Code for generating numbers and plots to be presented at the OHDSI 2018 Symposium.

library(DatabaseConnector)
source("extras/LegendMedCentral/DataPulls.R")


connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = paste(Sys.getenv("legendServer"),
                                                            Sys.getenv("legendDatabase"),
                                                            sep = "/"),
                                             port = Sys.getenv("legendPort"),
                                             user = Sys.getenv("legendUser"),
                                             password = Sys.getenv("legendPw"),
                                             schema = Sys.getenv("legendSchema"))
connection <- connect(connectionDetails)


x <- querySql(connection, "SELECT COUNT(*) FROM chronograph WHERE ic IS NULL;")

querySql(connection, "SELECT COUNT(*) FROM chronograph WHERE ic IS NULL;")
querySql(connection, "SELECT COUNT(*) FROM chronograph;")

2600581 / 3967179

# Numbers for diagram ---------------------------------------------------------------------------
exposures <- getExposures(connection = connection, filterByCmResults = TRUE)
drugs <- unique(exposures[exposures$exposureGroup == "Drug", ])
singleDrugs <- drugs[!grepl(" & ", drugs$exposureName), ]
comboDrugs <- drugs[grepl(" & ", drugs$exposureName), ]
writeLines(paste("Single drugs:", nrow(singleDrugs)))
writeLines(paste("Combination drugs:", nrow(comboDrugs)))

classes <- unique(exposures[exposures$exposureGroup == "Drug class", ])
singleClasses <- classes[!grepl(" & ", classes$exposureName), ]
comboClasses <- classes[grepl(" & ", classes$exposureName), ]
writeLines(paste("Single classes:", nrow(singleClasses)))
writeLines(paste("Combination classes:", nrow(comboClasses)))

majorClasses <- unique(exposures[exposures$exposureGroup == "Drug major class", ])
singleMajorClasses <- majorClasses[!grepl(" & ", majorClasses$exposureName), ]
comboCMajorlasses <- majorClasses[grepl(" & ", majorClasses$exposureName), ]
writeLines(paste("Single major classes:", nrow(singleMajorClasses)))
writeLines(paste("Combination major classes:", nrow(comboCMajorlasses)))

comparisons <- querySql(connection, "SELECT target_id, comparator_id FROM cohort_method_result GROUP BY target_id, comparator_id")
colnames(comparisons) <- SqlRender::snakeCaseToCamelCase(colnames(comparisons))
singleDrugComparisons <- comparisons[comparisons$targetId %in% singleDrugs$exposureId &
                                         comparisons$comparatorId %in% singleDrugs$exposureId, ]
writeLines(paste("Single drug comparisons:", nrow(singleDrugComparisons)))
singleClassComparisons <- comparisons[comparisons$targetId %in% singleClasses$exposureId &
                                          comparisons$comparatorId %in% singleClasses$exposureId, ]
writeLines(paste("Single class comparisons:", nrow(singleClassComparisons)))
singleMajorClassComparisons <- comparisons[comparisons$targetId %in% singleMajorClasses$exposureId &
                                               comparisons$comparatorId %in% singleMajorClasses$exposureId, ]
writeLines(paste("Single major class comparisons:", nrow(singleMajorClassComparisons)))
monoDuoDrugComparisons <- comparisons[(comparisons$targetId %in% singleDrugs$exposureId &
                                           comparisons$comparatorId %in% comboDrugs$exposureId) |
                                          (comparisons$targetId %in% comboDrugs$exposureId &
                                               comparisons$comparatorId %in% singleDrugs$exposureId) , ]
writeLines(paste("mono vs duo drug comparisons:", nrow(monoDuoDrugComparisons)))
duoDuoDrugComparisons <- comparisons[(comparisons$targetId %in% comboDrugs$exposureId &
                                          comparisons$comparatorId %in% comboDrugs$exposureId), ]
writeLines(paste("duo vs duo drug comparisons:", nrow(duoDuoDrugComparisons)))

monoDuoClassComparisons <- comparisons[(comparisons$targetId %in% singleClasses$exposureId &
                                            comparisons$comparatorId %in% comboClasses$exposureId) |
                                           (comparisons$targetId %in% comboClasses$exposureId &
                                                comparisons$comparatorId %in% singleClasses$exposureId) , ]
writeLines(paste("mono vs duo class comparisons:", nrow(monoDuoClassComparisons)))
duoDuoClassComparisons <- comparisons[(comparisons$targetId %in% comboClasses$exposureId &
                                           comparisons$comparatorId %in% comboClasses$exposureId), ]
writeLines(paste("duo vs duo class comparisons:", nrow(duoDuoClassComparisons)))

monoDuoMajorClassComparisons <- comparisons[(comparisons$targetId %in% singleMajorClasses$exposureId &
                                                 comparisons$comparatorId %in% comboCMajorlasses$exposureId) |
                                                (comparisons$targetId %in% comboCMajorlasses$exposureId &
                                                     comparisons$comparatorId %in% singleMajorClasses$exposureId) , ]
writeLines(paste("mono vs duo major class comparisons:", nrow(monoDuoMajorClassComparisons)))
duoDuoMajorClassComparisons <- comparisons[(comparisons$targetId %in% comboCMajorlasses$exposureId &
                                                comparisons$comparatorId %in% comboCMajorlasses$exposureId), ]
writeLines(paste("duo vs duo major class comparisons:", nrow(duoDuoMajorClassComparisons)))


writeLines(paste("total comparisons:", nrow(comparisons)))


outcomesUsed <- querySql(connection, "SELECT DISTINCT cmr.outcome_id FROM cohort_method_result cmr INNER JOIN outcome_of_interest ooi ON cmr.outcome_id = ooi.outcome_id")
writeLines(paste("total outcomes observed:", nrow(outcomesUsed)))
tcos <- querySql(connection, "SELECT DISTINCT target_id, comparator_id, cmr.outcome_id FROM cohort_method_result cmr INNER JOIN outcome_of_interest ooi ON cmr.outcome_id = ooi.outcome_id")
writeLines(paste("total tcos observed:", nrow(tcos)))

ncsUsed <- querySql(connection, "SELECT DISTINCT cmr.outcome_id FROM cohort_method_result cmr INNER JOIN negative_control_outcome nco ON cmr.outcome_id = nco.outcome_id")
writeLines(paste("total negative controls observed:", nrow(ncsUsed)))
tcncs <- querySql(connection, "SELECT DISTINCT target_id, comparator_id, cmr.outcome_id FROM cohort_method_result cmr INNER JOIN negative_control_outcome nco ON cmr.outcome_id = nco.outcome_id")
writeLines(paste("total tc - negative controls observed:", nrow(tcncs)))

tcpcs <- querySql(connection, "SELECT DISTINCT target_id, comparator_id, cmr.outcome_id FROM cohort_method_result cmr INNER JOIN positive_control_outcome pco ON cmr.outcome_id = pco.outcome_id")
writeLines(paste("total tc - positive controls observed:", nrow(tcpcs)))


sql <- "
SELECT COUNT(*) FROM (
SELECT DISTINCT database_id, target_id, comparator_id, cohort_method_result.outcome_id, analysis_id
FROM cohort_method_result
INNER JOIN outcome_of_interest
ON cohort_method_result.outcome_id = outcome_of_interest.outcome_id
WHERE calibrated_se_log_rr IS NOT NULL
AND indication_id = 'Hypertension'
) tmp;
"
writeLines("Number of estimates for HOIs:")
querySql(connection, sql)


sql <- "
SELECT COUNT(*) FROM (
SELECT DISTINCT database_id, target_id, comparator_id, cohort_method_result.outcome_id, analysis_id
FROM cohort_method_result
INNER JOIN (
SELECT outcome_id FROM negative_control_outcome WHERE indication_id = 'Hypertension'
UNION ALL
SELECT outcome_id FROM positive_control_outcome WHERE indication_id = 'Hypertension'
) controls
ON cohort_method_result.outcome_id = controls.outcome_id
WHERE calibrated_se_log_rr IS NOT NULL
) tmp;
"
writeLines("Number of estimates for controls:")
querySql(connection, sql)


sql <- "
SELECT COUNT(*) FROM (
SELECT DISTINCT database_id, target_id, comparator_id, analysis_id
FROM cohort_method_result
INNER JOIN outcome_of_interest
ON cohort_method_result.outcome_id = outcome_of_interest.outcome_id
WHERE calibrated_se_log_rr IS NOT NULL
AND indication_id = 'Hypertension'
) tmp;
"
writeLines("Number of calibration models:")
querySql(connection, sql)

# Concordance ------------------------------------------------------------------------------------------

rctEstimates <- read.csv("Documents/SystematicReviewEstimates.csv")
# rctEstimates$hrRct <- exp(rctEstimates$logHR)
# rctEstimates$lbRct <- exp(rctEstimates$logLb)
# rctEstimates$ubRct <- exp(rctEstimates$logUb)
# rctEstimates$logHR <- NULL
# rctEstimates$logLb <- NULL
# rctEstimates$logUb <- NULL
legendEstimates <- getMainResults(connection = connection,
                                  targetIds = unique(rctEstimates$targetId),
                                  comparatorIds = unique(rctEstimates$comparatorId),
                                  outcomeIds = unique(rctEstimates$outcomeId),
                                  databaseIds = "Meta-analysis",
                                  analysisIds = 1)
# legendEstimates$hrLegend <- legendEstimates$rr
# legendEstimates$lbLegend <- legendEstimates$ci95Lb
# legendEstimates$ubLegend <- legendEstimates$ci95Ub
legendEstimates$hrLegend <- legendEstimates$calibratedRr
legendEstimates$lbLegend <- legendEstimates$calibratedCi95Lb
legendEstimates$ubLegend <- legendEstimates$calibratedCi95Ub
combined <- merge(rctEstimates, legendEstimates[, c("targetId", "comparatorId", "outcomeId", "hrLegend", "lbLegend", "ubLegend", "i2")])
combined$someOverlapDm <- combined$lbLegend <= combined$ubDm & combined$ubLegend >= combined$lbDm
combined$someOverlapNm <- combined$lbLegend <= combined$ubNm & combined$ubLegend >= combined$lbNm
combined$completeOverlapDm <- combined$lbLegend >= combined$lbDm & combined$ubLegend <= combined$ubDm
combined$completeOverlapNm <- combined$lbLegend >= combined$lbNm & combined$ubLegend <= combined$ubNm
oddsTest <- function(lb1,hr1,ub1,lb2,hr2,ub2) {
    s1 <- (log(ub1) - log(lb1))/(2*1.96)
    s2 <- (log(ub2) - log(lb2))/(2*1.96)
    se <- sqrt(s1^2 + s2^2)
    z <- (log(hr2) - log(hr1))/se
    dat <- 2*pnorm(-abs(z))
    return(dat)
}
combined$pDifferenceDm <- oddsTest(combined$lbLegend, combined$hrLegend, combined$ubLegend, combined$lbDm, combined$hrDm, combined$ubDm)
combined$pDifferenceNm <- oddsTest(combined$lbLegend, combined$hrLegend, combined$ubLegend, combined$lbNm, combined$hrNm, combined$ubNm)




createPlotForOutcome <- function(outcome, combined) {
    vizData <- rbind(data.frame(Target = combined$targetName,
                                Comparator = combined$comparatorName,
                                outcome = combined$outcomeName,
                                Source = "Direct meta-analysis",
                                hr = combined$hrDm,
                                lb = combined$lbDm,
                                ub = combined$ubDm,
                                completeOverlap = combined$completeOverlapDm,
                                someOverlap = combined$someOverlapDm,
                                pDifference = combined$pDifferenceDm,
                                stringsAsFactors = FALSE),
                     data.frame(Target = combined$targetName,
                                Comparator = combined$comparatorName,
                                outcome = combined$outcomeName,
                                Source = "Network meta-analysis",
                                hr = combined$hrNm,
                                lb = combined$lbNm,
                                ub = combined$ubNm,
                                completeOverlap = combined$completeOverlapNm,
                                someOverlap = combined$someOverlapNm,
                                pDifference = combined$pDifferenceNm,
                                stringsAsFactors = FALSE),
                     data.frame(Target = combined$targetName,
                                Comparator = combined$comparatorName,
                                outcome = combined$outcomeName,
                                Source = "LEGEND meta-analysis",
                                hr = combined$hrLegend,
                                lb = combined$lbLegend,
                                ub = combined$ubLegend,
                                completeOverlap = NA,
                                someOverlap = NA,
                                pDifference = NA,
                                stringsAsFactors = FALSE))
    vizData <- vizData[vizData$outcome == outcome, ]
    vizData$Source <- factor(vizData$Source, levels = c("Network meta-analysis", "Direct meta-analysis", "LEGEND meta-analysis"))
    vizData$Concordance <- "Reference"
    vizData$Concordance[vizData$someOverlap] <- "Partial overlap of confidence intervals"
    vizData$Concordance[vizData$completeOverlap] <- "Full overlap of confidence intervals"
    vizData$Concordance[vizData$pDifference < 0.05] <- "Statistically significant difference (p < 0.05)"
    vizData$Concordance <- factor(vizData$Concordance, levels = c("Reference", "Full overlap of confidence intervals", "Partial overlap of confidence intervals",  "Statistically significant difference (p < 0.05)"))
    vizData$show <- 1
    breaks <- c(0.25, 0.5, 1, 2, 4)
    plot <- ggplot2::ggplot(vizData, ggplot2::aes(x = hr, xmin = lb, xmax = ub, y = Source, color = Concordance, shape = Source)) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = 0.25*show), colour = "#AAAAAA", size = 0.2) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = 0.5*show), colour = "#AAAAAA", size = 0.2) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = 2*show), colour = "#AAAAAA", size = 0.2) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = 4*show), colour = "#AAAAAA", size = 0.2) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = 1*show), size = 0.5) +
        ggplot2::geom_errorbarh(size = 0.6, height = 0.3) +
        ggplot2::geom_point(size = 2) +
        ggplot2::scale_x_log10("Hazard ratio",
                               breaks = breaks,
                               labels = as.character(breaks),
                               position = "bottom") +
        ggplot2::coord_cartesian(xlim = c(0.20, 5)) +
        ggplot2::facet_grid(Target ~ Comparator, switch = "y") +
        ggplot2::scale_shape_manual(values = c(15, 16, 17),
                                    guide = ggplot2::guide_legend(direction = "vertical", reverse = TRUE)) +
        # Colors from http://ksrowell.com/blog-visualizing-data/2012/02/02/optimal-colors-for-graphs/
        ggplot2::scale_color_manual(values = c(rgb(0, 0, 0),
                                               rgb(0.2431373, 0.5882353, 0.3176471),
                                               rgb(0.8549020, 0.4862745, 0.1882353),
                                               rgb(0.8000000, 0.1450980, 0.1607843)),
                                    guide = ggplot2::guide_legend(direction = "vertical", reverse = FALSE)) +
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(size = 8),
                       axis.ticks = ggplot2::element_blank(),
                       strip.background = ggplot2::element_blank(),
                       legend.position = "bottom")
    return(plot)
}

plot1 <- createPlotForOutcome("Heart failure", combined)
plot2 <- createPlotForOutcome("Myocardial infarction", combined)
plot3 <- createPlotForOutcome("Stroke", combined)

# ggplot2::ggsave(filename = sprintf("c:/temp/%s.png", outcome), width = 8, height = 4, dpi = 300)

grabLegend <- function(a.gplot){
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}

mylegend <- grabLegend(plot1)

plot <- gridExtra::grid.arrange(plot1 + ggplot2::theme(legend.position = "none",
                                                       axis.title.x = ggplot2::element_blank(),
                                                       axis.text.x = ggplot2::element_blank()),
                                grid::textGrob("Heart failure", rot = -90),
                                plot2 + ggplot2::theme(legend.position = "none",
                                                       axis.title.x = ggplot2::element_blank(),
                                                       axis.text.x = ggplot2::element_blank(),
                                                       strip.text.x = ggplot2::element_blank()),
                                grid::textGrob("Myocardial infarction", rot = -90),
                                plot3 + ggplot2::theme(legend.position = "none",
                                                       strip.text.x = ggplot2::element_blank()),
                                grid::textGrob("Stroke", rot = -90),
                                mylegend,
                                grid::textGrob(""),
                                ncol = 2,
                                widths = c(100, 3),
                                heights = c(100, 85, 105, 80))


ggplot2::ggsave(filename = "c:/temp/concordance.png", plot, width = 7, height = 7, dpi = 300)



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
paperFolder <- "c:/temp"

connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = paste(Sys.getenv("legendServer"),
                                                            Sys.getenv("legendDatabase"),
                                                            sep = "/"),
                                             port = Sys.getenv("legendPort"),
                                             user = Sys.getenv("legendUser"),
                                             password = Sys.getenv("legendPw"),
                                             schema = Sys.getenv("legendSchema"))
connection <- connect(connectionDetails)


x <- querySql(connection, "SELECT * FROM database;")

querySql(connection, "SELECT COUNT(*) FROM chronograph WHERE ic IS NULL;")
querySql(connection, "SELECT * FROM preference_score_dist WHERE database_id = 'CCAE' LIMIT 100;")

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
comboMajorClasses <- majorClasses[grepl(" & ", majorClasses$exposureName), ]
writeLines(paste("Single major classes:", nrow(singleMajorClasses)))
writeLines(paste("Combination major classes:", nrow(comboMajorClasses)))

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
                                                 comparisons$comparatorId %in% comboMajorClasses$exposureId) |
                                                (comparisons$targetId %in% comboMajorClasses$exposureId &
                                                     comparisons$comparatorId %in% singleMajorClasses$exposureId) , ]
writeLines(paste("mono vs duo major class comparisons:", nrow(monoDuoMajorClassComparisons)))
duoDuoMajorClassComparisons <- comparisons[(comparisons$targetId %in% comboMajorClasses$exposureId &
                                                comparisons$comparatorId %in% comboMajorClasses$exposureId), ]
writeLines(paste("duo vs duo major class comparisons:", nrow(duoDuoMajorClassComparisons)))


writeLines(paste("total comparisons:", nrow(comparisons)))

# x <- comparisons[!(comparisons$targetId %in% c(singleDrugs$exposureId, singleClasses$exposureId, majorClasses$exposureId, comboDrugs$exposureId, comboClasses$exposureId, comboMajorClasses$exposureId)), ]
# querySql(connection, "SELECT * FROM cohort_method_result WHERE target_id = 1066 LIMIT 100")
# querySql(connection, "SELECT * FROM combi_exposure_of_interest WHERE exposure_id = 1066 LIMIT 100")

outcomesToRemove <- c(39, 40, 44) # Kidney Disease, Coronary Heart Disease, Edema

outcomesUsed <- querySql(connection, "SELECT DISTINCT cmr.outcome_id, outcome_name FROM cohort_method_result cmr INNER JOIN outcome_of_interest ooi ON cmr.outcome_id = ooi.outcome_id")
writeLines(paste("total outcomes observed:", nrow(outcomesUsed)))
sql <- "SELECT DISTINCT target_id,
    comparator_id,
    cmr.outcome_id
FROM cohort_method_result cmr
INNER JOIN outcome_of_interest ooi
ON cmr.outcome_id = ooi.outcome_id
WHERE cmr.outcome_id NOT IN (@rem)"
sql <- SqlRender::render(sql, rem = outcomesToRemove)
tcos <- querySql(connection, sql)
writeLines(paste("total tcos observed:", nrow(tcos)))

ncsUsed <- querySql(connection, "SELECT DISTINCT cmr.outcome_id FROM cohort_method_result cmr INNER JOIN negative_control_outcome nco ON cmr.outcome_id = nco.outcome_id")
writeLines(paste("total negative controls observed:", nrow(ncsUsed)))
tcncs <- querySql(connection, "SELECT DISTINCT target_id, comparator_id, cmr.outcome_id FROM cohort_method_result cmr INNER JOIN negative_control_outcome nco ON cmr.outcome_id = nco.outcome_id")
writeLines(paste("total tc - negative controls observed:", nrow(tcncs)))

tcpcs <- querySql(connection, "SELECT DISTINCT target_id, comparator_id, cmr.outcome_id FROM cohort_method_result cmr INNER JOIN positive_control_outcome pco ON cmr.outcome_id = pco.outcome_id")
writeLines(paste("total tc - positive controls observed:", nrow(tcpcs)))

937362 + 737454


sql <- "
SELECT COUNT(*) FROM (
SELECT DISTINCT database_id, target_id, comparator_id, cohort_method_result.outcome_id, analysis_id
FROM cohort_method_result
INNER JOIN outcome_of_interest
ON cohort_method_result.outcome_id = outcome_of_interest.outcome_id
WHERE calibrated_se_log_rr IS NOT NULL
AND indication_id = 'Hypertension'
AND cohort_method_result.outcome_id NOT IN (@rem)
) tmp;
"
sql <- SqlRender::render(sql, rem = outcomesToRemove)
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
legendEstimates <- getMainResults(connection = connection,
                                  targetIds = unique(rctEstimates$targetId),
                                  comparatorIds = unique(rctEstimates$comparatorId),
                                  outcomeIds = unique(rctEstimates$outcomeId),
                                  databaseIds = "Meta-analysis",
                                  analysisIds = 1)
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


combined$ConcordanceDm[combined$someOverlapDm] <- "Partial"
combined$ConcordanceDm[combined$completeOverlapDm] <- "Full"
combined$ConcordanceDm[combined$pDifferenceDm < 0.05] <- "None"
combined$ConcordanceNm[combined$someOverlapNm] <- "Partial"
combined$ConcordanceNm[combined$completeOverlapNm] <- "Full"
combined$ConcordanceNm[combined$pDifferenceNm < 0.05] <- "None"


writeLines(paste("Number of comparisons:", 2*nrow(combined)))
writeLines(paste("Complete overlap DM:", sum(combined$ConcordanceDm == "Full"), "(", 100*sum(combined$ConcordanceDm == "Full")/nrow(combined), "%)"))
writeLines(paste("Partial overlap DM:", sum(combined$ConcordanceDm == "Partial"), "(", 100*sum(combined$ConcordanceDm == "Partial")/nrow(combined), "%)"))
writeLines(paste("No overlap DM:", sum(combined$ConcordanceDm == "None"), "(", 100*sum(combined$ConcordanceDm == "None")/nrow(combined), "%)"))

writeLines(paste("Complete overlap NM:", sum(combined$ConcordanceNm == "Full"), "(", 100*sum(combined$ConcordanceNm == "Full")/nrow(combined), "%)"))
writeLines(paste("Partial overlap NM:", sum(combined$ConcordanceNm == "Partial"), "(", 100*sum(combined$ConcordanceNm == "Partial")/nrow(combined), "%)"))
writeLines(paste("No overlap NM:", sum(combined$ConcordanceNm == "None"), "(", 100*sum(combined$ConcordanceNm == "None")/nrow(combined), "%)"))


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
        ggplot2::coord_cartesian(xlim = c(1/3, 3)) +
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


ggplot2::ggsave(filename = file.path(paperFolder, "concordance.png"), plot, width = 7, height = 7, dpi = 300)

# Internal validity ----------------------------------------------------------
connection <- connect(connectionDetails)

# Coverage
sql <- "
SELECT COUNT(*) AS control_count,
    SUM(coverage) AS coverage,
    SUM(coverage_calibrated) AS coverage_calibrated
FROM (
    SELECT CASE WHEN ci_95_lb <= effect_size AND ci_95_ub >= effect_size THEN 1 ELSE 0 END AS coverage,
        CASE WHEN calibrated_ci_95_lb <= effect_size AND calibrated_ci_95_ub >= effect_size THEN 1 ELSE 0 END AS coverage_calibrated
    FROM (
        SELECT cohort_method_result.*,
          CAST(1 AS numeric) AS effect_size
        FROM cohort_method_result
        INNER JOIN negative_control_outcome
            ON cohort_method_result.outcome_id = negative_control_outcome.outcome_id
        WHERE calibrated_se_log_rr IS NOT NULL

        UNION ALL

        SELECT cohort_method_result.*,
          effect_size
        FROM cohort_method_result
        INNER JOIN positive_control_outcome
            ON cohort_method_result.outcome_id = positive_control_outcome.outcome_id
                AND cohort_method_result.target_id = positive_control_outcome.exposure_id
        WHERE calibrated_se_log_rr IS NOT NULL
    ) tmp1
) tmp2;
"
coverage <- querySql(connection, sql)
writeLines(paste("Total control estimates:", coverage$CONTROL_COUNT))
writeLines(paste("Coverage uncalibrated:", 100 * coverage$COVERAGE / coverage$CONTROL_COUNT, "%"))
writeLines(paste("Coverage calibrated:", 100 * coverage$COVERAGE_CALIBRATED / coverage$CONTROL_COUNT, "%"))




# Transitivity
sql <- "
SELECT cohort_method_result.*
FROM cohort_method_result
INNER JOIN outcome_of_interest
ON cohort_method_result.outcome_id = outcome_of_interest.outcome_id
WHERE calibrated_se_log_rr IS NOT NULL
    AND indication_id = 'Hypertension'
    AND database_id = 'Meta-analysis'
    AND analysis_id IN (1, 2, 3, 4);
"
estimates <- querySql(connection, sql)
colnames(estimates) <- SqlRender::snakeCaseToCamelCase(colnames(estimates))
saveRDS(estimates, "c:/temp/maEstimates.rds")
estimates <- readRDS("c:/temp/maEstimates.rds")

d <- estimates
d$sign <- d$calibratedCi95Lb > 1 | d$calibratedCi95Ub < 1
sign <- d[d$sign, ]
ab <- data.frame(nameA = sign$targetId,
                 nameB = sign$comparatorId,
                 outcome = sign$outcomeId,
                 analysisId = sign$analysisId,
                 increase = sign$calibratedRr > 1,
                 rrAB = sign$calibratedRr,
                 lbAB = sign$calibratedCi95Lb,
                 ubAB = sign$calibratedCi95Ub)
bc <- data.frame(nameB = sign$targetId,
                 nameC = sign$comparatorId,
                 outcome = sign$outcomeId,
                 analysisId = sign$analysisId,
                 increase = sign$calibratedRr > 1,
                 rrBC = sign$calibratedRr,
                 lbBC = sign$calibratedCi95Lb,
                 ubBC = sign$calibratedCi95Ub)
abc <- merge(ab, bc)
ac <- data.frame(nameA = d$targetId,
                 nameC = d$comparatorId,
                 outcome = d$outcomeId,
                 analysisId = d$analysisId,
                 rrAC = d$calibratedRr,
                 lbAC = d$calibratedCi95Lb,
                 ubAC = d$calibratedCi95Ub)
abcPlusAc <- merge(abc, ac)

agree <- (abcPlusAc$increase & abcPlusAc$lbAC > 1) | (!abcPlusAc$increase & abcPlusAc$ubAC < 1)
mean(agree)
length(agree)
sum(agree)

# Between-database heterogeneity
library(meta)
library(ggplot2)

sql <- "
SELECT cohort_method_result.outcome_id,
    target_id,
    comparator_id,
    database_id,
    analysis_id,
    log_rr,
    se_log_rr,
    calibrated_log_rr,
    calibrated_se_log_rr
FROM cohort_method_result
INNER JOIN outcome_of_interest
ON cohort_method_result.outcome_id = outcome_of_interest.outcome_id
WHERE calibrated_se_log_rr IS NOT NULL
    AND indication_id = 'Hypertension'
    AND database_id != 'Meta-analysis'
    AND analysis_id = 1;
"
d <- querySql(connection, sql)
colnames(d) <- SqlRender::snakeCaseToCamelCase(colnames(d))
saveRDS(d, "c:/temp/nonMaEstimates.rds")
d <- readRDS("c:/temp/nonMaEstimates.rds")

d <- d[d$analysisId == 1, ]
dd <- aggregate(databaseId ~ targetId + comparatorId + outcomeId, data = d, length)
dd <- dd[dd$databaseId >= 4, ]
dd$databaseId <- NULL
nrow(dd)

d <- merge(d, dd)


computeI2 <- function(studies, calibrated = TRUE) {
    if (calibrated) {
        meta <- meta::metagen(studies$calibratedLogRr, studies$calibratedSeLogRr, sm = "RR")
    } else {
        meta <- meta::metagen(studies$logRr, studies$seLogRr, sm = "RR")
    }
    return(meta$I2)
}

splitD <- split(d, paste(d$targetId, d$comparatorId, d$outcomeId))
cluster <- ParallelLogger::makeCluster(15)
i2Cal <- ParallelLogger::clusterApply(cluster, splitD, computeI2, calibrated = TRUE)
i2Cal <- do.call("rbind", i2Cal)
i2 <- ParallelLogger::clusterApply(cluster, splitD, computeI2, calibrated = FALSE)
i2 <- do.call("rbind", i2)
ParallelLogger::stopCluster(cluster)

ddd <- data.frame(i2 = c(i2, i2Cal),
                  group = c(rep("Uncalibrated", length(i2)), rep("Calibrated", length(i2Cal))))

# ggplot(ddd, aes(x=i2, group = group, color = group, fill = group)) +
#     geom_density() +
#     scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
#     scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
#     theme(legend.title = ggplot2::element_blank())

ggplot(ddd, aes(x=i2, group = group, color = group, fill = group)) +
    geom_histogram(binwidth = 0.05, boundary = 0, position = "identity") +
    scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.4))) +
    scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.4))) +
    scale_x_continuous(expression(i^2)) +
    scale_y_continuous("TCO triplets") +
    theme(legend.title = ggplot2::element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = rgb(0.25,0.25,0.25, alpha = 0.2)),
          panel.grid.major.x = element_line(color = rgb(0.25,0.25,0.25, alpha = 0.2)))

ggsave(file.path(paperFolder, "I2.png"), width = 4.5, height = 2, dpi = 300)
length(i2Cal)
mean(i2Cal < 0.25)
mean(i2 < 0.25)



# Exemplar study ----------------------------------------------
# targetId <- 15 # THZ
# comparatorId <- 1 # ACE
# outcomeId <- 2 # AMI
targetId <- 1308216 # lisinopril
comparatorId <- 1332418 # Amlodipine
outcomeId <- 32 # Angioedema

estimates <- getMainResults(connection = connection,
                            targetIds = targetId,
                            comparatorIds = comparatorId,
                            outcomeIds = outcomeId,
                            analysisIds = 1)
estimates <- estimates[!is.na(estimates$calibratedSeLogRr), ]
estimates$databaseId[estimates$databaseId == "NHIS_NSC"] <- "NHIS"
source("extras/LegendMedCentral/PlotsAndTables.R")
plot <- plotForest(estimates, limits = c(1/6, 6))
ggplot2::ggsave(file.path(paperFolder, "Forest.png"), plot = plot, width = 13, height = 4, dpi = 400)

balanceSummary <- getCovariateBalanceSummary(connection, targetId, comparatorId, analysisId = 2)
balanceSummary$databaseId[balanceSummary$databaseId == "NHIS_NSC"] <- "NHIS"
stringToVars <- function(string) {
    parts <- as.numeric(unlist(strsplit(gsub("\\{|\\}", "", string), ",")))
    parts <- as.data.frame(matrix(parts, ncol = 5, byrow = TRUE))
    colnames(parts) <- c("ymin", "lower", "median", "upper", "ymax")
    return(parts)
}
before <- stringToVars(balanceSummary$percentilesBefore)
after <- stringToVars(balanceSummary$percentilesAfter)
balanceSummary <- data.frame(databaseId = balanceSummary$databaseId,
                             covariateCount = balanceSummary$covariateCount,
                             maxBefore = pmax(abs(before$ymax), abs(before$ymin)),
                             maxAfter = pmax(abs(after$ymax), abs(after$ymin)))
powerTable <- merge(estimates, balanceSummary, all.x = TRUE)
powerTable$targetYears <- powerTable$targetDays/365.25
powerTable$comparatorYears <- powerTable$comparatorDays/365.25
powerTable <- powerTable[, c("databaseId",
                            "targetSubjects",
                            "comparatorSubjects",
                            "targetYears",
                            "comparatorYears",
                            "targetOutcomes",
                            "comparatorOutcomes",
                            "covariateCount",
                            "maxBefore",
                            "maxAfter")]
dbResults <- powerTable[powerTable$databaseId != "Meta-analysis", ]
dbResults <- dbResults[order(dbResults$databaseId), ]
maResult <- powerTable[powerTable$databaseId == "Meta-analysis", ]
powerTable <- rbind(dbResults, maResult)
colnames(powerTable) <- c("Source",
                          "THZ subjects",
                          "ACE subjects",
                          "THZ years",
                          "ACE years",
                          "THZ events",
                          "ACE events",
                          "Covariate count",
                          "Before strat. max SD ",
                          "After strat. max SD ")
write.csv(powerTable, file.path(paperFolder, "Power.csv"), row.names = FALSE)


estimatesSens <- getMainResults(connection = connection,
                            targetIds = targetId,
                            comparatorIds = comparatorId,
                            outcomeIds = outcomeId,
                            databaseIds = "Meta-analysis",
                            analysisIds = 2:4)
paste0(formatC(exp(estimatesSens$calibratedLogRr),  digits = 2, format = "f"),
       " (",
       formatC((estimatesSens$calibratedCi95Lb), digits = 2, format = "f"),
       "-",
       formatC((estimatesSens$calibratedCi95Ub), digits = 2, format = "f"),
       ")")

disconnect(connection)

# Blood pressure sensitivity analysis --------------------------------------------

# Balance table
balanceOriginal <- read.csv("Documents/bpData/Balance.csv")
balanceAdjustBp <- read.csv("Documents/bpData/BalanceAdjustBp.csv")
rctEstimates <- read.csv("Documents/SystematicReviewEstimates.csv")
balanceOriginal <- data.frame(targetId = balanceOriginal$targetId,
                              comparatorId = balanceOriginal$comparatorId,
                              covariateName = balanceOriginal$covariateName,
                              beforeSdOriginal = balanceOriginal$beforeMatchingStdDiff,
                              afterSdOriginal = balanceOriginal$afterMatchingStdDiff)
balanceAdjustBp <- data.frame(targetId = balanceAdjustBp$targetId,
                              comparatorId = balanceAdjustBp$comparatorId,
                              covariateName = balanceAdjustBp$covariateName,
                              beforeSdAdjustBp = balanceAdjustBp$beforeMatchingStdDiff,
                              afterSdAdjustBp = balanceAdjustBp$afterMatchingStdDiff)
selectedTcs <- unique(data.frame(targetId = rctEstimates$comparatorId,
                                 targetName = rctEstimates$comparatorName,
                                 comparatorId = rctEstimates$targetId,
                                 comparatorName = rctEstimates$targetName))
balance <- merge(balanceOriginal, balanceAdjustBp)
balance <- merge(balance, selectedTcs)
balance <- balance[order(balance$targetName, balance$comparatorName, balance$comparatorName), ]
balance <- balance[, c("targetName", "comparatorName", "covariateName", "beforeSdOriginal", "afterSdOriginal", "beforeSdAdjustBp", "afterSdAdjustBp")]
balance$covariateName <- as.character(balance$covariateName)
balance$covariateName[balance$covariateName == "BP systolic"] <- "Systolic"
balance$covariateName[balance$covariateName == "BP diastolic"] <- "Diastolic"
write.csv(balance, file.path(paperFolder, "BalanceBp.csv"), row.names = FALSE)

# Estimates with and without BP adjustment
estimates <-  read.csv("Documents/bpData/HrsData_all.csv")
estimates <- estimates[estimates$estimate == "Calibrated", ]
rctEstimates <- read.csv("Documents/SystematicReviewEstimates.csv")
combined <- merge(rctEstimates[, c("targetId", "targetName", "comparatorId", "comparatorName", "outcomeId")], estimates)

computeConcordance <- function(subset) {
    estimateOriginal <- subset[subset$type == "Original", ]
    estimateAdjustBp <- subset[subset$type != "Original", ]
    # someOverlap <- estimateOriginal$ci95lb <= estimateAdjustBp$ci95ub & estimateOriginal$ci95ub >= estimateAdjustBp$ci95lb
    completeOverlap <- (estimateOriginal$ci95lb >= estimateAdjustBp$ci95lb & estimateOriginal$ci95ub <= estimateAdjustBp$ci95ub) |
        (estimateOriginal$ci95lb <= estimateAdjustBp$ci95lb & estimateOriginal$ci95ub >= estimateAdjustBp$ci95ub)
    oddsTest <- function(lb1,hr1,ub1,lb2,hr2,ub2) {
        s1 <- (log(ub1) - log(lb1))/(2*1.96)
        s2 <- (log(ub2) - log(lb2))/(2*1.96)
        se <- sqrt(s1^2 + s2^2)
        z <- (log(hr2) - log(hr1))/se
        dat <- 2*pnorm(-abs(z))
        return(dat)
    }
    pDifference <- oddsTest(estimateOriginal$ci95lb , estimateOriginal$rr, estimateOriginal$ci95ub , estimateAdjustBp$ci95lb, estimateAdjustBp$rr, estimateAdjustBp$ci95ub)
    if (pDifference < 0.05) {
        concordance <- "Statistically significant difference (p < 0.05)"
    } else if (completeOverlap) {
        concordance <- "Full overlap of confidence intervals"
    } else {
        concordance <- "Partial overlap of confidence intervals"
    }
    return(data.frame(targetId = estimateOriginal$targetId,
                      comparatorId = estimateOriginal$comparatorId,
                      outcomeId = estimateOriginal$outcomeId,
                      Concordance = concordance,
                      stringsAsFactors = FALSE))
}
subsets <- split(combined, paste(combined$targetId, combined$comparatorId, combined$outcomeId))
concordance <- lapply(subsets, computeConcordance)
concordance <- do.call("rbind", concordance)
combined <- merge(combined, concordance)
combined$Concordance[combined$type == "Original"] <- "Reference"
combined$type <- as.character(combined$type)
combined$type[combined$type == "Adjusting for\nblood pressure"] <- "Adjusting for blood pressure"

outcome = "Stroke"
createPlotForOutcome <- function(outcome, combined) {
    vizData <- combined[combined$outcomeName == outcome, ]
    vizData$Analysis <- factor(vizData$type, levels = c("Adjusting for blood pressure", "Original"))
    vizData$Concordance <- factor(vizData$Concordance, levels = c("Reference", "Partial overlap of confidence intervals", "Full overlap of confidence intervals", "Statistically significant difference (p < 0.05)"))
    vizData$show <- 1
    vizData
    breaks <- c(0.25, 0.5, 1, 2, 4)
    plot <- ggplot2::ggplot(vizData, ggplot2::aes(x = rr, xmin = ci95lb, xmax = ci95ub, y = Analysis, color = Concordance, shape = Analysis)) +
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
        ggplot2::coord_cartesian(xlim = c(1/3, 3)) +
        ggplot2::facet_grid(targetName ~ comparatorName, switch = "y") +
        ggplot2::scale_shape_manual(values = c(15, 16, 17),
                                    guide = ggplot2::guide_legend(direction = "vertical", reverse = TRUE)) +
        # Colors from http://ksrowell.com/blog-visualizing-data/2012/02/02/optimal-colors-for-graphs/
        ggplot2::scale_color_manual(values = c(rgb(0, 0, 0),
                                               rgb(0.8549020, 0.4862745, 0.1882353),
                                               rgb(0.2431373, 0.5882353, 0.3176471),
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
combined$outcomeName <- as.character(combined$outcomeName)
combined$outcomeName[combined$outcomeName == "Acute myocardial infarction"] <- "Myocardial infarction"
plot1 <- createPlotForOutcome("Heart failure", combined)
plot2 <- createPlotForOutcome("Myocardial infarction", combined)
plot3 <- createPlotForOutcome("Stroke", combined)

# ggplot2::ggsave(filename = sprintf("c:/temp/%s.png", outcome), width = 8, height = 4, dpi = 300)

grabLegend <- function(a.gplot){
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}

mylegend <- grabLegend(plot2)

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


ggplot2::ggsave(filename = file.path(paperFolder, "AdjustBpEffect.png"), plot, width = 7, height = 7, dpi = 300)

# Sample sizes ---------------------------------------------------------
sql <- "
SELECT MIN(subjects) AS min_subjects,
    PERCENTILE_DISC(0.5) WITHIN GROUP (ORDER BY subjects) AS median_subjects,
    MAX(subjects) AS max_subjects
FROM (
    SELECT
        target_subjects + comparator_subjects AS subjects
    FROM cohort_method_result
    WHERE database_id != 'Meta-analysis'
        AND calibrated_se_log_rr IS NOT NULL
) tmp;"
sizes <- querySql(connection, sql)
sizes

# Edges for chord diagram -----------------------------------------------
sql <- "
WITH exposure AS (
  SELECT single_exposure_of_interest.exposure_id,
    exposure_name
  FROM single_exposure_of_interest
  INNER JOIN exposure_group
    ON single_exposure_of_interest.exposure_id = exposure_group.exposure_id
  WHERE indication_id = 'Hypertension'
    AND exposure_group = 'Drug'

  UNION ALL

  SELECT combi_exposure_of_interest.exposure_id,
    'co-amilozide' AS exposure_name
  FROM combi_exposure_of_interest
  INNER JOIN single_exposure_of_interest exposure_1
    ON single_exposure_id_1 = exposure_1.exposure_id
  INNER JOIN single_exposure_of_interest exposure_2
    ON single_exposure_id_2 = exposure_2.exposure_id
  WHERE combi_exposure_of_interest.indication_id = 'Hypertension'
    AND ((exposure_1.exposure_name = 'Amiloride' AND exposure_2.exposure_name = 'Hydrochlorothiazide')
      OR (exposure_1.exposure_name = 'Hydrochlorothiazide' AND exposure_2.exposure_name = 'Amiloride'))
)
SELECT target.exposure_name,
  comparator.exposure_name,
  MAX(target_subjects + comparator_subjects) AS subjects
FROM cohort_method_result
INNER JOIN exposure target
ON cohort_method_result.target_id = target.exposure_id
INNER JOIN exposure comparator
ON cohort_method_result.comparator_id = comparator.exposure_id
WHERE calibrated_se_log_rr IS NOT NULL
    AND database_id = 'Meta-analysis'
    AND analysis_id IN (1, 2, 3, 4)
GROUP BY target.exposure_name,
  comparator.exposure_name;
"
edges <- querySql(connection, sql)
colnames(edges) <- c("from", "to", "value")
edges$study <- "Meta"
edges$from <- tolower(edges$from)
edges$to <- tolower(edges$to)
write.csv(edges, "extras/LEGEND_Meta_connect.csv", row.names = FALSE)

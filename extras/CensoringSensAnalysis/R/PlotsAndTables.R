# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of CensoringSensAnalysis
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

plotForest <- function(indicationFolder, sensAnalysisFolder, targetId, comparatorId) {
  outcomesOfInterest <- read.csv(file.path(indicationFolder, "export", "outcome_of_interest.csv"))
  colnames(outcomesOfInterest) <- SqlRender::snakeCaseToCamelCase(colnames(outcomesOfInterest))



  newEstimates <- read.csv(file.path(sensAnalysisFolder, "calibratedEstimates.csv"), stringsAsFactors = FALSE)
  newEstimates <- newEstimates[newEstimates$outcomeId %in% outcomesOfInterest$outcomeId, ]
  newEstimates <- merge(newEstimates, outcomesOfInterest[, c("outcomeId", "outcomeName")])

  oldEstimates <- read.csv(file.path(indicationFolder, "export", "cohort_method_result.csv"), stringsAsFactors = FALSE)
  colnames(oldEstimates) <- SqlRender::snakeCaseToCamelCase(colnames(oldEstimates))
  oldEstimates <- oldEstimates[oldEstimates$targetId == targetId &
                                 oldEstimates$comparatorId == comparatorId &
                                 oldEstimates$analysisId %in% c(1,3) &
                                 oldEstimates$outcomeId %in% outcomesOfInterest$outcomeId, ]
  oldEstimates <- merge(oldEstimates, outcomesOfInterest[, c("outcomeId", "outcomeName")])

  vizData <- rbind(data.frame(analysisId = oldEstimates$analysisId,
                              rr = oldEstimates$rr,
                              ci95lb = oldEstimates$ci95Lb,
                              ci95ub = oldEstimates$ci95Ub,
                              outcomeName = oldEstimates$outcomeName,
                              type = "Original",
                              estimate = "Uncalibrated"),
                   data.frame(analysisId = newEstimates$analysisId,
                              rr = newEstimates$rr,
                              ci95lb = newEstimates$ci95lb,
                              ci95ub = newEstimates$ci95ub,
                              outcomeName = newEstimates$outcomeName,
                              type = "With censoring",
                              estimate = "Uncalibrated"),
                   data.frame(analysisId = oldEstimates$analysisId,
                              rr = oldEstimates$calibratedRr,
                              ci95lb = oldEstimates$calibratedCi95Lb,
                              ci95ub = oldEstimates$calibratedCi95Ub,
                              outcomeName = oldEstimates$outcomeName,
                              type = "Original",
                              estimate = "Calibrated"),
                   data.frame(analysisId = newEstimates$analysisId,
                              rr = newEstimates$calibratedRr,
                              ci95lb = newEstimates$calibratedCi95Lb,
                              ci95ub = newEstimates$calibratedCi95Ub,
                              outcomeName = newEstimates$outcomeName,
                              type = "With censoring",
                              estimate = "Calibrated"))
  vizData <- vizData[!is.na(vizData$ci95lb), ]


  outcomeNames <- unique(vizData$outcomeName)
  outcomeNames <- outcomeNames[order(outcomeNames, decreasing = TRUE)]
  vizData$y <- match(vizData$outcomeName, outcomeNames) - 0.1 + 0.2*(vizData$type == "Original")


  plotHrs <- function(vizData, fileName) {
    breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 8, 10)
    odd <- seq(1, length(outcomeNames), by = 2)
    bars <- data.frame(xmin = 0.01,
                       xmax = 100,
                       ymin = odd - 0.5,
                       ymax = odd + 0.5,
                       type = "Original",
                       rr = 1,
                       y = 1)
    vizData$estimate <- factor(vizData$estimate, levels = c("Uncalibrated", "Calibrated"))
    plot <- ggplot2::ggplot(vizData, ggplot2::aes(x = rr, y = y, color = type, shape = type, xmin = ci95lb, xmax = ci95ub)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#EEEEEE", colour = "#EEEEEE", size = 0, data = bars) +
      ggplot2::geom_vline(xintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.25) +
      ggplot2::geom_vline(xintercept = 1, size = 0.5) +
      ggplot2::geom_errorbarh(alpha = 0.65, size = 0.6, height = 0.3) +
      ggplot2::geom_point(alpha = 0.65, size = 2) +
      ggplot2::scale_y_continuous(breaks = 1:length(outcomeNames), labels = outcomeNames) +
      ggplot2::coord_cartesian(xlim = c(0.1, 10)) +
      ggplot2::scale_x_log10(breaks = breaks, labels = breaks) +
      ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.65), rgb(0, 0, 0.8, alpha = 0.65))) +
      ggplot2::xlab("Hazard Ratio") +
      ggplot2::facet_grid(~estimate) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                     legend.title = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     strip.background = ggplot2::element_blank())
    ggplot2::ggsave(filename = fileName, plot = plot, width = 8, height = 10, dpi = 400)
  }
  plotHrs(vizData[vizData$analysisId == 1, ], file.path(sensAnalysisFolder, sprintf("Hrs_stratification.png", 1)))
  plotHrs(vizData[vizData$analysisId == 3, ], file.path(sensAnalysisFolder, sprintf("Hrs_matching.png", 3)))

  rawData <- vizData
  rawData$y <- NULL
  rawData$analysis <- "PS stratification"
  rawData$analysis[rawData$analysisId == 3] <- "PS matching"
  write.csv(rawData, file.path(sensAnalysisFolder, "All_hrs.csv"), row.names = FALSE)
}

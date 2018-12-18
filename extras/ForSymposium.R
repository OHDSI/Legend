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

# Numbers for presentation ---------------------------------------------------------------------------
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


# Sunburst --------------------------------------------------------------------------------------
counts <- querySql(connection, "SELECT exposure_id, subjects FROM attrition WHERE database_id = 'CCAE' AND target_id IS NULL AND sequence_number = 3")
colnames(counts) <- SqlRender::snakeCaseToCamelCase(colnames(counts))
counts <- merge(counts,
                unique(data.frame(exposureId = exposures$exposureId,
                                  exposureName = exposures$exposureName)))
cutOut <- "mono"

createSunburts <- function(counts, cutOut = "duo") {
    d <- counts[counts$exposureId %in% drugs$exposureId, ]
    d$exposureName <- as.character(d$exposureName)
    d$type <- "mono"
    d$type[grepl(" & ", d$exposureName)] <- "duo"
    d$exposureName[d$type == cutOut] <- cutOut
    d <- aggregate(subjects ~ exposureName, d, sum)
    d$fraction = d$subjects / sum(d$subjects)
    d$o <- 1
    if (cutOut == "duo") {
        d$o[d$exposureName == cutOut] <- 2
    } else {
        d$o[d$exposureName == cutOut] <- 0
    }
    d = d[order(d$o, d$fraction), ]
    d$ymax = cumsum(d$fraction)
    d$ymin = c(0, head(d$ymax, n = -1))
    d$xmax <- 4
    d$xmin <- 3
    # d$ymax[d$exposureName == cutOut] <- d$ymax[d$exposureName == cutOut] - 0.01
    # d$ymin[d$exposureName == cutOut] <- d$ymin[d$exposureName == cutOut] + 0.01
    d$xmax[d$exposureName == cutOut] <- 4
    d$xmin[d$exposureName == cutOut] <- 3

    library(ggplot2)
    plot <- ggplot(d, aes(fill = exposureName, ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin)) +
        geom_rect(data = d[d$exposureName != cutOut, ], color = rgb(1,1,1)) +
        geom_rect(fill = rgb(0.5, 0.5, 0.5), data = d[d$exposureName == cutOut, ], color = rgb(1,1,1)) +
        coord_polar(theta = "y") +
        xlim(c(0, 4.5)) +
        labs(fill = "Exposure") +
        theme(panel.grid = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              panel.background = element_blank())
    ggsave(sprintf("c:/temp/Legend%s.png", cutOut), plot, width = 10, height = 5)
    plot <- plot + theme(legend.position = "none")
    ggsave(sprintf("c:/temp/Legend%s_nolegend.png", cutOut), plot, width = 5, height = 5)
}

# PS plot posters --------------------------------------------------------------------------------------
databases <- getDatabases(connection)
databases <- databases[databases$isMetaAnalysis == 0, ]
exposures <- getExposures(connection = connection, filterByCmResults = FALSE)
drugs <- exposures[exposures$exposureGroup == "Drug", ]
classes <- exposures[exposures$exposureGroup == "Drug class", ]


createPoster <- function(connection, databaseId = "MDCD", exposureGroup = "Drug") {
    writeLines(sprintf("Creating plot for %s at %s level", databaseId, exposureGroup))
    eoi <- exposures[exposures$exposureGroup == exposureGroup, ]
    ps <- getPs(connection = connection,
                databaseId = databaseId,
                targetIds = eoi$exposureId,
                comparatorIds = eoi$exposureId)
    idx <- ps$targetId > ps$comparatorId
    ps$preferenceScore[idx] <- 1 - ps$preferenceScore[idx]
    ps <- merge(ps, data.frame(targetId = eoi$exposureId,
                               targetName = eoi$exposureName))
    ps <- merge(ps, data.frame(comparatorId = eoi$exposureId,
                               comparatorName = eoi$exposureName))
    ps <- rbind(data.frame(targetName = ps$targetName,
                           comparatorName = ps$comparatorName,
                           x = ps$preferenceScore,
                           y = ps$targetDensity,
                           group = "Target"),
                data.frame(targetName = ps$targetName,
                           comparatorName = ps$comparatorName,
                           x = ps$preferenceScore,
                           y = ps$comparatorDensity,
                           group = "Comparator"))

    # Normalize y per trellis:
    maxY <- aggregate(y ~ targetName + comparatorName, ps, max)
    colnames(maxY)[colnames(maxY) == "y"] <- "maxY"
    ps <- merge(ps, maxY)
    ps$y <- ps$y / ps$maxY

    ps$group <- factor(ps$group, levels = c("Target", "Comparator"))
    plot <- ggplot2::ggplot(ps, ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) +
        ggplot2::geom_density(stat = "identity") +
        ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
        ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
        ggplot2::scale_x_continuous("Preference score", limits = c(0, 1)) +
        ggplot2::scale_y_continuous("Density") +
        ggplot2::facet_grid(cols = ggplot2::vars(comparatorName),
                            rows = ggplot2::vars(targetName),
                            labeller = ) +
        ggplot2::theme(legend.title = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       strip.background = ggplot2::element_blank(),
                       strip.text.x = ggplot2::element_text(size = 13, angle = 90, hjust = 0),
                       strip.text.y = ggplot2::element_text(size = 13, angle = 0, hjust = 0),
                       panel.spacing = ggplot2::unit(0.1, "lines"),
                       legend.position = "none")
    fileName <- file.path("c:/temp/posters", sprintf("Plots_%s_%s.png", databaseId, exposureGroup))
    ggplot2::ggsave(filename = fileName, plot = plot, width = 32, height = 20, dpi = 300)
}

dbs <- c("JMDC", "IMSG", "MDCD", "MDCR", "NHIS_NSC")
dbs <- c("Optum")
exposureGroups <- c("Drug", "Drug class")
for (db in dbs) {
    for (exposureGroup in exposureGroups) {
        createPoster(connection = connection,
                     databaseId = db,
                     exposureGroup = exposureGroup)
    }
}

# Panther not yet in DB
data <- readRDS("c:/temp/Posters/ps.rds")
exposures <- getExposures(connection = connection, filterByCmResults = FALSE)
drugs <- exposures[exposures$exposureGroup == "Drug", ]
classes <- exposures[exposures$exposureGroup == "Drug class", ]
dataDrugs <- data[data$targetId %in% drugs$exposureId, ]
dataDrugClasses <- data[data$targetId %in% classes$exposureId &
                            data$comparatorId %in% classes$exposureId, ]
ps <- dataDrugClasses
ps$group <- "Target"
ps$group[ps$treatment == 0] <- "Comparator"
ps$group <- factor(ps$group, levels = c("Target", "Comparator"))
databaseId <- "Panther"
exposureGroup <- "Drug class"


ps <- dataDrugs
ps$group <- "Target"
ps$group[ps$treatment == 0] <- "Comparator"
ps$group <- factor(ps$group, levels = c("Target", "Comparator"))
databaseId <- "Panther"
exposureGroup <- "Drug"


# Overall scatter plot ----------------------------------------
outcomes <- getOutcomes(connection)
data <- getMainResults(connection, outcomeIds = outcomes$outcomeId, analysisIds = 1, estimatesOnly = FALSE)
saveRDS(data, "r:/legend/mainResults.rds")
source("extras/LegendMedCentral/PlotsAndTables.R")
d <- data.frame(logRr = data$calibratedLogRr,
                seLogRr = data$calibratedSeLogRr,
                ci95Lb = data$calibratedCi95Lb,
                ci95Ub = data$calibratedCi95Ub)
d <- d[!is.na(d$seLogRr), ]
plot <- plotLargeScatter(d, "Hazard ratio", pointSize = 0.2)
ggplot2::ggsave("r:/legend/plot.png", width = 10, height = 6, dpi = 300)

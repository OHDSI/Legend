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

# PS plots hydrochlorathiazide vs chlorthaladone --------------------------------------------------------------
databaseIds <-  c("CCAE", "Optum", "Panther")
targetId <- 1395058
comparatorId <- 974166
exposures <- getExposures(connection = connection,
                          filterByCmResults = FALSE)
eoi <- exposures[exposures$exposureId %in% c(targetId, comparatorId), ]
targetName <- eoi$exposureName[eoi$exposureId == targetId]
comparatorName <- eoi$exposureName[eoi$exposureId == comparatorId]
loadPs <- function(databaseId) {
    ps <- getPs(connection = connection,
                databaseId = databaseId,
                targetIds = targetId,
                comparatorIds = comparatorId)
    ps$databaseId <- databaseId
    return(ps)
}
ps <- lapply(databaseIds, loadPs)
ps <- do.call("rbind", ps)

# ps$preferenceScore[idx] <- 1 - ps$preferenceScore[idx]
ps <- rbind(data.frame(databaseId = ps$databaseId,
                       x = ps$preferenceScore,
                       y = ps$targetDensity,
                       group = targetName,
                       stringsAsFactors = FALSE),
            data.frame(databaseId = ps$databaseId,
                       x = ps$preferenceScore,
                       y = ps$comparatorDensity,
                       group = comparatorName,
                       stringsAsFactors = FALSE))


ps$group <- factor(ps$group, levels = c(targetName, comparatorName))
ps$databaseId[ps$databaseId == "Panther"] <- "PanTher"
plot <- ggplot2::ggplot(ps, ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) +
    ggplot2::geom_density(stat = "identity") +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Preference score", limits = c(0, 1), breaks = c(0, 0.2, 0.5, 0.75, 1), labels = c("0", "0.25", "0.5", "0.75", "1")) +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::facet_grid(~databaseId) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   # axis.title.x = ggplot2::element_blank(),
                   # axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   # axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   legend.position = "top")
fileName <- file.path("c:/temp/papers", "ps.png")
ggplot2::ggsave(filename = fileName, plot = plot, width = 8, height = 3, dpi = 300)


# Balance plots hydrochlorathiazide vs chlorthaladone --------------------------------------------------------------
databaseIds <-  c("CCAE", "Optum", "Panther")
targetId <- 974166
comparatorId <- 1395058
exposures <- getExposures(connection = connection,
                          filterByCmResults = FALSE)
eoi <- exposures[exposures$exposureId %in% c(targetId, comparatorId), ]
targetName <- eoi$exposureName[eoi$exposureId == targetId]
comparatorName <- eoi$exposureName[eoi$exposureId == comparatorId]
loadBal <- function(databaseId) {
    bal <- getCovariateBalance(connection = connection,
                               databaseId = databaseId,
                               targetId = targetId,
                               comparatorId = comparatorId,
                               analysisId = 2)
    bal$databaseId <- databaseId
    return(bal)
}
balance <- lapply(databaseIds, loadBal)
balance <- do.call("rbind", balance)
balance$databaseId[balance$databaseId == "Panther"] <- "PanTher"
limits <- c(min(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                na.rm = TRUE),
            max(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                na.rm = TRUE))
theme <- ggplot2::element_text(colour = "#000000", size = 12)
labels <- aggregate(covariateId ~ databaseId, balance, length)

labels$text <- sprintf("Number of covariates: %s", format(labels$covariateId, big.mark = ",", scientific = FALSE))
plot <- ggplot2::ggplot(balance, ggplot2::aes(x = absBeforeMatchingStdDiff, y = absAfterMatchingStdDiff)) +
    ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16, size = 1) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_label(x = limits[1] + 0.01,
                        y = limits[2],
                        hjust = "left",
                        vjust = "top",
                        alpha = 0.8,
                        ggplot2::aes(label = text),
                        data = labels,
                        size = 3.5) +
    ggplot2::scale_x_continuous("Before stratification", limits = limits) +
    ggplot2::scale_y_continuous("After stratification", limits = limits) +
    ggplot2::facet_grid(~databaseId) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank())
fileName <- file.path("c:/temp/papers", "bal.png")
ggplot2::ggsave(filename = fileName, plot = plot, width = 8, height = 3, dpi = 300)

length(unique(balance$covariateId))

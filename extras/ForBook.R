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
source("extras/LegendMedCentral/PlotsAndTables.R")
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

# Exemplar study ----------------------------------------------

folder <- "C:/Users/mschuemi/Git/TheBookOfOhdsi/images/PopulationLevelEstimation"

ps <- getPs(connection, targetIds = 1308216, comparatorIds = 1332418, databaseId = "CCAE")
plot <- plotPs(ps = ps, targetName = "Lisinopril", comparatorName = "Amlodipine")
ggplot2::ggsave(filename = file.path(folder, "psExample1.png"), plot = plot, width = 7, height = 3.5, dpi = 300)

ps <- getPs(connection, targetIds = 1308216, comparatorIds = 1318853, databaseId = "CCAE")
plot <- plotPs(ps = ps, targetName = "Lisinopril", comparatorName = "Nifedipine")
ggplot2::ggsave(filename = file.path(folder, "psExample2.png"), plot = plot, width = 7, height = 3.5, dpi = 300)


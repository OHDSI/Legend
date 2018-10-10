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

# Some functions for reducing the file sizes when running a large analysis
# Only use these if you know exactly what you're doing!

cmOutputFolder <- file.path(outputFolder, indicationId, "cmOutput")

# Drop any outcome-specific PS files -------------------------------------
files <- list.files(cmOutputFolder, "^Ps_.*_o[0-9]*.rds$", full.names = TRUE)
unlink(files)

# Drop any strata files ---------------------------------------------------
files <- list.files(cmOutputFolder, "^Strat.*.rds$", full.names = TRUE)
unlink(files)

# Minimize studyPop file sizes -----------------------------------------------
# Will perpetuate to PS and strata files

files <- list.files(cmOutputFolder, "StudyPop.*.rds", full.names = TRUE)

cluster <- ParallelLogger::makeCluster(8)
ParallelLogger::clusterApply(cluster, files, Legend:::minimizeFile)
ParallelLogger::stopCluster(cluster)


# Set studyPop files to size 0 -----------------------------------------------
files <- list.files(cmOutputFolder, "StudyPop.*.rds", full.names = TRUE)
cluster <- ParallelLogger::makeCluster(8)
ParallelLogger::clusterApply(cluster, files, Legend:::replaceWithZeroFile)
ParallelLogger::stopCluster(cluster)

# Fix outcome model file -------------------------------------------------
# Accidentally dropped attrition property. Bring it back
files <- list.files(cmOutputFolder, "StudyPop.*.rds", full.names = TRUE)
unlink(files)
# Manually run through runCohortMethod to recreate all studyPops
pathToRds <- file.path(cmOutputFolder, "outcomeModelReference1.rds")
outcomeModelReference <- readRDS(pathToRds)
outcomeModelReference <- outcomeModelReference[outcomeModelReference$studyPopFile != "", ]
copyMetaData <- function(i) {
  studyPop <- readRDS(file.path(cmOutputFolder, outcomeModelReference$studyPopFile[i]))
  metaData <- attr(studyPop, "metaData")
  om <- readRDS(file.path(cmOutputFolder, outcomeModelReference$outcomeModelFile[i]))
  om$attrition <- metaData$attrition
  saveRDS(om, file.path(cmOutputFolder, outcomeModelReference$outcomeModelFile[i]))
}
plyr::l_ply(1:nrow(outcomeModelReference), copyMetaData, .progress = "text")

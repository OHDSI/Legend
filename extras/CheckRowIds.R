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

# Use this code to check if row IDs have changed from one data fetch to the next
# Should be used for example when redoing data fetch but keeping old shared PS objecs.


folder1 <- file.path(outputFolder, indicationId, "allCohortsOld")
folder2 <- folder1 <- file.path(outputFolder, indicationId, "allCohorts")

file <- "allCohorts.rds"
files <- list.files(folder1)

for (file in files) {
  writeLines(file)
  cohort1 <- readRDS(file.path(folder1, file))
  cohort2 <- readRDS(file.path(folder2, file))
  if (!is.null(cohort1$cohortId)) {
    cohort1 <- cohort1[order(cohort1$rowId, cohort1$cohortId),]
    row.names(cohort1) <- NULL
    cohort2 <- cohort2[order(cohort2$rowId, cohort2$cohortId),]
    row.names(cohort2) <- NULL
  }
  if (!all.equal(cohort1, cohort2)) {
    warning("Not equal: ", file)
  }
}

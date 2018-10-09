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

# These commands were used when we changed the time-at-risk (TAR) to start on day 1 instead of day 0,
# To delete any old files that used the old TAR.

indicationFolder <- file.path(outputFolder, indicationId)
cmFolder <- file.path(indicationFolder, "cmOutput")

# Outcome models
files <- list.files(cmFolder, "^Analysis_.*", include.dirs = TRUE, full.names = TRUE)
head(files)
tail(files)

unlink(files, recursive = TRUE)

# Strata files
files <- list.files(cmFolder, "^Strat.*\\.rds$", full.names = TRUE)
head(files)
tail(files)

unlink(files)

# Outcome specific PS files
files <- list.files(cmFolder, "^Ps_.*_o[0-9]+\\.rds$", full.names = TRUE)
head(files)
tail(files)

unlink(files)

# Study population files
files <- list.files(cmFolder, "StudyPop.*.rds", full.names = TRUE)
head(files)
tail(files)

unlink(files)

# Balance folder
file <- file.path(indicationFolder, "balance")
file

unlink(file, recursive = TRUE)

# Export folder
file <- file.path(indicationFolder, "export")
file

unlink(file, recursive = TRUE)

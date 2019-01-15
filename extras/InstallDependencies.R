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

# Use this code to install the Legend package and all of its dependencies.

# Say yes to potential question about restarting R. Say no to question about installing from source:
install.packages(c("assertthat", "crayon", "bit", "cli", "devtools", "colorspace", "glue", "lattice", "magrittr", "R.methodsS3", "Rcpp", "rlang", "stringi", "utf8", "curl", "dichromat", "digest", "fastmatch", "ff", "jsonlite", "labeling", "Matrix", "mime", "munsell", "nlme", "openssl", "pillar", "plyr", "R.oo", "R6", "RColorBrewer", "rJava", "stringr", "triebeard", "viridisLite", "DBI", "ffbase", "git2r", "gtable", "httr", "lazyeval", "MASS", "memoise", "mgcv", "R.utils", "reshape2", "rstudioapi", "scales", "tibble", "urltools", "whisker", "withr", "codetools", "formatR", "ggplot2", "mailR", "RcppParallel", "RcppEigen", "RJSONIO", "snow", "survival", "XML", "gridExtra", "pROC", "openxlsx"))

# Run this if devtools complains that "Rtools 3.5 found on the path at D:/Rtools is not compatible with R 3.5.1."
# library(devtools)
# assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")
# find_rtools()

install.packages("Cyclops")
devtools::install_github("ohdsi/SqlRender", dep = FALSE)
devtools::install_github("ohdsi/DatabaseConnectorJars", dep = FALSE)
devtools::install_github("ohdsi/DatabaseConnector", dep = FALSE)
devtools::install_github("ohdsi/FeatureExtraction", ref = "v2.1.5", dep = FALSE)
devtools::install_github("ohdsi/ParallelLogger",ref = "develop",dep = FALSE)
devtools::install_github("ohdsi/CohortMethod", dep = FALSE)
devtools::install_github("ohdsi/EmpiricalCalibration", dep = FALSE)
devtools::install_github("ohdsi/IcTemporalPatternDiscovery", ref = "v1.1.1", dep = FALSE)
devtools::install_github("ohdsi/MethodEvaluation", dep = FALSE)
devtools::install_github("ohdsi/Legend", dep = FALSE)



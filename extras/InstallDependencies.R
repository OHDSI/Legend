# Say yes to potential question about restarting R. Say no to question about installing from source:
install.packages(c("assertthat", "crayon", "bit", "cli", "devtools", "colorspace", "glue", "lattice", "magrittr", "R.methodsS3", "Rcpp", "rlang", "stringi", "utf8", "curl", "dichromat", "digest", "fastmatch", "ff", "jsonlite", "labeling", "Matrix", "mime", "munsell", "nlme", "openssl", "pillar", "plyr", "R.oo", "R6", "RColorBrewer", "rJava", "stringr", "triebeard", "viridisLite", "DBI", "ffbase", "git2r", "gtable", "httr", "lazyeval", "MASS", "memoise", "mgcv", "R.utils", "reshape2", "rstudioapi", "scales", "tibble", "urltools", "whisker", "withr", "codetools", "formatR", "ggplot2", "mailR", "RcppParallel", "RcppEigen", "RJSONIO", "snow", "survival", "XML", "gridExtra", "pROC", "openxlsx"))

# Run this if devtools complains that "Rtools 3.5 found on the path at D:/Rtools is not compatible with R 3.5.1."
# library(devtools)
# assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")
# find_rtools()

devtools::install_github("r-lib/zip", dep = FALSE)
devtools::install_github("ohdsi/SqlRender", dep = FALSE)
devtools::install_github("ohdsi/DatabaseConnectorJars", dep = FALSE)
devtools::install_github("ohdsi/DatabaseConnector", dep = FALSE)
devtools::install_github("ohdsi/Cyclops", dep = FALSE)
devtools::install_github("ohdsi/FeatureExtraction", dep = FALSE)
devtools::install_github("ohdsi/OhdsiRTools", dep = FALSE)
devtools::install_github("ohdsi/ParallelLogger", dep = FALSE)
devtools::install_github("ohdsi/CohortMethod", ref = "develop", dep = FALSE)
devtools::install_github("ohdsi/EmpiricalCalibration", dep = FALSE)
devtools::install_github("ohdsi/IcTemporalPatternDiscovery", dep = FALSE)
devtools::install_github("ohdsi/MethodEvaluation", ref = "develop", dep = FALSE)
devtools::install_github("ohdsi/Legend", dep = FALSE)



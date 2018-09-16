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

# 76.8GB free before
# 419GB free after

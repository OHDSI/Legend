# Use this script to share a run between two machines. Can only do this once all data is fetched and
# positive control synthesis is complete.

# Remote machine: where the analysis is already running. Make sure the analysis is stopped.
# Local machine: the extra machine to take on the load

remoteFolder <- "\\\\wprdusmjtglay/Legend/ccae/Hypertension"
localFolder <- "s:/legend/ccae/Hypertension"

if (!file.exists(localFolder)) {
    dir.create(localFolder, recursive = TRUE)
}

# Split exposure summary file --------------------------------------------------------------
exposureSummary <- read.csv(file.path(remoteFolder, "pairedExposureSummaryFilteredBySize.csv"), stringsAsFactors = FALSE)
# Save a copy
write.csv(exposureSummary, file.path(remoteFolder, "pairedExposureSummaryFilteredBySize_Full.csv"), row.names = FALSE)
set.seed(123)
idx <- sample.int(nrow(exposureSummary), nrow(exposureSummary)/2)
remoteExposureSummary <- exposureSummary[idx, ]
localExposureSummary <- exposureSummary[-idx, ]
write.csv(remoteExposureSummary, file.path(remoteFolder, "pairedExposureSummaryFilteredBySize_part1.csv"), row.names = FALSE)
write.csv(localExposureSummary, file.path(remoteFolder, "pairedExposureSummaryFilteredBySize_part2.csv"), row.names = FALSE)
write.csv(localExposureSummary, file.path(localFolder, "pairedExposureSummaryFilteredBySize.csv"), row.names = FALSE)
# DANGER:
# write.csv(remoteExposureSummary, file.path(remoteFolder, "pairedExposureSummaryFilteredBySize.csv"), row.names = FALSE)

# Copy files from remote to local ------------------------------------------------------------
remoteCmOutput <- file.path(remoteFolder, "cmOutput")
localCmOutput <- file.path(localFolder, "cmOutput")
localExposureSummary <- read.csv(file.path(localFolder, "pairedExposureSummaryFilteredBySize.csv"))
if (!file.exists(localCmOutput)) {
    dir.create(localCmOutput, recursive = TRUE)
}

copyFilesForTc <- function(i) {
  pattern <- paste0("_t", localExposureSummary$targetId[i], "_c", localExposureSummary$comparatorId[i], "([^0-9]|$)")
  files <- list.files(remoteCmOutput, pattern, include.dirs = TRUE)
  file.copy(file.path(remoteCmOutput, files), localCmOutput, recursive = TRUE)
  return(NULL)
}

plyr::l_ply(1:nrow(localExposureSummary), copyFilesForTc)

# Todo: add covariate balance data


# Copy finished files back from local to remove -----------------------------------------------



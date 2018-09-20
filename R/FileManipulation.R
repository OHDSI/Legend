# Functions to use with extras/MinimizeFileSizes.R
# Compiled with package for efficiency

minimizeFile <- function(file) {
    sp <- readRDS(file)
    sp <- sp[, c("rowId", "treatment", "subjectId", "outcomeCount", "timeAtRisk", "survivalTime")]
    saveRDS(sp, file)
}

replaceWithZeroFile <- function(file) {
    saveRDS(c(), file)
}

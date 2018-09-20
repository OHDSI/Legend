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

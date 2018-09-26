indicationFolder <- file.path(outputFolder, indicationId)
cmFolder <- file.path(indicationFolder, "cmOutput")

# Outcome models
files <- list.files(cmFolder, "^Analysis_.*", include.dirs = TRUE, full.names = TRUE)
head(files)
tail(files)

unlink(files, recursive = TRUE)

# Strata files
files <- list.files(cmFolder, "^Strata_.*\\.rds$", full.names = TRUE)
head(files)
tail(files)

unlink(files)

# Outcome specific PS files
files <- list.files(cmFolder, "^Ps_.*_o[0-9+\\.rds$", full.names = TRUE)
head(files)
tail(files)

unlink(files)

# Study population files
files <- list.files(cmFolder, "^StudyPop\\.rds$", full.names = TRUE)
head(files)
tail(files)

unlink(files)

# Balance folder
file <- file.path(indicationFolder, "balance")
file

unlink(file)

# Export folder
file <- file.path(indicationFolder, "export")
file

unlink(file)

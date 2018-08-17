mainFolder <- "C:/Legend/phenotypeAssessment"

zipFiles <- list.files(mainFolder, "^PhenotypeAssessment.*\\.zip$")

allExposures <- data.frame()
allOutcomes <- data.frame()
allSubgroups <- data.frame()
for (zipFile in zipFiles) {
    unzip(zipfile = file.path(mainFolder, zipFile),
          exdir = file.path(mainFolder))
    outcomes <- read.csv(file.path(mainFolder, "outcomes.csv"), stringsAsFactors = FALSE)
    allOutcomes <- rbind(allOutcomes, outcomes)
    exposures <- read.csv(file.path(mainFolder, "exposures.csv"), stringsAsFactors = FALSE)
    allExposures <- rbind(allExposures, exposures)
    subgroups <- read.csv(file.path(mainFolder, "subgroups.csv"), stringsAsFactors = FALSE)
    allSubgroups <- rbind(allSubgroups, subgroups)
}
pathToCsv <- system.file("settings", "OutcomesOfInterest.csv", package = "Legend")
outcomesOfInterest <- read.csv(pathToCsv)

library(ggplot2)
plotCounts <- function(subset, fileName) {
    plot <- ggplot(subset, aes(y = count, x = name)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        facet_grid(~databaseId, scales = "free") +
        theme(axis.title.y = element_blank())
    ggsave(fileName, plot, dpi = 300)
}

allOutcomes$name <- allOutcomes$cohortName
allExposures$count <- allExposures$exposureCount
allSubgroups$name <- allSubgroups$covariateName
allSubgroups$count <- allSubgroups$fraction

allOutcomes$count <- as.numeric(allOutcomes$count)
allExposures$count <- as.numeric(allExposures$count)
allSubgroups$count <- as.numeric(allSubgroups$count)

hois <- allOutcomes[allOutcomes$cohortDefinitionId %in% outcomesOfInterest$cohortId, ]
drugs <- allExposures[allExposures$type == "Drug" | allExposures$type == "Procedure", ]
drugClasses <- allExposures[allExposures$type == "Drug class", ]
for (indicationId in unique(outcomes$indicationId)) {
    plotCounts(subset = hois[hois$indicationId == indicationId, ],
               fileName = file.path(mainFolder, sprintf("Hois%s.png", indicationId)))
    plotCounts(subset = drugs[drugs$indicationId == indicationId, ],
               fileName = file.path(mainFolder, sprintf("Drugs%s.png", indicationId)))
    plotCounts(subset = drugClasses[drugClasses$indicationId == indicationId, ],
               fileName = file.path(mainFolder, sprintf("DrugClasses%s.png", indicationId)))
    plotCounts(subset = allSubgroups[allSubgroups$indicationId == indicationId, ],
               fileName = file.path(mainFolder, sprintf("Subgroups%s.png", indicationId)))
}

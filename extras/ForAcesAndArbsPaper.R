library(tidyverse)
source("extras/LegendMedCentral/DataPulls.R")

aceId <- 1
arbId <- 4
mainPaperAnalysisId <- 1 # on-treatment, PS-stratified

connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = paste(Sys.getenv("legendServer"),
                                                            Sys.getenv("legendDatabase"), sep = "/"),
                                             port = Sys.getenv("legendPort"),
                                             user = Sys.getenv("legendUser"),
                                             password = Sys.getenv("legendPw"),
                                             schema = Sys.getenv("legendSchema"))

connection <- connect(connectionDetails)

mainResultsOT <- getMainResults(connection,
                                targetIds = aceId,
                                comparatorIds = arbId,
                                outcomeIds = NULL,
                                databaseIds = NULL,
                                analysisIds = mainPaperAnalysisId)

sql <- "SELECT * FROM positive_control_outcome WHERE exposure_id = @target_id;"
pcs <- renderTranslateQuerySql(connection, sql, target_id = aceId, snakeCaseToCamelCase = TRUE)

sql <- "SELECT * FROM negative_control_outcome;"
ncs <- renderTranslateQuerySql(connection, sql, snakeCaseToCamelCase = TRUE)

DatabaseConnector::disconnect(connection)

mainResultsOT <- mainResultsOT %>% filter(databaseId != "Meta-analysis")

# mainResultsOT <- mainResultsOT %>% filter(databaseId != “NHIS_NSC”) # Uncomment to remove Korea

# Add true effect sizes of positive and negative controls:
mainResultsOT <- merge(mainResultsOT, data.frame(outcomeId = pcs$outcomeId,
                                                 trueEffectSize = pcs$effectSize), all.x = TRUE)
mainResultsOT$trueEffectSize[mainResultsOT$outcomeId %in% ncs$outcomeId] <- 1

results <- Legend:::computeGroupMetaAnalysis(group = mainResultsOT, interactions = FALSE)

results # for example


# Exploring counts in IMSG

library(DatabaseConnector)
source("extras/LegendMedCentral/DataPulls.R")
ami <- 2
connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = paste(Sys.getenv("legendServer"),
                                                            Sys.getenv("legendDatabase"), sep = "/"),
                                             port = Sys.getenv("legendPort"),
                                             user = Sys.getenv("legendUser"),
                                             password = Sys.getenv("legendPw"),
                                             schema = Sys.getenv("legendSchema"))
connection <- connect(connectionDetails)

coughAttrition <- getAttrition(connection = connection,
                               targetId = aceId,
                               comparatorId = arbId,
                               outcomeId = 41,
                               analysisId = mainPaperAnalysisId,
                               databaseId = "IMSG")

coughAttritionCcae <- getAttrition(connection = connection,
                               targetId = aceId,
                               comparatorId = arbId,
                               outcomeId = 41,
                               analysisId = mainPaperAnalysisId,
                               databaseId = "CCAE")

amiAttrition <- getAttrition(connection = connection,
                             targetId = aceId,
                             comparatorId = arbId,
                             outcomeId = ami,
                             analysisId = mainPaperAnalysisId,
                             databaseId = "IMSG")


amiAttritionCcae <- getAttrition(connection = connection,
                             targetId = aceId,
                             comparatorId = arbId,
                             outcomeId = ami,
                             analysisId = mainPaperAnalysisId,
                             databaseId = "CCAE")

getMainResults(connection = connection,
               targetId = aceId,
               comparatorId = arbId,
               outcomeId = 2,
               analysisId = mainPaperAnalysisId,
               databaseId = "IMSG")

sql <- "SELECT * FROM incidence WHERE outcome_id = 2 AND database_id = 'CCAE';"
x <- DatabaseConnector::querySql(connection, sql)


DatabaseConnector::disconnect(connection)

coughAttrition

amiAttrition


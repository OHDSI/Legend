library(shiny)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel("LEGEND basic viewer"),
            tabsetPanel(id = "mainTabsetPanel",
                        tabPanel("Overview",
                                 tabsetPanel(id = "overviewTabsetPanel",
                                             tabPanel("Main effects",
                                                      fluidRow(
                                                        column(3,
                                                               selectInput("meIndication", "Indication", c("All", indications$indicationId)),
                                                               selectInput("meExposureGroup", "Exposure group", c("All", unique(exposureGroups$exposureGroup))),
                                                               selectInput("meTarget", "Target", c("All", unique(exposures$exposureName))),
                                                               selectInput("meComparator", "Comparator", c("All", unique(exposures$exposureName))),
                                                               selectInput("meOutcome", "Outcome", c("All", unique(outcomes$outcomeName))),
                                                               selectInput("meDatabase", "Database", c("All", databases$databaseId)),
                                                               selectInput("meAnalysis", "Analysis", c("All", analyses$description[analyses$analysisId <= 4]))
                                                        ),
                                                        column(9,
                                                               plotOutput("mePlot")
                                                        )
                                                      )
                                             ),
                                             tabPanel("Interaction effects",
                                                      column(3,
                                                             selectInput("ieIndication", "Indication", c("All", indications$indicationId)),
                                                             selectInput("ieExposureGroup", "Exposure group", c("All", unique(exposureGroups$exposureGroup))),
                                                             selectInput("ieTarget", "Target", c("All", unique(exposures$exposureName))),
                                                             selectInput("ieComparator", "Comparator", c("All", unique(exposures$exposureName))),
                                                             selectInput("ieOutcome", "Outcome", c("All", unique(outcomes$outcomeName))),
                                                             selectInput("ieDatabase", "Database", c("All", databases$databaseId)),
                                                             selectInput("ieAnalysis", "Analysis", c("All", analyses$description[analyses$analysisId <= 4]))
                                                      ),
                                                      column(9,
                                                             plotOutput("iePlot")
                                                      )
                                             ),
                                             tabPanel("Propensity score distributions",
                                                      fluidRow(
                                                        column(3,
                                                               selectInput("psIndication", "Indication", indications$indicationId),
                                                               selectInput("psExposureGroup", "Exposure group", unique(exposureGroups$exposureGroup)),
                                                               selectInput("psTarget", "Target", c("All", unique(exposures$exposureName))),
                                                               selectInput("psComparator", "Comparator", c("All", unique(exposures$exposureName))),
                                                               selectInput("psDatabase", "Database", databases$databaseId)
                                                        ),
                                                        column(9,
                                                               plotOutput("psPlot")
                                                        )
                                                      )
                                                      
                                             )
                                 )
                        ),
                        tabPanel("Specific research questions")
            )
  )
)

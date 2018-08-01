library(shiny)
library(DT)
source("widgets.R")

shinyUI(fluidPage(style = 'width:1000px;',
                  titlePanel("PubLegend Central"),
                  verticalLayout(
                    div(style = "display:inline-block", textInput("query", label = "", placeholder = "Enter your search here")), 
                    div(style = "display:inline-block", searchButton("searchButton", "Search", "query")),
                    conditionalPanel("output.isSearchResultPage == true",
                                     dataTableOutput("searchResults")),
                    conditionalPanel("output.isAbstractPage == true",
                                     uiOutput("abstract"),
                                     downloadLink("pdf", label = "PDF")
                    )
                  )
)
)


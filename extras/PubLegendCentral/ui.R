library(shiny)
library(DT)

shinyUI(fluidPage(style = 'width:1000px;',
  titlePanel("PubLegend Central"),
  verticalLayout(
      textInput("search", label = "Search", placeholder = "Enter your search here"),
      dataTableOutput("searchResults"),
      conditionalPanel("output.rowIsSelected == true",
                       uiOutput("abstract"),
                       downloadLink("pdf", label = "PDF")
      )
  )
)
)


library(shiny)
library(ggplot2)
library(DT)

shinyServer(function(input, output) {
  
  selectedTcoDbs <- reactive({
    if (nchar(input$search) < 3) {
      return(NULL)
    } else{
      parts <- strsplit(input$search, " ")[[1]]
      outcomeIds <- c()
      exposureIds <- c()
      for (part in parts) {
        outcomeIds <- c(outcomeIds, 
                        outcomes$outcomeId[agrepl(part, 
                                                  outcomes$outcomeName,
                                                  max.distance = 0.1)])
        exposureIds <- c(exposureIds, 
                         exposures$exposureId[agrepl(part, 
                                                     exposures$exposureName,
                                                     max.distance = 0.1)])
        
      }
      tcoDbs <- getTcoDbs(connection, 
                          targetIds = exposureIds,
                          comparatorIds = exposureIds,
                          outcomeIds = outcomeIds)
      return(tcoDbs)
    }
  })
  
  selectedTcoDb <- reactive({
    idx <- input$searchResults_rows_selected
    if (is.null(idx)) {
      return(NULL)
    } else {
      return(selectedTcoDbs()[idx, ])  
    }
  })
  
  output$rowIsSelected <- reactive({
    return(!is.null(selectedTcoDb()))
  })
  outputOptions(output, "rowIsSelected", suspendWhenHidden = FALSE)
  
  
  output$searchResults <- renderDataTable({
    tcoDbs <- selectedTcoDbs()
    if (is.null(tcoDbs)) {
      return(NULL)
    } else {
      titles <- createTitle(tcoDbs)
      options = list(pageLength = 15,
                     searching = FALSE,
                     lengthChange = TRUE,
                     ordering = TRUE,
                     paging = TRUE)
      selection = list(mode = "single", target = "row")
      table <- datatable(data.frame(Title = titles),
                         options = options,
                         selection = selection,
                         rownames = FALSE,
                         escape = FALSE,
                         class = "stripe nowrap compact")
      return(table)
    }
  })
  
  output$abstract <- renderUI({
    tcoDb <- selectedTcoDb()
    if (is.null(tcoDb)) {
      return(NULL)
    } else {
      results <- getMainResults(connection, 
                                targetIds = tcoDb$targetId, 
                                comparatorIds = tcoDb$comparatorId,
                                outcomeIds = tcoDb$outcomeId,
                                databaseIds = tcoDb$databaseId)
      mainResult <- results[results$analysisId == 1, ]
      hr <- sprintf("%.2f (%.2f - %.2f).", mainResult$rr, mainResult$ci95lb, mainResult$ci95ub)
      
      title <- createTitle(tcoDb)
      abstract <- div(h1(title),
                      h2("Abstract"),
                      p("This is a really cool paper"),
                      p(paste("We observed a hazard ratio of", hr)))
      return(abstract)
    }
  })
  
  output$pdf <- downloadHandler(
    filename = function() {
      return("Paper.pdf")
    },
    content = function(con) {
      tcoDb <- selectedTcoDb()
      title <- createTitle(tcoDb)
      tempFileName <- paste0(paste(sample(letters, 8), collapse = ""), ".pdf")
      withProgress(message = 'Generating PDF', value = 0, {
        rmarkdown::render("dbPaper.rmd",
                          output_file = tempFileName,
                          params = list(setTitle = title,
                                        targetId = tcoDb$targetId,
                                        comparatorId = tcoDb$comparatorId,
                                        outcomeId = tcoDb$outcomeId,
                                        databaseId = tcoDb$databaseId),
                          rmarkdown::pdf_document(latex_engine = "pdflatex"))
      })
      file.rename(tempFileName, con)
    }
  )
  
})







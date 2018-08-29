library(shiny)
library(ggplot2)
library(DT)

shinyServer(function(input, output, session) {
  
  searchResults <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    if (is.null(query$term)) {
      return(NULL)
    } else {
      parts <- strsplit(query$term, " ")[[1]]
      outcomeIds <- c()
      exposureIds <- c()
      for (part in parts) {
        outcomeDist <- adist(part, outcomes$outcomeName)
        exposureDist <- adist(part, exposures$exposureName)
        if (min(outcomeDist) < min(exposureDist)) {
          outcomeIds <- c(outcomeIds, outcomes$outcomeId[outcomeDist == min(outcomeDist)])
        } else {
          exposureIds <- c(exposureIds, exposures$exposureId[exposureDist == min(exposureDist)])
        }
      }
      tcoDbs <- getTcoDbsStrict(connection, 
                                exposureIds = exposureIds,
                                outcomeIds = outcomeIds)
      return(tcoDbs)
    }
  })
  
  selectedTcoDb <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    if (is.null(query$targetId)) {
      return(NULL)
    } else {
      tcoDb <- getTcoDbs(connection, 
                         targetIds = query$targetId,
                         comparatorIds = query$comparatorId,
                         outcomeIds = query$outcomeId)  
      return(tcoDb)
    }
  })

  # Maintain contents of search box:
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$term))
      updateTextInput(session, "query",
                      value = query$term)
  })
  
  output$isSearchPage <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    return(is.null(query$targetId) && is.null(query$term))
  })
  outputOptions(output, "isSearchPage", suspendWhenHidden = FALSE)

  output$isSearchResultPage <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    return(is.null(query$targetId) && !is.null(query$term))
  })
  outputOptions(output, "isSearchResultPage", suspendWhenHidden = FALSE)
  
  output$isAbstractPage <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    return(!is.null(query$targetId))
  })
  outputOptions(output, "isAbstractPage", suspendWhenHidden = FALSE)
  
  
  output$searchResults <- renderDataTable({
    tcoDbs <- searchResults()
    if (is.null(tcoDbs)) {
      return(NULL)
    } else {
      titles <- createTitle(tcoDbs)
      titles <- paste0("<a href = '?targetId=", 
                       tcoDbs$targetId, 
                       "&comparatorId=",
                       tcoDbs$comparatorId,
                       "&outcomeId=",
                       tcoDbs$outcomeId,
                       "&term=",
                       URLencode(input$query),
                       "'>",
                       titles, 
                       "</a></br><i>Annals of OHDSI</i>, October, 2018</br>") 
      options = list(pageLength = 15,
                     searching = FALSE,
                     lengthChange = TRUE,
                     paging = TRUE,
                     dom = '<"top"ip>rt<"bottom"flp><"clear">')
      data <- data.frame(title = titles)
      colnames(data) <- "Search results"
      table <- datatable(data,
                         options = options,
                         rownames = TRUE,
                         escape = FALSE,
                         class = "compact")
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
      abstract <- div(em("Annals of OHDSI"),
                      h2(title),
                      h3("Abstract"),
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

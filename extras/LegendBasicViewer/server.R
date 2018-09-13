library(shiny)


shinyServer(function(input, output, session) {

  # Main effects tab ---------------------------------------------------------------------------
  observe({
    indicationId <- input$meIndication
    if (indicationId == "All") {
      updateSelectInput(session = session, 
                        inputId = "meExposureGroup",
                        choices = c("All", unique(exposureGroups$exposureGroup)))
    } else {
      updateSelectInput(session = session, 
                        inputId = "meExposureGroup",
                        choices = c("All", unique(exposureGroups$exposureGroup[exposureGroups$indicationId == indicationId])))
    }
  })
  
  observe({
    indicationId <- input$meIndication
    exposureGroup <- input$meExposureGroup
    if (indicationId == "All") {
      filteredExposures <- exposures
      filteredOutcomes <- outcomes
    } else {
      filteredExposures <- exposures[exposures$indicationId == indicationId, ]
      filteredOutcomes <- outcomes[outcomes$indicationId == indicationId, ]
    }
    if (exposureGroup == "All") {
      filteredExposures <- filteredExposures
    } else {
      filteredExposures <- filteredExposures[filteredExposures$exposureGroup == exposureGroup, ]
    } 
    updateSelectInput(session = session, 
                      inputId = "meTarget",
                      choices = c("All", unique(filteredExposures$exposureName)))
    updateSelectInput(session = session, 
                      inputId = "meComparator",
                      choices = c("All", unique(filteredExposures$exposureName)))
    updateSelectInput(session = session, 
                      inputId = "meOutcome",
                      choices = c("All", unique(filteredOutcomes$outcomeName)))
  })
  
  output$mePlot <- renderPlot({
    indicationId <- input$meIndication
    exposureGroup <- input$meExposureGroup
    if (indicationId == "All") {
      filteredExposures <- exposures
      filteredOutcomes <- outcomes
    } else {
      filteredExposures <- exposures[exposures$indicationId == indicationId, ]
      filteredOutcomes <- outcomes[outcomes$indicationId == indicationId, ]
    }
    if (exposureGroup == "All") {
      filteredExposures <- filteredExposures
    } else {
      filteredExposures <- filteredExposures[filteredExposures$exposureGroup == exposureGroup, ]
    } 
    target <- input$meTarget
    if (target == "All") {
      targetIds <- unique(filteredExposures$exposureId)
    } else {
      targetIds <- unique(filteredExposures$exposureId[filteredExposures$exposureName == target])
    }
    comparator <- input$meComparator
    if (comparator == "All") {
      comparatorIds <- unique(filteredExposures$exposureId)
    } else {
      comparatorIds <- unique(filteredExposures$exposureId[filteredExposures$exposureName == comparator])
    }
    outcome <- input$meComparator
    if (outcome == "All") {
      outcomeIds <- unique(filteredOutcomes$outcomeId)
    } else {
      outcomeIds <- unique(filteredOutcomes$outcomeId[filteredOutcomes$outcomeName == outcome])
    }
    database <- input$meDatabase
    if (database == "All") {
      databaseIds <- databases$databaseId   
    } else {
      databaseIds <- database
    }
    analysis <- input$meAnalysis
    if (analysis == "All") {
      analysisIds <- 1:4
    } else {
      analysisIds <- analyses$analysisId[analyses$description == analysis]
    }
    writeLines("Fetching main effects")
    mainEffects <- getMainResults(connection = connection,
                                  targetIds = targetIds,
                                  comparatorIds = comparatorIds,
                                  outcomeIds = outcomeIds,
                                  databaseIds = databaseIds,
                                  analysisIds = analysisIds,
                                  estimatesOnly = TRUE)
    mainEffects <- data.frame(logRr = mainEffects$calibratedLogRr,
                              seLogRr = mainEffects$calibratedSeLogRr,
                              ci95lb = mainEffects$calibratedCi95Lb,
                              ci95ub = mainEffects$calibratedCi95Ub)
    writeLines("Plotting main effects")
    plot <- plotLargeScatter(mainEffects, "Calibrated hazard ratio")
    return(plot)
  }, width = 1000 , height = 500)
  
  # Propensity scores tab ---------------------------------------------------------------------------
  observe({
    indicationId <- input$psIndication
    updateSelectInput(session = session, 
                        inputId = "psExposureGroup",
                        choices = unique(exposureGroups$exposureGroup[exposureGroups$indicationId == indicationId]))
  })
  
  observe({
    indicationId <- input$psIndication
    exposureGroup <- input$psExposureGroup
    filteredExposures <- exposures[exposures$indicationId == indicationId, ]
    filteredOutcomes <- outcomes[outcomes$indicationId == indicationId, ]
    filteredExposures <- filteredExposures[filteredExposures$exposureGroup == exposureGroup, ]
    updateSelectInput(session = session, 
                      inputId = "psTarget",
                      choices = c("All", unique(filteredExposures$exposureName)))
    updateSelectInput(session = session, 
                      inputId = "psComparator",
                      choices = c("All", unique(filteredExposures$exposureName)))
  })
  
  output$psPlot <- renderPlot({
    indicationId <- input$psIndication
    exposureGroup <- input$psExposureGroup
    filteredExposures <- exposures[exposures$indicationId == indicationId, ]
    filteredExposures <- filteredExposures[filteredExposures$exposureGroup == exposureGroup, ]
    target <- input$psTarget
    if (target == "All") {
      targetIds <- unique(filteredExposures$exposureId)
    } else {
      targetIds <- unique(filteredExposures$exposureId[filteredExposures$exposureName == target])
    }
    comparator <- input$psComparator
    if (comparator == "All") {
      comparatorIds <- unique(filteredExposures$exposureId)
    } else {
      comparatorIds <- unique(filteredExposures$exposureId[filteredExposures$exposureName == comparator])
    }
    databaseId <- input$psDatabase
    writeLines("Fetching PS distributions")
    ps <- getPs(connection = connection,
                targetIds = targetIds,
                comparatorIds = comparatorIds,
                databaseId = databaseId)
    if (nrow(ps) == 0) {
      return(NULL)
    }
    ps <- merge(ps, data.frame(targetId = exposures$exposureId,
                               targetName = exposures$exposureName))
    ps <- merge(ps, data.frame(comparatorId = exposures$exposureId,
                               comparatorName = exposures$exposureName))
    writeLines("Plotting PS distributions")
    plot <- plotAllPs(ps)
    return(plot)
  }, width = 1000 , height = 600)
})

onStop(function() {
  writeLines("Closing connection")
  DatabaseConnector::disconnect(connection)
})

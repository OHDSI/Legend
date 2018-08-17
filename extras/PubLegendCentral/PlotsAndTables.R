createTitle <- function(tcoDbs) {
  tcoDbs$targetName <- exposures$exposureName[match(tcoDbs$targetId, exposures$exposureId)]
  tcoDbs$comparatorName <- exposures$exposureName[match(tcoDbs$comparatorId, exposures$exposureId)]
  tcoDbs$outcomeName <- outcomes$outcomeName[match(tcoDbs$outcomeId, outcomes$outcomeId)]
  
  titles <- paste("A Comparison of",
                  tcoDbs$targetName,
                  "to",
                  tcoDbs$comparatorName,
                  "for the Risk of",
                  tcoDbs$outcomeName,
                  "in the",
                  tcoDbs$databaseId,"Database.")
  return(titles)
}

prepareTable1 <- function(balance, 
                          beforeLabel = "Before stratification",
                          afterLabel = "After stratification",
                          targetLabel = "Target",
                          comparatorLabel = "Comparator",
                          percentDigits = 1,
                          stdDiffDigits = 2,
                          output = "latex") {
  #pathToCsv <- system.file("settings", "Table1Specs.csv", package = packageName)
  if (output == "latex") {
    space <- " "
  } else {
    space <- "&nbsp;"
  }
  pathToCsv <- "Table1Specs.csv"
  specifications <- read.csv(pathToCsv, stringsAsFactors = FALSE)
  
  fixCase <- function(label) {
    idx <- (toupper(label) == label)
    if (any(idx)) {
      label[idx] <- paste0(substr(label[idx], 1, 1),
                           tolower(substr(label[idx], 2, nchar(label[idx]))))
    }
    return(label)
  }
  
  formatPercent <- function(x) {
    result <- format(round(100*x, percentDigits), digits = percentDigits+1, justify = "right")
    # result <- formatC(x * 100, digits = percentDigits, format = "f")
    result <- gsub("NA", "", result)
    result <- gsub(" ", space, result)
    return(result)
  }
  
  formatStdDiff <- function(x) {
    result <- format(round(x, stdDiffDigits), digits = stdDiffDigits+1, justify = "right")
    result <- gsub("NA", "", result)
    result <- gsub(" ", space, result)
    return(result)
  }
  
  resultsTable <- data.frame()
  for (i in 1:nrow(specifications)) {
    if (specifications$analysisId[i] == "") {
      resultsTable <- rbind(resultsTable,
                            data.frame(Characteristic = specifications$label[i], value = ""))
    } else {
      idx <- balance$analysisId == specifications$analysisId[i]
      if (any(idx)) {
        if (specifications$covariateIds[i] != "") {
          covariateIds <- as.numeric(strsplit(specifications$covariateIds[i], ";")[[1]])
          idx <- balance$covariateId %in% covariateIds
        } else {
          covariateIds <- NULL
        }
        if (any(idx)) {
          balanceSubset <- balance[idx, ]
          if (is.null(covariateIds)) {
            balanceSubset <- balanceSubset[order(balanceSubset$covariateId), ]
          } else {
            balanceSubset <- merge(balanceSubset, data.frame(covariateId = covariateIds,
                                                             rn = 1:length(covariateIds)))
            balanceSubset <- balanceSubset[order(balanceSubset$rn,
                                                 balanceSubset$covariateId), ]
          }
          balanceSubset$covariateName <- fixCase(gsub("^.*: ",
                                                      "",
                                                      balanceSubset$covariateName))
          if (specifications$covariateIds[i] == "" || length(covariateIds) > 1) {
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                           beforeMatchingMeanTreated = NA,
                                                           beforeMatchingMeanComparator = NA,
                                                           beforeMatchingStdDiff = NA,
                                                           afterMatchingMeanTreated = NA,
                                                           afterMatchingMeanComparator = NA,
                                                           afterMatchingStdDiff = NA,
                                                           stringsAsFactors = FALSE))
            resultsTable <- rbind(resultsTable,
                                  data.frame(Characteristic = paste0(space, space, space, space, balanceSubset$covariateName),
                                             beforeMatchingMeanTreated = balanceSubset$beforeMatchingMeanTreated,
                                             beforeMatchingMeanComparator = balanceSubset$beforeMatchingMeanComparator,
                                             beforeMatchingStdDiff = balanceSubset$beforeMatchingStdDiff,
                                             afterMatchingMeanTreated = balanceSubset$afterMatchingMeanTreated,
                                             afterMatchingMeanComparator = balanceSubset$afterMatchingMeanComparator,
                                             afterMatchingStdDiff = balanceSubset$afterMatchingStdDiff,
                                             stringsAsFactors = FALSE))
          } else {
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                           beforeMatchingMeanTreated = balanceSubset$beforeMatchingMeanTreated,
                                                           beforeMatchingMeanComparator = balanceSubset$beforeMatchingMeanComparator,
                                                           beforeMatchingStdDiff = balanceSubset$beforeMatchingStdDiff,
                                                           afterMatchingMeanTreated = balanceSubset$afterMatchingMeanTreated,
                                                           afterMatchingMeanComparator = balanceSubset$afterMatchingMeanComparator,
                                                           afterMatchingStdDiff = balanceSubset$afterMatchingStdDiff,
                                                           stringsAsFactors = FALSE))
          }
        }
      }
    }
  }
  resultsTable$beforeMatchingMeanTreated <- formatPercent(resultsTable$beforeMatchingMeanTreated) 
  resultsTable$beforeMatchingMeanComparator <- formatPercent(resultsTable$beforeMatchingMeanComparator) 
  resultsTable$beforeMatchingStdDiff <- formatStdDiff(resultsTable$beforeMatchingStdDiff) 
  resultsTable$afterMatchingMeanTreated <- formatPercent(resultsTable$afterMatchingMeanTreated) 
  resultsTable$afterMatchingMeanComparator <- formatPercent(resultsTable$afterMatchingMeanComparator) 
  resultsTable$afterMatchingStdDiff <- formatStdDiff(resultsTable$afterMatchingStdDiff) 
  
  headerRow <- as.data.frame(t(rep("", ncol(resultsTable))))
  colnames(headerRow) <- colnames(resultsTable)
  headerRow$beforeMatchingMeanTreated <- targetLabel
  headerRow$beforeMatchingMeanComparator <- comparatorLabel
  headerRow$afterMatchingMeanTreated <- targetLabel
  headerRow$afterMatchingMeanComparator <- comparatorLabel
  
  subHeaderRow <- as.data.frame(t(rep("", ncol(resultsTable))))
  colnames(subHeaderRow) <- colnames(resultsTable)
  subHeaderRow$Characteristic <- "Characteristic"
  subHeaderRow$beforeMatchingMeanTreated <- "%"
  subHeaderRow$beforeMatchingMeanComparator <- "%"
  subHeaderRow$beforeMatchingStdDiff <- "Std. diff"
  subHeaderRow$afterMatchingMeanTreated <- "%"
  subHeaderRow$afterMatchingMeanComparator <- "%"
  subHeaderRow$afterMatchingStdDiff <- "Std. diff"
  
  resultsTable <- rbind(headerRow, subHeaderRow, resultsTable)
  
  colnames(resultsTable) <- rep("", ncol(resultsTable))
  colnames(resultsTable)[2] <- beforeLabel
  colnames(resultsTable)[5] <- afterLabel
  return(resultsTable)
}

plotPs <- function(ps, targetName, comparatorName) {
  ps <- rbind(data.frame(x = ps$preferenceScore,
                         y = ps$targetDensity,
                         group = targetName),
              data.frame(x = ps$preferenceScore,
                         y = ps$comparatorDensity,
                         group = comparatorName))
  ps$group <- factor(ps$group, levels = c(as.character(targetName), as.character(comparatorName)))
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  plot <- ggplot2::ggplot(ps, ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) +
    ggplot2::geom_density(stat = "identity") +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5), rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Preference score", limits = c(0, 1)) +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::theme(legend.title = ggplot2::element_blank(), 
                   panel.grid.major = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank(), 
                   legend.position = "top",
                   legend.text = theme,
                   axis.text = theme,
                   axis.title = theme)
  return(plot)
}

plotKaplanMeier <- function(kaplanMeier, targetName, comparatorName) {
  data <- rbind(data.frame(time = kaplanMeier$time,
                           s = kaplanMeier$targetSurvival,
                           lower = kaplanMeier$targetSurvivalLb,
                           upper = kaplanMeier$targetSurvivalUb,
                           strata = targetName),
                data.frame(time = kaplanMeier$time,
                           s = kaplanMeier$comparatorSurvival,
                           lower = kaplanMeier$comparatorSurvivalLb,
                           upper = kaplanMeier$comparatorSurvivalUb,
                           strata = comparatorName))
  
  xlims <- c(-max(data$time)/40, max(data$time))
  ylims <- c(min(data$lower), 1)
  xLabel <- "Time in days"
  yLabel <- "Survival probability"
  xBreaks <- kaplanMeier$time[!is.na(kaplanMeier$targetAtRisk)]
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = time,
                                             y = s,
                                             color = strata,
                                             fill = strata,
                                             ymin = lower,
                                             ymax = upper)) +
    ggplot2::geom_ribbon(color = rgb(0, 0, 0, alpha = 0)) +
    ggplot2::geom_step(size = 1) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.8),
                                           rgb(0, 0, 0.8, alpha = 0.8))) +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.3),
                                          rgb(0, 0, 0.8, alpha = 0.3))) +
    ggplot2::scale_x_continuous(xLabel, limits = xlims, breaks = xBreaks) +
    ggplot2::scale_y_continuous(yLabel, limits = ylims) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "top",
                   plot.title = ggplot2::element_text(hjust = 0.5))
 
    targetAtRisk <- kaplanMeier$targetAtRisk[!is.na(kaplanMeier$targetAtRisk)]
    comparatorAtRisk <- kaplanMeier$comparatorAtRisk[!is.na(kaplanMeier$comparatorAtRisk)]
    labels <- data.frame(x = c(0, xBreaks, xBreaks),
                         y = as.factor(c("Number at risk", rep(targetName, length(xBreaks)), rep(comparatorName, length(xBreaks)))),
                         label = c("", formatC(targetAtRisk, big.mark = ",", mode = "integer"), formatC(comparatorAtRisk, big.mark = ",", mode = "integer")))
    labels$y <- factor(labels$y, levels = c(comparatorName, targetName, "Number at risk"))
    dataTable <- ggplot2::ggplot(labels, ggplot2::aes(x = x, y = y, label = label)) +
      ggplot2::geom_text(size = 3.5, vjust = 0.5) +
      ggplot2::scale_x_continuous(xLabel, limits = xlims, breaks = xBreaks) +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     legend.position = "none",
                     panel.border = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(color = "white"),
                     axis.title.x = ggplot2::element_text(color = "white"),
                     axis.title.y = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_line(color = "white"))
    plots <- list(plot, dataTable)
    grobs <- widths <- list()
    for (i in 1:length(plots)) {
      grobs[[i]] <- ggplot2::ggplotGrob(plots[[i]])
      widths[[i]] <- grobs[[i]]$widths[2:5]
    }
    maxwidth <- do.call(grid::unit.pmax, widths)
    for (i in 1:length(grobs)) {
      grobs[[i]]$widths[2:5] <- as.list(maxwidth)
    }
    plot <- gridExtra::grid.arrange(grobs[[1]], grobs[[2]], heights = c(400,100))
    return(plot)
}

drawAttritionDiagram <- function(attrition,
                                 targetLabel = "Target",
                                 comparatorLabel = "Comparator") {
  addStep <- function(data, attrition, row) {
    label <- paste(strwrap(as.character(attrition$description[row]), width = 30), collapse = "\n")
    data$leftBoxText[length(data$leftBoxText) + 1] <- label
    data$rightBoxText[length(data$rightBoxText) + 1] <- paste(targetLabel,
                                                              ": n = ",
                                                              data$currentTarget - attrition$targetPersons[row],
                                                              "\n",
                                                              comparatorLabel,
                                                              ": n = ",
                                                              data$currentComparator - attrition$comparatorPersons[row],
                                                              sep = "")
    data$currentTarget <- attrition$targetPersons[row]
    data$currentComparator <- attrition$comparatorPersons[row]
    return(data)
  }
  data <- list(leftBoxText = c(paste("Exposed:\n",
                                     targetLabel,
                                     ": n = ",
                                     attrition$targetPersons[1],
                                     "\n",
                                     comparatorLabel,
                                     ": n = ",
                                     attrition$comparatorPersons[1],
                                     sep = "")), rightBoxText = c(""), currentTarget = attrition$targetPersons[1], currentComparator = attrition$comparatorPersons[1])
  for (i in 2:nrow(attrition)) {
    data <- addStep(data, attrition, i)
  }
  
  
  data$leftBoxText[length(data$leftBoxText) + 1] <- paste("Study population:\n",
                                                          targetLabel,
                                                          ": n = ",
                                                          data$currentTarget,
                                                          "\n",
                                                          comparatorLabel,
                                                          ": n = ",
                                                          data$currentComparator,
                                                          sep = "")
  leftBoxText <- data$leftBoxText
  rightBoxText <- data$rightBoxText
  nSteps <- length(leftBoxText)
  
  boxHeight <- (1/nSteps) - 0.03
  boxWidth <- 0.45
  shadowOffset <- 0.01
  arrowLength <- 0.01
  x <- function(x) {
    return(0.25 + ((x - 1)/2))
  }
  y <- function(y) {
    return(1 - (y - 0.5) * (1/nSteps))
  }
  
  downArrow <- function(p, x1, y1, x2, y2) {
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x1, y = y1, xend = x2, yend = y2))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 + arrowLength,
                                                       yend = y2 + arrowLength))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 + arrowLength))
    return(p)
  }
  rightArrow <- function(p, x1, y1, x2, y2) {
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x1, y = y1, xend = x2, yend = y2))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 + arrowLength))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 - arrowLength))
    return(p)
  }
  box <- function(p, x, y) {
    p <- p + ggplot2::geom_rect(ggplot2::aes_string(xmin = x - (boxWidth/2) + shadowOffset,
                                                    ymin = y - (boxHeight/2) - shadowOffset,
                                                    xmax = x + (boxWidth/2) + shadowOffset,
                                                    ymax = y + (boxHeight/2) - shadowOffset), fill = rgb(0,
                                                                                                         0,
                                                                                                         0,
                                                                                                         alpha = 0.2))
    p <- p + ggplot2::geom_rect(ggplot2::aes_string(xmin = x - (boxWidth/2),
                                                    ymin = y - (boxHeight/2),
                                                    xmax = x + (boxWidth/2),
                                                    ymax = y + (boxHeight/2)), fill = rgb(0.94,
                                                                                          0.94,
                                                                                          0.94), color = "black")
    return(p)
  }
  label <- function(p, x, y, text, hjust = 0) {
    p <- p + ggplot2::geom_text(ggplot2::aes_string(x = x, y = y, label = paste("\"", text, "\"",
                                                                                sep = "")),
                                hjust = hjust,
                                size = 3.7)
    return(p)
  }
  
  p <- ggplot2::ggplot()
  for (i in 2:nSteps - 1) {
    p <- downArrow(p, x(1), y(i) - (boxHeight/2), x(1), y(i + 1) + (boxHeight/2))
    p <- label(p, x(1) + 0.02, y(i + 0.5), "Y")
  }
  for (i in 2:(nSteps - 1)) {
    p <- rightArrow(p, x(1) + boxWidth/2, y(i), x(2) - boxWidth/2, y(i))
    p <- label(p, x(1.5), y(i) - 0.02, "N", 0.5)
  }
  for (i in 1:nSteps) {
    p <- box(p, x(1), y(i))
  }
  for (i in 2:(nSteps - 1)) {
    p <- box(p, x(2), y(i))
  }
  for (i in 1:nSteps) {
    p <- label(p, x(1) - boxWidth/2 + 0.02, y(i), text = leftBoxText[i])
  }
  for (i in 2:(nSteps - 1)) {
    p <- label(p, x(2) - boxWidth/2 + 0.02, y(i), text = rightBoxText[i])
  }
  p <- p + ggplot2::theme(legend.position = "none",
                          plot.background = ggplot2::element_blank(),
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank(),
                          panel.border = ggplot2::element_blank(),
                          panel.background = ggplot2::element_blank(),
                          axis.text = ggplot2::element_blank(),
                          axis.title = ggplot2::element_blank(),
                          axis.ticks = ggplot2::element_blank())
  
  return(p)
}

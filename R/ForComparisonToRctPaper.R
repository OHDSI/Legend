data <- read.csv("Documents/ComparisonToIndividualRcts.csv")
data <- data[!is.na(data$RR) & !is.na(data$LEGENDRR), ]
n <- nrow(data)

oddsTest <- function(lb1,hr1,ub1,lb2,hr2,ub2) {
    s1 <- (log(ub1) - log(lb1))/(2*1.96)
    s2 <- (log(ub2) - log(lb2))/(2*1.96)
    se <- sqrt(s1^2 + s2^2)
    z <- (log(hr2) - log(hr1))/se
    dat <- 2*pnorm(-abs(z))
    return(dat)
}

concordant <- function(indexRct, indexLegend) {
    p <- oddsTest(data$LBRR[indexRct], data$RR[indexRct], data$UBRR[indexRct],
                  data$LEGENDLB[indexLegend], data$LEGENDRR[indexLegend], data$LEGENDUB[indexLegend])
    return(p > 0.05)
}

# Actual observed concordance:
concordance <- mean(concordant(1:n, 1:n))
print(concordance)

randomDraw <- function(dummy) {
    indexLegend <- 1:n

    # Pick a random RCT for each LEGEND result, just not the RCT that answers the same question as the LEGEND one:
    indexRct <- sample.int(n - 1, length(indexLegend), replace = TRUE)
    idx <- indexRct >= indexLegend
    indexRct[idx] <- indexRct[idx] + 1

    # Alternative: just reshuffle the RCTs:
    # indexRct <- sample(1:n, n)

    randomConcordance <- mean(concordant(indexRct, indexLegend))
    return(randomConcordance)
}
# Distribution of concordances under the null:
dist <- sapply(1:100000, randomDraw)

# How often is null at or above observed?
mean(dist >= concordance)




frequencyScores <- function(pr) {
  coalitionSizes <- sapply(pr$rankingCoalitions, length)
  result <- list()
  for(e in pr$elements) {
    m <- matrix(0, nrow=length(pr$elements), ncol=length(pr$equivalenceClasses))
    coalitionsWithE <- sets::set(e) <= pr$rankingCoalitions
    for(i in which(coalitionsWithE)) {
      col <- equivalenceClassIndex(pr, pr$rankingCoalitions[[i]])
      row <- coalitionSizes[i]
      m[row,col] <- m[row,col] + 1
    }
    result[[paste(e)]] <- m
  }
  structure(result, class = 'l1Score')
}

noSubsetScores <- function(pr) {
  coalitionSizes <- sapply(pr$rankingCoalitions, length)
  result <- list()
  for(e in pr$elements) {
    m <- matrix(0, nrow=length(pr$elements), ncol=length(pr$equivalenceClasses))
    coalitionsWithE <- sets::set(e) <= pr$rankingCoalitions
    for(i in which(coalitionsWithE)) {
      col <- equivalenceClassIndex(pr, pr$rankingCoalitions[[i]])
      if(any(sapply(pr$equivalenceClasses[[col]], function(x) x < pr$rankingCoalitions[[i]])))
        next
      row <- coalitionSizes[i]
      m[row,col] <- m[row,col] + 1
    }
    result[[paste(e)]] <- m
  }
  structure(result, class = 'l1Score')
}

completePr <- function(pr) {
  ps <- lapply(createPowerset(pr$elements), sets::as.set)
  ps <- ps[!(ps %in% pr$rankingCoalitions)]
  if(length(ps) == 0)
    pr
  else
    newPowerRelation(equivalenceClasses = append(pr$equivalenceClasses, list(ps)))
}

makeMonotonic <- function(pr) {
  allCoalitions <- pr$
}





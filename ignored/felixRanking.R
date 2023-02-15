
`[.l1Scores` <- function(x, i, ...) structure(unclass(x)[i], class = 'l1Scores')
`==.l1Scores` <- function(a, b) {all(a[[1]] == b[[1]])}
`>.l1Scores` <- function(a, b) {
  a <- a[[1]]
  b <- b[[1]]
  if(all(a == b)) return(FALSE);
  col <- min(which(sapply(1:ncol(a), function(i) any(a[,i] != b[,i]))))
  row <- min(which(a[,col] != b[,col]))
  a[row,col] > b[row,col]
}


createL1Matrix <- function(powerRelation) {
  l <- lapply(powerRelation$elements, function(x) matrix(0, nrow = length(powerRelation$elements), length(powerRelation$equivalenceClasses)))
  names(l) <- powerRelation$elements

  for(x in seq_along(powerRelation$equivalenceClasses)) {
    for(coalition in powerRelation$equivalenceClasses[[x]]) {
      y <- length(coalition)
      for(i in coalition) {
        l[[i]][y,x] <- l[[i]][y,x] + 1
      }
    }
  }

  structure(l, class = 'l1Scores')
}

rankL1 <- function(powerRelation) {
  doRanking(createL1Matrix(powerRelation))
}



makePrFelixMinimal <- function(powerRelation) {
  eqs <- list()
  current <- list()
  for(eq in powerRelation$equivalenceClasses) {
    current <- append(current, eq)
    current <- Filter(function(x) !sets::set_is_proper_subset(current, x), current)
    eqs <- append(eqs, list(current))
  }

  eqs
}



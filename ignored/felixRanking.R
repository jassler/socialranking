
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


makeStefanoMatrix <- function(pr) {
  l <- lapply(pr$elements, function(x) matrix(0, nrow = length(pr$elements), length(pr$eqs)))
  names(l) <- pr$elements

  for(x in seq_along(pr$eqs)) {
    for(coalition in pr$eqs[[x]]) {
      if(any(sapply(pr$eqs[[x]], function(otherCoal) all(otherCoal %in% coalition) && length(setdiff(coalition, otherCoal)) > 0)))
        next

      y <- length(coalition)
      for(i in coalition) {
        l[[i]][y,x] <- l[[i]][y,x] + 1
      }
    }
  }

  structure(l, class = 'L1Scores')
}

countL1 <- function(mwc, size, i) {
  n <- unlist(mwc) |> unique() |> length()
  result <- 0
  for(r in seq_along(mwc)) {
    result <- result + (-1)^(r-1) * (
      combn(length(mwc), r)
      |> apply(2, function(col) {
          S <- Reduce(union, append(mwc[col], i))
          S_l <- length(S)
          choose(n - S_l, size - S_l)
        })
      |> sum()
    )
  }
  return(result)
}

mwc <- list(c(1,2),c(1,3,4),c(1,3,5))
pr <- PowerRelation(list(mwc)) |> appendMissingCoalitions() |> makePowerRelationMonotonic()
countL1(mwc, 4, 1)

permutate <- function(v) {
  X <- NULL
  for (i in seq_along(v)) X <- rbind(X, cbind(v[i], permutate(v[-i])))
  X
}

sortRows <- function(m) {
  matrix(apply(m, 1, function(x) rev(sort(x))), ncol = ncol(m), dimnames = dimnames(m))
}

generateRandomTable <- function(elements, ratingOptions = 0:9, columns = 3) {
  samples <- sample(ratingOptions, length(elements) * columns, replace = TRUE)
  m <- matrix(samples, ncol = columns)
  m <- t(apply(m, 1, function(x) rev(sort(x))))
  dimnames(m) <- list(elements, paste0('[', 1:columns, ']'))
  m
}

generatePermutationTable <- function(elements) {
  n <- length(elements)
  m <- matrix(seq.int(n * factorial(n) - 1, 0, -1), nrow = length(elements))
  rownames(m) <- elements
  perms <- permutate(elements)
  for(i in 1:ncol(m)) {
    m[,i] <- m[,i][perms[i,]]
  }
  m
}

rankLexicographically <- function(m, order = NULL) {
  if(is.null(order)) order <- seq.int(ncol(m))
  l <- lapply(rownames(m), function(na) m[na,][order])
  names(l) <- rownames(m)
  class(l) <- 'LexcelScores'
  doRanking(NULL, l, rownames(m))
}

mySort <- function(someList, compare) {
  indices <- seq_along(someList)
  comps <- expand.grid(x = indices, y = indices)
  comps$diff <- apply(comps, 1, function(x) {
    if(is.list(someList)) {
      compare(someList[[x[1]]], someList[[x[2]]])
    } else {
      compare(someList[x[1]], someList[x[2]])
    }
  })
  answer <- as.numeric(names(sort(table(comps$x, comps$diff)[,2])))
  result <- someList[answer]
  attributes(result) <- attributes(someList)
  names(result) <- names(someList)[answer]
  return(result)
}

if(interactive()) {
  # tabl <- generateRandomTable(c("a", "b", "c"))
  # mySort(list('abc', 'de', 'fghi'), function(a,b) nchar(a) > nchar(b))
}

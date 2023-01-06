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


if(interactive()) {
  tabl <- generateRandomTable(c("a", "b", "c"))


}

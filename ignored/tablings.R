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
generatePermutationTable <- function(elements, columns = 2^length(elements)) {
  n <- length(elements)
  m <- matrix(seq.int(n * factorial(n) - 1, 0, -1), nrow = length(elements))
  rownames(m) <- elements
  perms <- permutate(elements)
  for(i in 1:ncol(m)) {
    m[,i] <- m[,i][perms[i,]]
  }
  m
}
rankLexicographically <- function(m, order = seq.int(ncol(m))) {
  l <- lapply(rownames(m), function(na) m[na,][order])
  doRanking(structure(l, class = 'LexcelScores', names = rownames(m)))
}
constructSearchTable <- function(elements, columns) {
  perms <- permutate(seq_along(elements))
  m <- matrix(rep_len(t(perms), columns * ncol(perms)), nrow = ncol(perms))
  rownames(m) <- elements
  for(i in seq.int(columns)) {
    m[,i] <- (columns - (i-1)) * nrow(m) + m[,i]
  }
  m
}

randomLexOrder <- function(columns) sample(columns, columns)

# from a list of potential candidates (columns), find out who the single column 'responsible' for the score
findCandidate <- function(m, rankSolutionFunction, solution, candidates) {
  score <- m[,candidates[1]]
  m[,candidates] <- 0

  while(length(candidates) > 1) {
    m[,candidates[1]] <- score
    if(rankSolutionFunction(m) == solution)
      break
    m[,candidates[1]] <- 0
    candidates <- candidates[-1]
  }

  candidates[1]
}









# rankSolutionFunction only returns the ranking solution for a given matrix
# (i.e. rankSolutionFunction(m) produces 'a > b > c')
backtrackLexOrder <- function(m, f) {
  elements <- rownames(m)
  lex <- c()

  permM <- apply(m, 2, function(x) match(elements, doRanking(structure(x, names = elements))))
  rownames(permM) <- elements

  while(length(lex) < ncol(m)) {
    solution <- f(m)
    colOrder <- match(elements, unlist(solution))
    colScore <- rev(seq_along(elements))[colOrder]
    candidate <- which(apply(m, 2, function(x) all(x == colScore)))

    if(length(candidate) > 1)
      candidate <- findCandidate(m, f, solution, candidate)

    m[,candidate] <- 0
    lex <- c(lex, candidate)
  }

  lex
}






if(interactive()) {
  m <- constructSearchTable(letters[1:3], 10)
  print(m)
  lex <- randomLexOrder(ncol(m))
  print(paste0('Starting with: c(', paste(lex, collapse = ', '), ')'))
  backLex <- backtrackLexOrder(m, function(m) rankLexicographically(m, lex))
  print(paste0('   Ended with: c(', paste(backLex, collapse = ', '), ')'))
}

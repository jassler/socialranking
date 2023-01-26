######################################
#           Variable input           #
######################################
quota <- 6
onlyIntegerSolutions <- T

# a list of minimal winning coalitions
mwcs <- list(c(1,2), c(1,3,4), c(1,3,5), c(1,4,5), c(2,3,4,5))
# should output: w = (4, 2, 1, 1, 1)

# for this set of minimal winning coalitions, no weight vector exists
mwcsBad <- list(c(1,2), c(1,3), c(3,4), c(2,4,5), c(1,4,5))

######################################
#     All the computation stuff      #
######################################
library(lpSolve)

isSuperset <- function(bigCoalition, smallCoalition) {
  all(smallCoalition %in% bigCoalition)
}

constructConstraints <- function(mwcs, quota = 10) {
  elements <- unique(sort(unlist(mwcs)))

  f.obj <- rep(1, length(elements))

  # first, all constraints concerned with the mwcs having to surpass the quota
  f.con <- matrix(0, nrow = length(mwcs), ncol = length(elements))
  f.dir <- rep('>=', length(mwcs))
  f.rhs <- rep(quota, length(mwcs))

  for(index in seq_along(mwcs)) {
    for(player in mwcs[[index]]) {
      f.con[index,player] <- 1
    }
  }

  # now, all constraints that say that any coalition that is not winning
  # must not exceed the quota
  for(s in seq_along(elements)) {
    m <- combn(elements, s)
    for(pick in seq.int(ncol(m))) {
      pick <- m[,pick]

      # only add the constraint if the coalition picked is
      # not in the set of all winning coalitions
      # (as in, any superset of all minimal winning coalitions)
      isSuperset <- sapply(mwcs, function(mwc) isSuperset(pick, mwc))
      if(any(isSuperset)) next

      f.con <- rbind(f.con, sapply(elements, function(i) if(i %in% pick) 1 else 0))
      # note: '<' and '<=' makes no difference,
      # so rhs has to be slightly below quota
      f.dir <- c(f.dir, '<=')
      f.rhs <- c(f.rhs, quota - 0.1)
    }
  }

  # elements cannot be 0
  # again, '>' and '>=' makes no difference, so rhs has to be slightly above 0
  f.con <- rbind(f.con, diag(length(elements)))
  f.dir <- c(f.dir, rep('>=', length(elements)))
  f.rhs <- c(f.rhs, rep(0.1, length(elements)))

  list(
    obj = f.obj,
    con = f.con,
    dir = f.dir,
    rhs = f.rhs
  )
}

printConstraints <- function(progr) {
  m <- progr$con
  for(i in seq.int(ncol(m))) {
    m[,i] <- sapply(m[,i], function(x) if(x == 1) paste0('w', i) else '')
  }
  for(i in seq.int(nrow(m))) {
    els <- which(m[i,] != '')
    if(length(els) <= 1) next
    for(j in els[-length(els)]) {
      m[i,j] <- paste(m[i,j], '+ ')
    }
  }
  m <- cbind(m, paste0(' ', progr$dir, ' '), progr$rhs)
  lines <- sapply(seq.int(nrow(m)), function(i) paste(m[i,], collapse = ''))
  writeLines(lines)
}

findWeightVector <- function(mwcs, quota = 10, integerOnly = TRUE) {
  progr <- constructConstraints(mwcs, quota)
  # uncomment line below to view constraints added
  # printConstraints(progr)
  intVec <- if(integerOnly) seq.int(progr$obj) else c()
  result <- lp('max', progr$obj, progr$con, progr$dir, progr$rhs, int.vec = intVec)

  mwcStr <- sapply(mwcs, function(x) paste(x, collapse = ', '))
  mwcStr <- paste0('{{', paste(mwcStr, collapse = '}, {'), '}}')
  writeLines(paste0('q = ', quota, ', mwc = ', mwcStr))
  if(all(result$solution == 0)) {
    writeLines(paste0('Found no ', if(integerOnly) 'integer ', 'solutions\n'))
  } else {
    writeLines(c(
      paste0('w = (', paste(result$solution, collapse = ', '), ')'),
      ''
    ))
  }

  result
}

result <- findWeightVector(mwcs, quota, onlyIntegerSolutions)
result <- findWeightVector(mwcsBad, quota, onlyIntegerSolutions)

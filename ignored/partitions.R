library(partitions)

permutate <- function(v) {
  X <- NULL
  for (i in seq_along(v)) X <- rbind(X, cbind(v[i], permutate(v[-i])))
  X
}

prGenerator <- function(P) {
  parts <- listParts(length(P))
  total <- length(parts)
  perms <- permutate(seq_along(parts[[1]]))
  parts <- rapply(parts, function(i) P[i], how = "replace")
  function() {
    if(length(perms) == 0) {
      if(length(parts) <= 1)
        return(NULL)
      parts <<- parts[-1]
      perms <<- permutate(seq_along(parts[[1]]))
      print(paste(total-length(parts), "/", total, ":", capture.output(parts[[1]])))
    }

    #eqs <- rapply(parts[[1]], function(i) P[i], how = "replace")
    perm <- perms[1,]
    perms <<- perms[-1,,drop=FALSE]
    newPowerRelation(equivalenceClasses = parts[[1]][perm])
    # newPowerRelation(equivalenceClasses = eqs[perm])
  }
}

listRankingResults <- function(P) {
  prs <- prGenerator(P)
  r <- list(
    banz = c(),
    cope = c(),
    ks = c(),
    lex = c(),
    duallex = c()
  )

  pr <- prs()
  while(!is.null(pr)) {
    r$banz <- c(r$banz, capture.output(ordinalBanzhafRanking(pr)))
    r$cope <- c(r$cope, capture.output(copelandRanking(pr)))
    r$ks <- c(r$ks, capture.output(kramerSimpsonRanking(pr)))
    r$lex <- c(r$lex, capture.output(lexcelRanking(pr)))
    r$duallex <- c(r$duallex, capture.output(dualLexcelRanking(pr)))
    pr <- prs()
  }

  r
}

frameIt <- function(r) {
  els <- unique(unlist(r))
  els <- els[order(paste(gsub("[^ ~>]", "", els), gsub("[^a-z]", "", els)))]
  data.frame(
    banz = sapply(els, function(s) sum(r$banz == s)),
    cope = sapply(els, function(s) sum(r$cope == s)),
    ks = sapply(els, function(s) sum(r$ks == s)),
    lex = sapply(els, function(s) sum(r$lex == s)),
    duallex = sapply(els, function(s) sum(r$duallex == s))
  )
}




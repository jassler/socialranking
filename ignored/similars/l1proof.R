
coalToKey <- function(S) paste0('{',paste(S,collapse=','),'}')

permuteS <- function(pr, mapping, e1, e2) {
  eqs <- pr$eqs
  for(k in seq_along(eqs)) {
    for(s in seq_along(eqs[[k]])) {
      S <- eqs[[k]][[s]]
      if(!(e1 %in% S) || e2 %in% S) {
        next
      }

      piS <- mapping[[coalToKey(setdiff(S, e1))]]
      if(is.null(piS)) {
        next
      }
      eqs[[k]][[s]] <- c(e1, piS)
    }
  }
  PowerRelation(eqs)
}

applyL1Proof <- function(pr, e1, e2) {
  writeLines('Starting with')
  print(pr)

  m1 <- L1Scores(pr, e1)[1]
  m2 <- L1Scores(pr, e2)[1]
  writeLines(paste0('L1(', e1, ') ='))
  print(m1[[1]])
  writeLines(paste0('L1(', e2, ') ='))
  print(m2[[1]])
  writeLines('')

  if(m2 > m1) {
    writeLines(paste0(e2, 'P', e1, ', stopping here. Please switch order of parameters.'))
    return()
  }
  if(m1 == m2) {
    writeLines(paste0(e1, 'I', e2, ', nothing to do'))
    return()
  }
  writeLines(paste0(e1, 'P', e2))

  k <- which(m1[[1]] != m2[[1]])[1] - 1
  s <- k %% length(pr$elements) + 1
  k <- k %/% length(pr$elements) + 1
  writeLines(paste0('ŝ = ', s, ', k̂ = ', k))

  writeLines(paste0('\nStep 1: Form a union over the equivalence classes from Σ_', k+1, ' onwards'))
  if(length(pr$eqs) > k + 1) {
    eqs <- pr$eqs
    eqs[[k+1]] <- unlist(eqs[(k+1):length(eqs)], recursive = FALSE)
    eqs <- eqs[-((k+2):length(eqs))]
    pr <- PowerRelation(eqs)
  }
  print(pr)

  others <- setdiff(pr$elements, c(e1, e2))
  writeLines(paste0('\nStep 2: Apply a bijection over the power set of the elements N\\{', e1, ',', e2, '} = {', paste(others, collapse = ','), '}'))
  coals <- createPowerset(others)
  mapping <- list()

  findTBelowKHat <- function(S, eqIndex) {
    # assuming eqIndex < k
    for(coal in pr$eqs[[eqIndex]]) {
      if(
        !(e1 %in% coal) &&
        e2 %in% coal &&
        length(coal) == length(S) + 1 # |pi(S)| = |S|
      ) {
        piS <- setdiff(coal, e2)
        if(coalToKey(piS) %in% names(mapping)) {
          next
        }
        return(piS)
      }
    }
    return(NULL)
  }

  addKeyCoal <- function(coal, S) {
    keyCoal <- coalToKey(coal)
    keyS <- coalToKey(S)
    writeLines(paste0('π(', keyCoal, ') = ', keyS))
    mapping[keyCoal] <<- list(S)

    if(keyCoal != keyS) {
      writeLines(paste0('π(', keyS, ') = ', keyCoal))
      mapping[keyS] <<- list(coal)
    }
  }

  for(coal in coals) {
    i <- pr$coalitionLookup(c(e1, coal))
    if(i < k || (i == k && (length(coal) + 1) < s)) {
      S <- findTBelowKHat(coal, i)
      if(is.null(S)) {
        stop('S should not be NULL')
      }
      addKeyCoal(coal, S)
    } else if(i == k) {
      # |S| >= s
      s_ <- length(coal) + 1
      if(m1[[1]][s_,i] >= m2[[1]][s_,i]) {
        S <- findTBelowKHat(coal, i)
        if(is.null(S)) {
          next
        }
        addKeyCoal(coal, S)
      }
    }
  }
  pr <- permuteS(pr, mapping, e1, e2)

  for(s_ in (s+1):(nrow(m1[[1]]))) {
    d <- m2[[1]][(length(coal) + 1),k] - m1[[1]][(length(coal) + 1),k]
    if(d > 0) {

    }
  }


  return(pr)
}

pr <- appendMissingCoalitions(as.PowerRelation('(134 ~ 135 ~ 234 ~ 245) > (1 ~ 2) > (13 ~ 14 ~ 25 ~ 235) > (15 ~ 23 ~ 24 ~ 145)'))
applyL1Proof(pr, 1, 2)











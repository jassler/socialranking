library(socialranking)
library(sets)

# makeStefanoMatrix <- function(pr) {
#   l <- lapply(pr$elements, function(x) matrix(0, nrow = length(pr$elements), length(pr$eqs)))
#   names(l) <- pr$elements
#
#   for(x in seq_along(pr$eqs)) {
#     for(coalition in pr$eqs[[x]]) {
#       if(any(sapply(pr$eqs[[x]], function(otherCoal) all(otherCoal %in% coalition) && length(setdiff(coalition, otherCoal)) > 0)))
#         next
#
#       y <- length(coalition)
#       for(i in coalition) {
#         l[[i]][y,x] <- l[[i]][y,x] + 1
#       }
#     }
#   }
#
#   structure(l, class = 'L1Scores')
# }
#
# s <- (function(elements) {
#   l <- list()
#   gen <- powerRelationGenerator(createPowerset(elements, includeEmptySet = FALSE))
#   print('doing permutations')
#   while(!is.null(pr <- gen())) {
#     if(coalitionLookup(pr, elements) > 1)
#       gen <- generateNextPartition(gen)
#     else
#       l[[length(l)+1]] <- capture.output(makePowerRelationMonotonic(pr))
#   }
#
#   print('doing sets')
#   as.set(l)
# })(c('a','b','c'))
#
# (function(elements) {
#   superset <- if(TRUE) {
#     createPowerset(elements, includeEmptySet = FALSE)
#   } else {
#     supersettmp <- createPowerset(elements, includeEmptySet = FALSE)
#     superset <- list()
#     for(i in seq_along(elements)) {
#       superset <- append(superset, supersettmp[which(sapply(supersettmp, length) == i)])
#     }
#     superset
#   }
#
#   m <- matrix(nrow = 0, ncol = length(superset), dimnames = list(c(), sapply(superset, paste0, collapse='')))
#   s <- lapply(colnames(m), function(x) as.set(strsplit(x, '')[[1]]))
#   for(coal in s) {
#     r <- sapply(s, set_is_subset, x=coal)
#     m <- rbind(m, sapply(r, function(v) if(v) '.' else ''))
#   }
#   rownames(m) <- rep('', nrow(m))
#
#   s <- paste(colnames(m),collapse=';')
#   s <- paste0(s,'\n')
#   s <- paste0(s,paste(
#     apply(m, 1, paste, collapse=';'),
#     collapse='\n'
#   ))
#   clipr::write_clip(s)
#   #m
# })(letters[1:7])
#
# (function() {
#   coals <- createPowerset(1:9, includeEmptySet = FALSE)
#   s <- length(coals)-1
#   count <- 0
#
#   while(TRUE) {
#     pr <- as.PowerRelation(
#       sample(coals),
#       sample(c('>','~'),length(coals)-1,replace=TRUE)
#     )
#     pr <- makePowerRelationMonotonic(pr)
#     rank <- L1Ranking(pr)
#     if(!(rank == lexcelRanking(pr))) {
#       print('AAAAAAAAAAAAAA')
#       print(pr)
#       print('AAAAAAAAAAAAAA')
#     }
#     count <- count + 1
#     if(count %% 100 == 0) {
#       print(paste(count, ':', capture.output(pr)))
#     }
#   }
# })()
#
# (function(prs) {
#   ks <- 0
#   cop <- 0
#   for(pr in prs) {
#     s1 <- kramerSimpsonRanking(pr); s2 <- copelandRanking(pr);
#     s1 <- s1[[length(s1)]]; s2 <- s2[[length(s2)]];
#     if((length(s1) <= length(s2) && all(s1 %in% s2))) {
#       ks <- ks + 1
#       print('ks')
#     } else if(all(s2 %in% s1)) {
#       cop <- cop + 1
#       print('cop')
#       print(pr)
#     }
#   }
#
#   print(paste('ks =', ks, '| cop =', cop))
# })(s)

nextCoal <- function(counter, prevs) {
  coals <- which(counter == 0)

  s <- 0

  if(length(coals) == 0) {
    s <- 1
    print(paste(prevs, collapse = ' > '))

  } else for(i in seq_along(coals)) {
    coal <- coals[i]
    counter[coal] <- -1

    s <- s + nextCoal(chooseCoal(counter, coal), c(prevs, names(coal)))

    counter[coal] <- 0
  }

  return(s)
}












toCoalition <- function(coalitions) {
  sapply(coalitions, function(coalition)
    paste(which(intToBits(coalition) == 1), collapse = '')
  )
}



# 123, 12, 13, 23, 1, 2, 3, {}





chooseCoal <- function(counter, coal) {
  i <- 0
  p <- 1
  while(p < coal) {
    if(bitwAnd((coal-1), p) != 0) {
      i <- bitwXor((coal-1), p) + 1
      counter[i] <- counter[i] - 1
    }
    p <- bitwShiftL(p, 1)
  }

  counter
}

nextCoalWo <- function(counter) {
  coals <- which(counter == 0)

  s <- 0

  if(length(coals) == 0) {
    s <- 1

  } else for(i in seq_along(coals)) {
    coal <- coals[i]
    counter[coal] <- -1

    s <- s + nextCoalWo(chooseCoal(counter, coal))

    counter[coal] <- 0
  }

  return(s)
}

generatePrs <- function(amount) {
  pascalRow <- numbers::pascal_triangle(amount)[amount+1,]

  # counter <- lapply(seq_along(pascalRow), function(i) {
  #   rep(amount - i + 1, pascalRow[i])
  # }) |> unlist()

  nms <- toCoalition(0:(2^amount-1))
  counter <- structure(
    sapply(nms, function(nm) amount - nchar(nm)),
    names = nms
  )

  counter[(length(counter)-2):length(counter)] <- -1
  counter <- chooseCoal(counter, length(counter))
  counter <- chooseCoal(counter, length(counter)-1)
  counter <- chooseCoal(counter, length(counter)-2)

  #nextCoal(counter, c())
  #nextCoal(counter, names(counter)[length(counter):(length(counter)-1)])# list('1234', '123'))
  nextCoalWo(counter)
}

















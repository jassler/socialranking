library(socialranking)

checkSimilars <- function(
    coalitions = createPowerset(c('a','b')),
    srfs = list(ordinalBanzhafRanking, copelandRanking, kramerSimpsonRanking, lexcelRanking),
    colNames = c('banz', 'cope', 'ks', 'lex')
) {
  gen <- powerRelationGenerator(coalitions)
  pr <- gen()
  df <- data.frame(input = capture.output(pr))
  for(srf in srfs) {
    df <- cbind(df, capture.output(srf(pr)))
  }
  names(df) <- c('input', colNames)

  while(!is.null(pr <- gen())) {
    row <- c(
      capture.output(pr),
      sapply(srfs, function(srf) capture.output(srf(pr)))
    )
    df <- rbind(df, row)
  }

  df
}

# df <- checkSimilars(createPowerset(c('a','b','c')))
# write.csv(df, '/Users/felix/Documents/progr/R/socialranking/ignored/similars/3.csv')

# gen <- powerRelationGenerator(createPowerset(letters[1:3]), TRUE)
# bbb <- c(FALSE, FALSE)
# while(!is.null(pr <- gen()) && !all(bbb)) {
#   if(dominates(pr, 'a', 'b', strictly = TRUE) && (pr %:% 'b' %>banz% 'a')) {
#     print(pr)
#   }
#   # if(dominates(pr, 'a', 'b', strictly = TRUE)) {
#   #   if(pr %:% 'b' %>=banz% 'a') {
#   #     print(pr)
#   #   }
#     # sCop <- copelandScores(pr, c('a', 'b'))
#     # sKS <- kramerSimpsonScores(pr, c('a', 'b'))
#     # if(!(sCop[1] > sCop[2]) && !bbb[1]) {
#     #   print('Missed for Copeland')
#     #   print(pr)
#     #   print(sCop)
#     #   bbb[1] <- TRUE
#     # }
#     # if(sKS[2] > sKS[1] && !bbb[2]) {
#     #   print('Missed for KramerSimpson')
#     #   print(pr)
#     #   print(sKS)
#     #   bbb[2] <- TRUE
#     # }
#   # }
# }

createRandomPr <- function(Uij, coalsIJ, i, j, comparators = c('>','~')) {
  l <- sample(append(Uij, coalsIJ))
  for(uij in Uij) {
    eis <- sample(1:length(l), 2, replace = TRUE)
    l <- append(l, list(c(j, uij)), after = max(eis))
    l <- append(l, list(c(i, uij)), after = min(eis))
  }
  as.PowerRelation(
    l,
    comparators = sample(comparators,length(l)-1,replace = TRUE)
  )
}



testRanking <- function(score, N = 4, linearOrders = FALSE, ignoreI = FALSE) {
  stopifnot(N >= 3)
  coals <- createPowerset(letters[3:N])
  coalsIJ <- lapply(coals, function(co) c('a','b',co))
  comparators <- c('>', if(linearOrders) '~')

  count <- 0
  while(TRUE) {
    count <- count + 1
    if(count %% 1000 == 0) {
      print(count)
    }
    pr <- createRandomPr(coals, coalsIJ, 'a', 'b')
    sc <- score(pr, elements = c('a','b'))
    if(!ignoreI && sc[1] == sc[2]) {
      if(dominates(pr, 'a', 'b', strictly = TRUE)) {
        print('sc[1] == sc[2], but a dominates b strictly')
        return(pr)
      }
    } else if(sc[2] > sc[1]) {
      print('sc[2] > sc[1] when it should be sc[1] >= sc[2]')
      return(pr)
    }
  }
}

pr <- testRanking(ordinalBanzhafScores, N = 6, ignoreI = T)
print(pr)

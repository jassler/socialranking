
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

df <- checkSimilars(createPowerset(c('a','b','c')))
write.csv(df, '/Users/felix/Documents/progr/R/socialranking/ignored/similars/3.csv')

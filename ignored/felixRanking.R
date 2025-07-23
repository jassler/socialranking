
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


(function() {
  pr <- as.PowerRelation('1~23 > 13~24 > 3~12~14 > 2') |> appendMissingCoalitions()
  lexSc <- lexcelScores(pr)
  L1Sc <- L1Scores(pr)
  for(e in pr$elements) {
    writeLines(paste0('\\theta^{\\succsim,', e, '} = (', paste(lexSc[[e]], collapse=', '), ')'))
  }
  for(e in pr$elements) {
    writeLines((
      paste0('\\begin{aligned}&M^{\\succsim,',e,'} = \\\\ &\\begin{bmatrix}\n        ', paste(apply(L1Sc[[e]], 1, paste, collapse=' & '), collapse='\\\\\n        '), '\n      \\end{bmatrix}\\end{aligned}')
    ))
  }

  prPr <- (
    capture.output(pr)
    |> stringr::str_replace_all('(\\(|\\))', '')
    |> stringr::str_replace_all('([^>]*) ', '\\\\underbrace{\\1}_{X}')
    |> gsub(pattern='~', replacement='\\\\sim')
    |> gsub(pattern='>', replacement=' \\\\succ ')
    |> gsub(pattern='(\\d+)', replacement='\\\\{\\1\\\\}')
    |> gsub(pattern='(\\d)(\\d)', replacement='\\1, \\2')
    |> gsub(pattern='(\\d)(\\d)', replacement='\\1, \\2')
    |> gsub(pattern='(\\d)(\\d)', replacement='\\1, \\2')
  )

  count <- 1
  prPrNew <- stringr::str_replace(prPr, 'X', paste0('\\\\Sigma_', count))
  while(prPrNew != prPr) {
    count <- count + 1
    prPr <- prPrNew
    prPrNew <- stringr::str_replace(prPr, 'X', paste0('\\\\Sigma_', count))
  }

  writeLines(prPrNew)

  print('lex-cel')
  print(lexcelRanking(pr))
  print('L1')
  print(L1Ranking(pr))
  print('CP')
  cps <- list(
    capture.output(cpMajorityComparison(pr, 1, 2, strictly = TRUE)) |> strsplit('\\n'),
    capture.output(cpMajorityComparison(pr, 1, 3, strictly = TRUE)) |> strsplit('\\n'),
    capture.output(cpMajorityComparison(pr, 2, 3, strictly = TRUE)) |> strsplit('\\n')
  )
  for(cp in cps) {
    writeLines('\\begin{aligned}')
    writeLines(paste0(
      '    ',
      (cp[2]
       |> stringr::str_replace_all('D_(\\d\\d)', 'D_{\\1}(\\\\succsim)')
       |> stringr::str_replace_all('\\{\\}', '\\\\varnothing')
       |> stringr::str_replace_all('=', '& =')
       |> stringr::str_replace_all('(\\D)(\\d)(\\D)', '\\1\\\\{\\2\\\\}\\3')
      ),
      ' \\\\\n    ',
      (cp[3]
       |> stringr::str_replace_all('D_(\\d\\d)', 'D_{\\1}(\\\\succsim)')
       |> stringr::str_replace_all('\\{\\}', '\\\\varnothing')
       |> stringr::str_replace_all('=', '& =')
       |> stringr::str_replace_all('(\\D)(\\d)(\\D)', '\\1\\\\{\\2\\\\}\\3')
      )
    ))
    writeLines('  \\end{aligned}')
  }
})()


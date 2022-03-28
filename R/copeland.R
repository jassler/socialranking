#' @export
`[.CopelandScores` <- function(x, i, ...) structure(unclass(x)[i], class = 'CopelandScores')

#' @export
`==.CopelandScores` <- function(a, b) {sum(a[[1]]) == sum(b[[1]])}

#' @export
`>.CopelandScores` <- function(a, b) {
  sum(a[[1]]) > sum(b[[1]])
}

#' @export
is.na.CopelandScores <- function(x) FALSE

#' Copeland-like method
#'
#' Based on [`cpMajorityComparison()`], add or subtract scores
#' based on how an element fares against the others.
#'
#' \loadmathjax
#' Strongly inspired by the Copeland score of social choice theory \insertCite{1951Copeland}{socialranking},
#' the Copeland-like solution is based on the net flow of the CP-majority graph \insertCite{2021Manipulability}{socialranking}.
#'
#' Individuals are ordered according to the number of pairwise winning comparisons, minus the number of pairwise losing comparisons,
#' over the set of all CP-comparisons.
#'
#' More formally, in a given `PowerRelation pr` with element \mjseqn{i}, count the number of elements
#' \mjeqn{j \in N \setminus \lbrace i \rbrace}{j in N - \{i\}} where
#' [`cpMajorityComparison`]`(pr, i, j) >= 0` and subtract those where
#' [`cpMajorityComparison`]`(pr, i, j) <= 0`.
#'
#' @return A list with a vector of length 2 for each `element`.
#' Index 1 contains the number of pairwise winning comparisons,
#' index 2 the number of pairwise losing comparisons.
#' Index 2 is negative, so the sum of both numbers corresponds to the resulting Copeland value.
#'
#' @template param/powerRelation
#' @template param/elements
#'
#' @family CP-majority based functions
#' @family score vector functions
#'
#' @references
#' \insertRef{2021Manipulability}{socialranking}
#'
#' \insertRef{1951Copeland}{socialranking}
#'
#' @examples
#' # (123 ~ 12 ~ 3 ~ 1) > (2 ~ 23) > 13
#' pr <- newPowerRelation(
#'   c(1,2,3),
#'   "~", c(1,2),
#'   "~", c(3),
#'   "~", c(1),
#'   ">", c(2),
#'   "~", c(2,3),
#'   ">", c(1,3)
#' )
#'
#' # `1` = 1
#' # `2` = 0
#' # `3` = -1
#' copelandScores(pr)
#'
#' # only calculate results for two elements
#' # `1` = 1
#' # `3` = -1
#' copelandScores(pr, c(1,3))
#'
#' # or just one element
#' copelandScores(pr, 2)
#'
#' @export
copelandScores <- function(powerRelation, elements = NULL) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  if(is.null(elements)) elements <- powerRelation$elements
  else if(!is.null(err <- powerRelationHasElements(powerRelation, elements))) stop(err)
  # --- end checks --- #

  eSet <- sets::as.set(powerRelation$elements)
  result <- list()
  for(e in elements) {
    scores <- sapply(eSet - sets::set(e), function(p2) sum(cpMajorityComparisonScore(powerRelation, e, p2)))
    result[[paste(e)]] <- c(sum(scores >= 0), -sum(scores <= 0))
  }

  structure(result, class = 'CopelandScores')
}

#' Copeland ranking
#'
#' `copelandRanking` returns the corresponding ranking.
#'
#' @template param/powerRelation
#'
#' @rdname copelandScores
#'
#' @examples
#' # 1 > 2 > 3
#' copelandRanking(pr)
#'
#' @export
copelandRanking <- function(powerRelation) {
  doRanking(
    powerRelation,
    copelandScores(powerRelation)
  )
}

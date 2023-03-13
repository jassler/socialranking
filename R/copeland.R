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
#' Strongly inspired by the Copeland score of social choice theory \insertCite{1951Copeland}{socialranking},
#' the Copeland-like solution is based on the net flow of the CP-majority graph \insertCite{2021Manipulability}{socialranking}.
#'
#' Individuals are ordered according to the number of pairwise winning comparisons, minus the number of pairwise losing comparisons,
#' over the set of all CP-comparisons.
#'
#' More formally, in a given `PowerRelation pr` with element \eqn{i}{i}, count the number of elements
#' \eqn{j \in N \setminus \lbrace i \rbrace}{j in N - \{i\}} where
#' [`cpMajorityComparison`]`(pr, i, j) >= 0` and subtract those where
#' [`cpMajorityComparison`]`(pr, i, j) <= 0`.
#'
#' @return Score function returns a list of type `CopelandScores` and length of `powerRelation$elements`
#' (unless parameter `elements` is specified). Each element is a vector of 2 numbers,
#' the number of pairwise winning comparisons and the number of pairwise losing comparisons.
#' Those two numbers summed together gives us the actual ordinal Copeland score.
#'
#' @template param/powerRelation
#' @template param/elements
#'
#' @family CP-majority based functions
#' @family ranking solution functions
#'
#' @references
#' \insertRef{2021Manipulability}{socialranking}
#'
#' \insertRef{1951Copeland}{socialranking}
#'
#' @examples
#' # (123 ~ 12 ~ 3 ~ 1) > (2 ~ 23) > 13
#' pr <- PowerRelation(list(
#'   list(c(1,2,3), c(1,2), 3, 1),
#'   list(c(2,3), 2),
#'   list(c(1,3))
#' ))
#'
#' copelandScores(pr)
#' # `1` = c(2, -1)
#' # `2` = c(2, -2)
#' # `3` = c(1, -2)
#'
#' # only calculate results for two elements
#' copelandScores(pr, c(1,3))
#' # `1` = c(2, -1)
#' # `3` = c(1, -2)
#'
#' # or just one element
#' copelandScores(pr, 2)
#' # `2` = c(2, -2)
#'
#' @export
copelandScores <- function(powerRelation, elements = powerRelation$elements) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #

  result <- list()
  for(e in elements) {
    scores <- setdiff(powerRelation$elements, e) |> sapply(function(p2) {
      sum(cpMajorityComparisonScore(powerRelation, e, p2))
    })
    result[[paste(e)]] <- c(sum(scores >= 0), -sum(scores <= 0))
  }


  structure(result, class = 'CopelandScores')
}

#' Copeland ranking
#'
#' `copelandRanking()` returns the corresponding ranking.
#'
#' @template param/powerRelation
#'
#' @rdname copelandScores
#'
#' @template return/ranking
#'
#' @examples
#' # 1 > 2 > 3
#' copelandRanking(pr)
#'
#' @export
copelandRanking <- function(powerRelation) {
  doRanking(copelandScores(powerRelation))
}

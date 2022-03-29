#' @export
`[.KramerSimpsonScores` <- function(x, i, ...) structure(unclass(x)[i], class = 'KramerSimpsonScores')

#' @export
`==.KramerSimpsonScores` <- function(a, b) {a[[1]] == b[[1]]}

#' @export
`>.KramerSimpsonScores` <- function(a, b) {
  # lower is better
  a[[1]] < b[[1]]
}

#' @export
is.na.KramerSimpsonScores <- function(x) FALSE

#' Kramer-Simpson-like method
#'
#' Calculate the Kramer-Simpson-like scores.
#' Lower scores are better.
#'
#' \loadmathjax
#' Inspired by the Kramer-Simpson method of social choice theory \insertCite{1969Simpson}{socialranking} \insertCite{1975Kramer}{socialranking}, the _Kramer-Simpson-like_
#' method compares each element against all other elements using the CP-Majority rule.
#'
#' For a given element \mjseqn{i} calculate the [`cpMajorityComparisonScore`]
#' against all elements \mjseqn{j}, \mjeqn{d_{ji}(\succeq)}{d_ji(>=)} (notice that \mjseqn{i} and
#' \mjseqn{j} are in reverse order).
#' \mjeqn{\max_{j \in N \setminus \lbrace i \rbrace}(d_{ji}(\succeq))}{max_{j in N \\ {i}}(d_ji(>=))} then
#' determines the final score, where lower scoring elements are ranked higher.
#'
#' @section Note:
#'
#' By default this function does not compare \mjeqn{d_{ii}(\succeq)}{d_ii(>=)}. In other terms,
#' the score of every element is the maximum CP-Majority comparison score against all other elements.
#'
#' This is slightly different from definitions found in
#' \insertCite{2021Manipulability}{socialranking}. Since by definition  \mjeqn{d_{ii}(\succeq) = 0}{d_ii(>=) = 0}
#' always holds, the Kramer-Simpson scores in those cases will never be negative, possibly discarding valuable
#' information.
#'
#' For this reason `kramerSimpsonScores` and `kramerSimpsonRanking` includes a
#' `compIvsI` parameter that can be set to `TRUE` if one wishes for \mjeqn{d_{ii}(\succeq) = 0}{d_ii(>=) = 0}
#' to be included in the comparisons. Put into mathematical terms, if:
#'
#' | `compIvsI` | Score definition                                                                                   |
#' | ---------- | -------------------------------------------------------------------------------------------------- |
#' | `FALSE`    | \mjeqn{\max_{j \in N \setminus \lbrace i \rbrace}(d_{ji}(\succeq))}{max_{j in N \\ {i}}(d_ji(>=))} |
#' | `TRUE`     | \mjeqn{\max_{j \in N}(d_{ji}(\succeq))}{max_{j in N}(d_ji(>=))}                                    |
#'
#'
#' @template param/powerRelation
#' @template param/elements
#' @param compIvsI If `TRUE`, include CP-Majority comparison \mjeqn{d_{ii}(\succeq)}{d_ii(>=)}, or, the CP-Majority
#' comparison score of an element against itself, which is always 0.
#'
#' @family CP-majority based functions
#' @family score vector functions
#'
#' @return Score function returns a list of type `KramerSimpsonScores` and length of `powerRelation$elements`
#' (unless parameter `elements` is specified). Lower scoring elements are ranked higher.
#'
#' @references
#' \insertRef{2021Manipulability}{socialranking}
#'
#' \insertRef{1969Simpson}{socialranking}
#'
#' \insertRef{1975Kramer}{socialranking}
#'
#' @examples
#' # 2 > (1 ~ 3) > 12 > (13 ~ 23) > {} > 123
#' pr <- newPowerRelation(
#'   2,
#'   ">", 1,
#'   "~", 3,
#'   ">", c(1,2),
#'   ">", c(1,3),
#'   "~", c(2,3),
#'   ">", c(),
#'   ">", c(1,2,3)
#' )
#'
#' # get scores for all elements
#' # cpMajorityComparisonScore(pr, 2, 1) = 1
#' # cpMajorityComparisonScore(pr, 3, 1) = -1
#' # therefore the Kramer-Simpson-Score for element
#' # `1` = 1
#' #
#' # Score analogous for the other elements
#' # `2` = -1
#' # `3` = 2
#' kramerSimpsonScores(pr)
#'
#' # get scores for two elements
#' # `1` = 1
#' # `3` = 2
#' kramerSimpsonScores(pr, c(1,3))
#'
#' # or single element
#' # result is still a list
#' kramerSimpsonScores(pr, 2)
#'
#' # note how the previous result of element 2 is negative.
#' # If we compare element 2 against itself, its max score will be 0
#' kramerSimpsonScores(pr, 2, compIvsI = TRUE)
#'
#' @export
kramerSimpsonScores <- function(powerRelation, elements = NULL, compIvsI = FALSE) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  if(is.null(elements)) elements <- powerRelation$elements
  else if(!is.null(err <- powerRelationHasElements(powerRelation, elements))) stop(err)
  # --- end checks --- #

  result <- list()
  for(p1 in elements) {
    opponents <- if(compIvsI)
      powerRelation$elements
    else
      powerRelation$elements[-which(powerRelation$elements == p1)]

    result[[paste(p1)]] <- max(
      sapply(opponents, function(p2) {
        sum(cpMajorityComparisonScore(powerRelation, p2, p1))
      })
    )
  }

  structure(result, class = 'KramerSimpsonScores')
}

#' Kramer-Simpson-like ranking
#'
#' [`kramerSimpsonRanking`] returns the corresponding ranking.
#'
#' @template param/powerRelation
#'
#' @rdname kramerSimpsonScores
#'
#' @template return/ranking
#'
#' @examples
#' # 2 > 1 > 3
#' kramerSimpsonRanking(pr)
#'
#' @export
kramerSimpsonRanking <- function(powerRelation, compIvsI = FALSE) {
  doRanking(
    powerRelation,
    kramerSimpsonScores(powerRelation, compIvsI = compIvsI)
  )
}

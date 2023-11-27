#' Kramer-Simpson-like method
#'
#' Calculate the Kramer-Simpson-like scores.
#' Higher scores are better.
#'
#' Inspired by the Kramer-Simpson method of social choice theory \insertCite{1969Simpson}{socialranking} \insertCite{1975Kramer}{socialranking}, the _Kramer-Simpson-like_
#' method compares each element against all other elements using the CP-Majority rule.
#'
#' For a given element \eqn{i}{i}, calculate the [`cpMajorityComparisonScore()`]
#' against all elements \eqn{j}{j}, \eqn{d_{ji}(\succsim)}{d_ji(>=)} (notice that \eqn{i}{i} and
#' \eqn{j}{j} are in reverse order).
#' \eqn{-\max_{j \in N \setminus \lbrace i \rbrace}(d_{ji}(\succsim))}{-max_{j in N}(d_ji(>=))} then
#' determines the final score, where higher scoring elements are ranked higher (notice the negative symbol in front of the \eqn{\max}{max} statement).
#'
#' The implementation slightly differs from the original definition in \insertRef{2021Manipulability}{socialranking}.
#' While the ranking solution itself is the same, the scores for this package are intentionally multiplied by -1,
#' as this significantly improves performance when sorting the elements, as well as making simple comparisons between elements more logical to the user.
#'
#' @template param/powerRelation
#' @template param/elements
#'
#' @family CP-majority based functions
#' @family ranking solution functions
#'
#' @return Score function returns a vector of type `KramerSimpsonScores` and length of `powerRelation$elements`
#' (unless parameter `elements` is specified). Higher scoring elements are ranked higher.
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
#' pr <- as.PowerRelation("2 > (1~3) > 12 > (13~23) > {} > 123")
#'
#' # get scores for all elements
#' # cpMajorityComparisonScore(pr, 2, 1, strictly = TRUE)[1] = 1
#' # cpMajorityComparisonScore(pr, 3, 1, strictly = TRUE)[1] = 0
#' # therefore the Kramer-Simpson-Score for element
#' # `1` = -max(0, 1) = -1
#' #
#' # Score analogous for the other elements
#' # `2` = 0
#' # `3` = -2
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
#' @export
kramerSimpsonScores <- function(powerRelation, elements = powerRelation$elements) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #

  result <- structure(
    sapply(elements, function(p1) max(sapply(powerRelation$elements, function(p2) cpMajorityComparisonScore(powerRelation, p2, p1, TRUE)[1]))),
    names = elements
  )

  structure(-result, class = 'KramerSimpsonScores')
}

#' Kramer-Simpson-like ranking
#'
#' `kramerSimpsonRanking()` returns the corresponding ranking.
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
kramerSimpsonRanking <- function(powerRelation) {
  doRanking(kramerSimpsonScores(powerRelation))
}

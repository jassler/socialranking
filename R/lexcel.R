#' @export
`[.LexcelScores` <- function(x, i, ...) structure(unclass(x)[i], class = 'LexcelScores')

#' @export
`==.LexcelScores` <- function(a, b) {identical(a[[1]], b[[1]])}

#' @export
`>.LexcelScores` <- function(a, b) {
  a <- a[[1]]
  b <- b[[1]]
  i <- which(a != b)
  length(i) > 0 && a[i[1]] > b[i[1]]
}

#' @export
is.na.LexcelScores <- function(x) FALSE

#' Lexicographical Excellence
#'
#' Calculate the Lexicographical Excellence (or Lexcel) score.
#'
#' An equivalence class \eqn{\sum_i}{Sigma_i} contains coalitions that are indifferent to one another.
#' In a given power relation created with [`PowerRelation()`] or [`as.PowerRelation()`], the equivalence classes are saved in `$eqs`.
#'
#' As an example, consider the power relation
#' \eqn{\succeq: 123 \succ (12 \sim 13 \sim 1 \sim \emptyset) \succ (23 \sim 1 \sim 2)}{>=: 123 > (12 ~ 13 ~ 1) > (23 ~ 1 ~ 2)}.
#' The corresponding equivalence classes are:
#'
#' \deqn{
#' \sum_1 = \lbrace 123 \rbrace, \sum_2 = \lbrace 12, 13, 1, \emptyset \rbrace, \sum_3 = \lbrace 23, 1, 2 \rbrace.
#' }{
#' Sigma_1 = \{123\}, Sigma_2 = \{12, 13, 1, empty set\}, Sigma_3 = \{23, 1, 2\}.}
#'
#' The lexcel score of an element is a vector wherein each index indicates the number of times that element appears in the equivalence class.
#' From our example, we would get
#'
#' \deqn{\textrm{lexcel}(1) = [ 1, 3, 1 ], \textrm{lexcel}(2) = [ 1, 1, 2 ], \textrm{lexcel}(3) = [ 1, 1, 1 ].}{lexcel(1) = [1,3,1], \textrm{lexcel}(2) = [ 1, 1, 2 ], lexcel(3) = [1,1,1].}
#'
#' @template param/powerRelation
#' @template param/elements
#'
#' @family ranking solution functions
#'
#' @references
#' \insertRef{2019Lexcel}{socialranking}
#'
#' \insertRef{2021Lexcel}{socialranking}
#'
#' @return Score function returns a list of type `LexcelScores` and length of `powerRelation$elements`
#' (unless parameter `elements` is specified).
#' Each index contains a vector of length `powerRelation$eqs`, the number of
#' times the given element appears in each equivalence class.
#'
#' @examples
#' # note that the coalition {1} appears twice
#' # 123 > 12 ~ 13 ~ 1 ~ {} > 23 ~ 1 ~ 2
#' # E = {123} > {12, 13, 1, {}} > {23, 1, 2}
#' pr <- suppressWarnings(as.PowerRelation(
#'   "123 > (12 ~ 13 ~ 1 ~ {}) > (23 ~ 1 ~ 2)"
#' ))
#'
#' # lexcel scores for all elements
#' # `1` = c(1, 3, 1)
#' # `2` = c(1, 1, 2)
#' # `3` = c(1, 1, 1)
#' lexcelScores(pr)
#'
#' # lexcel scores for a subset of all elements
#' lexcelScores(pr, c(1, 3))
#' lexcelScores(pr, 2)
#'
#' @export
lexcelScores <- function(powerRelation, elements = powerRelation$elements) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #

  result <- list()
  for(e in elements) {
    result[[paste(e)]] <- unlist(lapply(
      powerRelation$eqs,
      function(coalition) sum(e == unlist(coalition))
    ))
  }

  structure(result, class = 'LexcelScores')
}

#' Lexcel Ranking
#'
#' `lexcelRanking()` returns the corresponding ranking.
#'
#' @section Lexcel Ranking:
#'
#' The most "excellent contribution" of an element determines its ranking against the other elements.
#' Given two Lexcel score vectors \eqn{\textrm{Score}(i)}{Score(i)}
#' and \eqn{\textrm{Score}(j)}{Score(j)}, the first index \eqn{x}{x} where
#' \eqn{\textrm{Score}(i)_x \neq \textrm{Score}(j)_x}{Score(i)_x != Score(j)_x}
#' determines which element should be ranked higher.
#'
#' From the previous example this would be \eqn{1 > 2 > 3}{1 > 2 > 3}, because:
#'
#' \eqn{\textrm{Score}(1)_2 = 3 > \textrm{Score}(2)_2 = \textrm{Score}(3)_2 = 1}{Score(1)_2 = 3 > Score(2) = Score(3) = 1},
#' \eqn{\textrm{Score}(2)_3 = 2 > \textrm{Score}(3)_3 = 1}{Score(2)_3 = 2 > Score(3)_3 = 1}.
#'
#' @template param/powerRelation
#'
#' @rdname lexcelScores
#'
#' @template return/ranking
#'
#' @examples
#' # 1 > 2 > 3
#' lexcelRanking(pr)
#'
#' @export
lexcelRanking <- function(powerRelation) {
  doRanking(lexcelScores(powerRelation))
}

#' Dual Lexcel Ranking
#'
#' `dualLexcelRanking()` uses the same score vectors but instead of rewarding
#' participation, it punishes mediocrity.
#'
#' @section Dual Lexcel Ranking:
#'
#' The dual lexcel works in reverse order and, instead of rewarding high
#' scores, punishes mediocrity. In that case we get \eqn{3 > 1 > 2}{3 > 1 > 2}
#' because:
#'
#' \eqn{\textrm{Score}(3)_3 < \textrm{Score}(2)_3}{Score(3)_3 < Score(2)_3} and
#' \eqn{\textrm{Score}(3)_2 < \textrm{Score}(1)_2}{Score(3)_2 < Score(1)_3},
#' \eqn{\textrm{Score}(1)_3 < \textrm{Score}(2)_3}{Score(1)_3 < Score(2)_3}.
#'
#' @template param/powerRelation
#'
#' @rdname lexcelScores
#'
#' @references
#' \insertRef{2021AntiLexcel}{socialranking}
#'
#' @examples
#' # 3 > 1 > 2
#' dualLexcelRanking(pr)
#'
#' @export
dualLexcelRanking <- function(powerRelation) {
  doRanking(structure(
    lapply(lexcelScores(powerRelation), function(r) -rev(r)),
    class = 'LexcelScores'
  ))
}

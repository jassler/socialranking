#' @export
`[.L1Scores` <- function(x, i, ...) structure(unclass(x)[i], class = 'LexcelScores')

#' @export
`==.L1Scores` <- function(a, b) {identical(a[[1]], b[[1]])}

#' @export
`>.L1Scores` <- function(a, b) {
  a <- a[[1]]
  b <- b[[1]]
  i <- which(a != b)
  length(i) > 0 && a[i[1]] > b[i[1]]
}

#' @export
is.na.L1Scores <- function(x) FALSE

#' L1 Scores
#'
#' Calculate the L1 (or Lexcel1) scores.
#'
#' Similar
#'
#' @template param/powerRelation
#' @template param/elements
#'
#' @family score vector functions
#'
#' @references
#' \insertRef{2021Lexcel}{socialranking}
#'
#' @return Score function returns a list of type `Lexcel1Scores` and length of `powerRelation$elements`
#' (unless parameter `elements` is specified).
#' Each index contains a vector of length `powerRelation$eqs`, the number of
#' times the given element appears in each equivalence class.
#'
#' @examples
#'
#' @export
L1Scores <- function(powerRelation, elements = NULL) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  if(is.null(elements)) elements <- powerRelation$elements
  # --- end checks --- #

  l <- list()
  for(e in elements) {
    m <- matrix(0, nrow = length(powerRelation$elements), ncol = length(powerRelation$eqs))
    for(index in powerRelation$elementLookup(e)) {
      y <- length(powerRelation$eqs[[index[1]]][[index[2]]])
      x <- index[1]
      m[y,x] <- m[y,x] + 1
    }
    l[[paste(e)]] <- m
  }

  structure(l, class = 'L1Scores')
}

# #' Lexcel Ranking
# #'
# #' [`lexcelRanking`] returns the corresponding ranking.
# #'
# #' @section Lexcel Ranking:
# #'
# #' The most "excellent contribution" of an element determines its ranking against the other elements.
# #' Given two Lexcel score vectors \eqn{\textrm{Score}(i)}{Score(i)}
# #' and \eqn{\textrm{Score}(j)}{Score(j)}, the first index \eqn{x}{x} where
# #' \eqn{\textrm{Score}(i)_x \neq \textrm{Score}(j)_x}{Score(i)_x != Score(j)_x}
# #' determines which element should be ranked higher.
# #'
# #' From the previous example this would be \eqn{1 > 2 > 3}{1 > 2 > 3}, because:
# #'
# #' \eqn{\textrm{Score}(1)_2 = 3 > \textrm{Score}(2)_2 = \textrm{Score}(3)_2 = 1}{Score(1)_2 = 3 > Score(2) = Score(3) = 1},
# #' \eqn{\textrm{Score}(2)_3 = 2 > \textrm{Score}(3)_3 = 1}{Score(2)_3 = 2 > Score(3)_3 = 1}.
# #'
# #' @template param/powerRelation
# #'
# #' @rdname lexcelScores
# #'
# #' @template return/ranking
# #'
# #' @examples
# #' # 1 > 2 > 3
# #' lexcelRanking(pr)
# #'
# #' @export
# lexcelRanking <- function(powerRelation) {
#   doRanking(lexcelScores(powerRelation))
# }
#
# #' Dual Lexcel Ranking
# #'
# #' [`dualLexcelRanking`] uses the same score vectors but instead of rewarding
# #' participation, it punishes mediocrity.
# #'
# #' @section Dual Lexcel Ranking:
# #'
# #' The dual lexcel works in reverse order and, instead of rewarding high
# #' scores, punishes mediocrity. In that case we get \eqn{3 > 1 > 2}{3 > 1 > 2}
# #' because:
# #'
# #' \eqn{\textrm{Score}(3)_3 < \textrm{Score}(2)_3}{Score(3)_3 < Score(2)_3} and
# #' \eqn{\textrm{Score}(3)_2 < \textrm{Score}(1)_2}{Score(3)_2 < Score(1)_3},
# #' \eqn{\textrm{Score}(1)_3 < \textrm{Score}(2)_3}{Score(1)_3 < Score(2)_3}.
# #'
# #' @template param/powerRelation
# #'
# #' @rdname lexcelScores
# #'
# #' @references
# #' \insertRef{2021AntiLexcel}{socialranking}
# #'
# #' @examples
# #' # 3 > 1 > 2
# #' dualLexcelRanking(pr)
# #'
# #' @export
# dualLexcelRanking <- function(powerRelation) {
#   doRanking(structure(
#     lapply(lexcelScores(powerRelation), function(r) -rev(r)),
#     class = 'LexcelScores'
#   ))
# }
#

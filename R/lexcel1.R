#' @export
`[.L1Scores` <- function(x, i, ...) structure(unclass(x)[i], class = 'L1Scores')

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

#' L1 Ranking
#'
#' Calculate the \eqn{L^{(1)}}{L^(1)} scores.
#'
#' @details
#'
#' Similar to [`lexcelRanking()`], the number of times an element appears in each equivalence class is counted.
#' In addition, we now also consider the size of the coalitions.
#'
#' @template lexcel/frequencyMatrixDoc
#'
#' @details
#' The \eqn{L^{(1)}}{L^(1)} rewards elements that appear in higher ranking coalitions as well as in smaller coalitions.
#' When comparing two matrices for a power relation, if \eqn{M^\succsim_i >_{L^{(1)}} M^\succsim_j}{M^(>=)_i > M^(>=)_j (the ^(>=) will be omitted to improve readability)},
#' this suggests that there exists a \eqn{p^0 \in \{1, \dots, |N|\}}{p^0 in \{1, ..., |N|\}} and \eqn{q^0 \in \{1, \dots, m\}}{q^0 in \{1, ..., m\}} such that the following holds:
#'
#' 1. \eqn{(M^\succsim_i)_{p^0,q^0} > (M^\succsim_j)_{p^0,q^0}}{(M_i)_(p^0 q^0) > (M_j)_(p^0 q^0)}
#' 2. \eqn{(M^\succsim_i)_{p,q^0} = (M^\succsim_j)_{p,q^0}}{(M_i)_(p q^0) = (M_j)_(p q^0)} for all \eqn{p < p^0}{p < p^0}
#' 3. \eqn{(M^\succsim_i)_{p,q} = (M^\succsim_j)_{p,q}}{(M_i)_(pq) = (M_j)_(p,q)} for all \eqn{q < q^0}{q < q^0} and \eqn{p \in \{1, \dots, |N|\}}{p in \{1, ..., |N|\}}
#'
#' @section Example:
#' Let \eqn{\succsim: (123 \sim 13 \sim 2) \succ (12 \sim 1 \sim 3) \succ (23 \sim \{\})}{>=: (123 ~ 13 ~ 2) > (12 ~ 1 ~ 3) > (23 ~ \{\})}.
#' From this, we get the following three matrices:
#'
#' \deqn{
#' M^\succsim_1 = \begin{bmatrix}
#' 0 & 1 & 0\\
#' 1 & 1 & 0\\
#' 1 & 0 & 0
#' \end{bmatrix}
#' M^\succsim_2 = \begin{bmatrix}
#' 1 & 0 & 0\\
#' 0 & 1 & 1\\
#' 1 & 0 & 0
#' \end{bmatrix}
#' M^\succsim_3 = \begin{bmatrix}
#' 0 & 1 & 0\\
#' 1 & 0 & 1\\
#' 1 & 0 & 0
#' \end{bmatrix}
#' }{
#' M^(>=)_1 = matrix(c(0,1,1,1,1,0,0,0,0),nrow=3)\\
#' M^(>=)_2 = matrix(c(1,0,1,0,1,0,0,1,0),nrow=3)\\
#' M^(>=)_3 = matrix(c(0,1,1,1,0,0,0,1,0),nrow=3)
#' }
#'
#' From \eqn{(M^\succsim_2)_{1,1} > (M^\succsim_1)_{1,1}}{(M_2)_(1,1) > (M_1)_1,1} and \eqn{(M^\succsim_2)_{1,1} > (M^\succsim_3)_{1,1}}{(M_2)_(1,1) > (M_3)_1,1} it
#' immediately follows that \eqn{2}{2} is ranked above \eqn{1}{1} and \eqn{3}{3} according to \eqn{L^{(1)}}{L^(1)}.
#'
#' Comparing \eqn{1}{1} against \eqn{3}{3} we can set \eqn{p^0 = 2}{p^0 = 2} and \eqn{q^0 = 2}{q^0 = 2}.
#' Following the constraints from the definition above, we can verify that the entire column 1 is identical.
#' In column 2, we determine that \eqn{(M^\succsim_1)_{1,q^0} = (M^\succsim_3)_{1,q^0}}{(M_1)_(1,q^0) = (M_3)_(1,q^0)}, whereas
#' \eqn{(M^\succsim_1)_{p^0,q^0} > (M^\succsim_3)_{p^0,q^0}}{(M_1)_(p^0,q^0) > (M_3)_(p^0,q^0)}, indicating that \eqn{1}{1}
#' is ranked higher than \eqn{3}{3}, hence \eqn{2 \succ 1 \succ 3}{2 > 1 > 3} according to \eqn{L^{(1)}}{L^(1)}.
#'
#' @section Aliases:
#'
#' For better discoverability, `lexcel1Scores()` and `lexcel1Ranking()` serve as aliases for `L1Scores()` and `L1Ranking()`, respectively.
#'
#' @template param/powerRelation
#' @template param/elements
#'
#' @family ranking solution functions
#'
#' @references
#' \insertRef{2021Lexcel}{socialranking}
#'
#' @return Score function returns a list of type `L1Scores` and length of `powerRelation$elements`
#' (unless parameter `elements` is specified).
#' Each index contains a vector of length `powerRelation$eqs`, the number of
#' times the given element appears in each equivalence class.
#'
#' @examples
#' pr <- as.PowerRelation("(123 ~ 13 ~ 2) > (12 ~ 1 ~ 3) > (23 ~ {})")
#' scores <- L1Scores(pr)
#' scores$`1`
#' #      [,1] [,2] [,3]
#' # [1,]    0    1    0
#' # [2,]    1    1    0
#' # [3,]    1    0    0
#'
#' L1Ranking(pr)
#' # 2 > 1 > 3
#'
#' @export
L1Scores <- function(powerRelation, elements = powerRelation$elements) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
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

#' `L1Ranking()` returns the corresponding ranking.
#'
#' @rdname L1Scores
#'
#' @template return/ranking
#'
#' @export
L1Ranking <- function(powerRelation) {
  doRanking(L1Scores(powerRelation))
}

#' @rdname L1Scores
#' @export
lexcel1Scores <- L1Scores

#' @rdname L1Scores
#' @export
lexcel1Ranking <- L1Ranking

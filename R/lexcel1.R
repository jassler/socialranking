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
#' Similar to [`lexcelRanking()`], the number of times an element appears in each equivalence class is counted.
#' In addition, we now also consider the size of the coalitions.
#'
#' Let \eqn{N}{N} be a set of elements, \eqn{\succeq \in \mathcal{T}(\mathcal{P})}{>= in T(P)} be a power relation,
#' and \eqn{\Sigma_1 \succ \Sigma_2 \succ \dots \succ \Sigma_m}{E_1 > E_2 > ... > E_m} its corresponding quotient order.
#'
#' For an element \eqn{i \in N}{i in N}, we get a matrix \eqn{M^\succeq_i}{M^(>=)_i} with \eqn{m}{m} columns and \eqn{|N|}{|N|} rows.
#' Whereas each column represents an equivalence class, each row corresponds to the coalition size.
#'
#' \deqn{(M^\succeq_i)_{pq} = |\{S \in \Sigma_q: |S| = p\}|}{(M^(>=)_i)_(pq) = |\{S in E_q: |S| = p\}}
#'
#' Take as an example \eqn{\succeq: (123 \sim 13 \sim 2) \succ (12 \sim 1 \sim 3) \succ (23 \sim \{\})}{>=: (123 ~ 13 ~ 2) > (12 ~ 1 ~ 3) > (23 ~ \{\})}.
#' From this, we get the following three matrices:
#'
#' \deqn{
#' M^\succeq_1 = \begin{bmatrix}
#' 0 & 1 & 0\\
#' 1 & 1 & 0\\
#' 1 & 0 & 0
#' \end{bmatrix}
#' M^\succeq_2 = \begin{bmatrix}
#' 1 & 0 & 0\\
#' 0 & 1 & 1\\
#' 1 & 0 & 0
#' \end{bmatrix}
#' M^\succeq_3 = \begin{bmatrix}
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
#' The \eqn{L^{(1)}}{L^(1)} then ranks the elements, rewarding elements that appear in higher ranking coalitions as well as smaller coalitions.
#' When comparing two matrices for a power relation, if \eqn{M^\succeq_i >_{L^{(1)}} M^\succeq_j}{M^(>=)_i > M^(>=)_j (the ^(>=) will be omitted to improve readability)},
#' this suggests that there exists a \eqn{p^0 \in \{1, \dots, |N|\}}{p^0 in \{1, ..., |N|\}} and \eqn{q^0 \in \{1, \dots, m\}}{q^0 in \{1, ..., m\}} such that the following holds:
#'
#' 1. \eqn{(M^\succeq_i)_{p^0q^0} > (M^\succeq_j)_{p^0q^0}}{(M_i)_(p^0 q^0) > (M_j)_(p^0 q^0)}
#' 2. \eqn{(M^\succeq_i)_{pq^0} = (M^\succeq_j)_{pq^0}}{(M_i)_(p q^0) = (M_j)_(p q^0)} for all \eqn{p < p^0}{p < p^0}
#' 3. \eqn{(M^\succeq_i)_{pq} = (M^\succeq_j)_{pq}}{(M_i)_(pq) = (M_j)_(pq)} for all \eqn{q < q^0}{q < q^0} and \eqn{p \in \{1, \dots, |N|\}}{p in \{1, ..., |N|\}}
#'
#' In the aforementioned example, we have that \eqn{M^\succeq_2 >_{L^{(1)}} M^\succeq_1 >_{L^{(1)}} M^\succeq_3}{M_2 > M_1 > M_3}, suggesting that element 2 should be ranked highest and 3 lowest.
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
#' @return Score function returns a list of type `Lexcel1Scores` and length of `powerRelation$elements`
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

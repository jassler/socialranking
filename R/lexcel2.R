#' @export
`[.L2Scores` <- function(x, i, ...) structure(unclass(x)[i], class = 'L2Scores')

#' @export
`==.L2Scores` <- function(a, b) {identical(a[[1]], b[[1]])}

#' @export
`>.L2Scores` <- function(a, b) {
  a <- a[[1]]
  b <- b[[1]]
  i <- which(a != b)
  length(i) > 0 && a[i[1]] > b[i[1]]
}

#' @export
is.na.L2Scores <- function(x) FALSE

#' L2 Ranking
#'
#' Calculate the \eqn{L^{(2)}}{L^(2)} scores.
#'
#' @template lexcel/frequencyMatrixDoc
#'
#' @details
#' Given two elements \eqn{i, j \in N}{i, j in N}, \eqn{L^{(2)}}{L^(2)} then ranks
#' \eqn{i}{i} _strictly above_ \eqn{j}{j} if there is some row
#' \eqn{p^0 \in \lbrace 1, \dots, |N| \rbrace}{p^0 in \{1, ..., |N|\}} and column
#' \eqn{q^0 \in \lbrace 1, \dots, m \rbrace}{q^0 in \{1, ..., m\}} such that
#'
#' 1. \eqn{\sum_{p = 1}^{|N|} (M^\succsim_i)_{p,q} = \sum_{p = 1}^{|N|} (M^\succsim_j)_{p,q}\text{ for all } q < q^0}{sum_(p=1)^(|N|) (M^(>=)_i)_(p,q) = sum_(p=1)^(|N|) (M^(>=)_j)_(p,q) for all q < q^0},
#' 2. \eqn{\begin{cases}
#' \text{(i)\hphantom{i} either } & \sum_{p=1}^{|N|} (M^\succsim_i)_{p,q^0} > \sum_{p=1}^{|N|} (M^\succsim_j)_{p,q^0}\\[5pt]
#' \text{(ii) or } & (M^\succsim_i)_{p^0,q^0} > (M^\succsim_j)_{p^0,q^0} \text{ and } (M^\succsim_i)_{p,q^0} = (M^\succsim_j)_{p,q^0} \text{ for all } p < p^0
#' \end{cases}}{either (i) the sum of the values in column q^0 in M^(>=)_i is greater than in M^(>=)_j, or (ii), (M^(>=)_i)_(p^0,q^0) > (M^(>=)_j)_(p^0,q^0) and every value above p^0 is the same.}
#'
#' Note that the conditions are very similar to [`L1Ranking()`], with the difference that condition 3.(i)
#' also ranks an element over another if they simply appear more often in an equivalence class, regardless of coalition size.
#' This implies that a row \eqn{p^0}{p^0} for condition 3.(ii) to be satisfied may not have to exist.
#'
#' @section Example:
#'
#' Let \eqn{N = \lbrace 1, 2, 3, 4 \rbrace}{N = \{1, 2, 3, 4\}} and \eqn{\succsim: (123 \sim 12 \sim 13 \sim 14 \sim 2 \sim 4) \succ S}{>=: (123 ~ 12 ~ 13 ~ 14 ~ 2 ~ 4) > S},
#' where \eqn{S}{S} is every other coalition not present in the first equivalence class.
#' From this, we get the following four matrices:
#'
#' \deqn{
#' M^\succsim_1 = \begin{bmatrix}
#' 0 & 1\\
#' 3 & 0\\
#' 1 & 2\\
#' 0 & 1
#' \end{bmatrix}
#' M^\succsim_2 = \begin{bmatrix}
#' 1 & 0\\
#' 1 & 2\\
#' 1 & 2\\
#' 0 & 1
#' \end{bmatrix}
#' M^\succsim_3 = \begin{bmatrix}
#' 0 & 1\\
#' 1 & 2\\
#' 1 & 2\\
#' 0 & 1
#' \end{bmatrix}
#' M^\succsim_4 = \begin{bmatrix}
#' 1 & 0\\
#' 1 & 2\\
#' 0 & 3\\
#' 0 & 1
#' \end{bmatrix}
#' }{
#' M^(>=)_1 = matrix(c(0,3,1,0,1,0,2,1),nrow=4)\\
#' M^(>=)_2 = matrix(c(1,1,1,0,0,2,2,1),nrow=4)\\
#' M^(>=)_3 = matrix(c(0,1,1,0,1,2,2,1),nrow=4)\\
#' M^(>=)_4 = matrix(c(1,1,0,0,0,2,3,1),nrow=4)
#' }
#'
#' For the sums in column 1, we get
#'
#' \deqn{\begin{aligned}\sum_{p=1}^{4} (M^\succsim_1)_{p,1} &= 4,\\\sum_{p=1}^{4} (M^\succsim_2)_{p,1} &= 3,\\\sum_{p=1}^{4} (M^\succsim_3)_{p,1} = \sum_{p=1}^{4} (M^\succsim_4)_{p,1} &= 2\end{aligned}.}{4 for M^(>=)_1, 3 for M^(>=)_2, and 2 for M^(>=)_3 and M^(>=)_4.}
#'
#' This immediately puts \eqn{1}{1} above all other elements and \eqn{2}{2} above \eqn{3}{3} and \eqn{4}{4} according to the \eqn{L^{(2)}}{L^(2)}.
#' \eqn{L^{(1)}}{L^(1)} would in this case prefer \eqn{2}{2} over \eqn{1}{1}, simply because \eqn{2}{2} appears once in a coalition of size 1 and \eqn{1}{1} doesn't.
#'
#' Since the column sum for \eqn{3}{3} and \eqn{4}{4} is the same, we can next evaluate if the individual row values are also the same.
#' Here, since \eqn{(M^\succsim_4)_{1,1} > (M^\succsim_3)_{1,1}}, this gives an edge of element \eqn{4}{4} over \eqn{3}{3}.
#'
#'
#' Note that, if the column was identical for \eqn{3}{3} and \eqn{4}{4}, we would go to the next column and repeat the process.
#' Elements are only then considered indifferent from each other, if the entire matrix is identical between the two.
#'
#' @section Alterations:
#'
#' The matrices as described above and in \insertRef{beal2022lexicographic}{socialranking} can be investigated with the [`L1Scores()`] function.
#'
#' For less complexity, another row is prepended to the matrix showing the sum of each column.
#' Through this, a simple \eqn{L^{(1)}}{L^(1)} comparison can be applied.
#'
#' @section Aliases:
#'
#' For better discoverability, `lexcel2Scores()` and `lexcel2Ranking()` serve as aliases for `L2Scores()` and `L2Ranking()`, respectively.
#'
#' @template param/powerRelation
#' @template param/elements
#'
#' @family ranking solution functions
#'
#' @references
#' \insertRef{2021Lexcel}{socialranking}
#'
#' @return Score function returns a list of type `L2Scores` and length of `powerRelation$elements`
#' (unless parameter `elements` is specified).
#' Each index contains a matrix with `length(powerRelation$eqs)` columns and `1 + length(powerRelation$elements)` rows.
#'
#' @examples
#' pr <- as.PowerRelation("123 ~ 12 ~ 13 ~ 14 ~ 2 ~ 4")
#' pr <- appendMissingCoalitions(pr)
#' scores <- L2Scores(pr)
#' scores$`1`
#' #      [,1] [,2]
#' # [1,]    0    1
#' # [2,]    3    0
#' # [3,]    1    2
#' # [3,]    0    1
#'
#' L2Ranking(pr)
#' # 1 > 2 > 4 > 3
#'
#' L1Ranking(pr)
#' # 2 > 4 > 1 > 3
#'
#' @export
L2Scores <- function(powerRelation, elements = powerRelation$elements) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #

  l <- L1Scores(powerRelation, elements)
  l <- lapply(l, function(m) rbind(apply(m, 2, sum), m))
  class(l) <- 'L2Scores'
  return(l)
}

#' `L2Ranking()` returns the corresponding ranking.
#'
#' @rdname L2Scores
#'
#' @template return/ranking
#'
#' @export
L2Ranking <- function(powerRelation) {
  doRanking(L2Scores(powerRelation))
}

#' @rdname L2Scores
#' @export
lexcel2Scores <- L2Scores

#' @rdname L2Scores
#' @export
lexcel2Ranking <- L2Ranking

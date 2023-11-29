#' @export
`[.LP*Scores` <- function(x, i, ...) structure(unclass(x)[i], class = 'LP*Scores')

#' @export
`==.LP*Scores` <- function(a, b) {identical(a[[1]], b[[1]])}

#' @export
`>.LP*Scores` <- function(a, b) {
  a <- a[[1]]
  b <- b[[1]]
  if(ncol(a) != ncol(b)) {
    return(ncol(a) < ncol(b))
  }
  i <- which(a != b)
  length(i) > 0 && a[i[1]] > b[i[1]]
}

#' @export
`is.na.LP*Scores` <- function(x) FALSE

#' LP* Ranking
#'
#' Calculate the \eqn{L^{p^*}}{L^p*} scores.
#'
#' @template lexcel/frequencyMatrixDoc
#' @details
#' For \eqn{i, j \in N}{i, j in N}, the social ranking solution \eqn{L^{p^*}}{L^p*} then ranks \eqn{i} strictly above \eqn{j} if one of the following conditions hold:
#'
#' 1. \eqn{\lbrace i \rbrace \succ \lbrace j \rbrace}{\{i\} > \{j\}};
#' 2. \eqn{\lbrace i \rbrace, \lbrace j \rbrace \in \Sigma_k}{\{i\}, \{j\} in E_k} and there exists a row \eqn{p_0 \in \lbrace 2, \dots, |N|\rbrace}{p_0 in \{2, ..., |N|-1\}} and column \eqn{q_0 \in \lbrace 1, \dots, k-1\rbrace}{q_0 in \{1, ..., k-1\}} such that:
#' \deqn{(M^\succsim_i)_{p,q} = (M^\succsim_j)_{p,q}\quad \forall p < p_0, q < k,}{(M^(>=)_i)_(p,q) = (M^(>=)_j)_(p,q) for all p < p_0, q < k,}
#' \deqn{(M^\succsim_i)_{p_0,q} = (M^\succsim_j)_{p_0,q}\quad \forall q < q_0,\text{ and}}{(M^(>=)_i)_(p_0,q) = (M^(>=)_j)_(p_0,q) for all q < q_0, and}
#' \deqn{(M^\succsim_i)_{p_0,q_0} > (M^\succsim_j)_{p_0,q_0}.}{(M^(>=)_i)_(p_0,q_0) > (M^(>=)_j)_(p_0,q_0).}
#'
#' @section Example:
#'
#' Let \eqn{\succsim: (123 \sim 12 \sim 2) \succ (13 \sim 23) \succ (1 \sim 3 \sim \{\})}{>=: (123 ~ 13 ~ 2) > (12 ~ 23) > (1 ~ 3 ~ \{\})}.
#' From this, we get the following three matrices:
#'
#' \deqn{
#' M^\succsim_1 = \begin{bmatrix}
#' 0 & 0 & 1\\
#' 1 & 1 & 0\\
#' 1 & 0 & 0
#' \end{bmatrix}
#' M^\succsim_2 = \begin{bmatrix}
#' 1 & 0 & 0\\
#' 1 & 0 & 1\\
#' 1 & 0 & 0
#' \end{bmatrix}
#' M^\succsim_3 = \begin{bmatrix}
#' 0 & 0 & 1\\
#' 0 & 2 & 0\\
#' 1 & 0 & 0
#' \end{bmatrix}
#' }{
#' M^(>=)_1 = matrix(c(0,1,1,0,1,0,1,0,0),nrow=3)\\
#' M^(>=)_2 = matrix(c(1,1,1,0,0,0,0,1,0),nrow=3)\\
#' M^(>=)_3 = matrix(c(0,0,1,0,2,0,1,0,0),nrow=3)
#' }
#'
#' \eqn{(M^\succsim_2)_{2,3}}{(M^(>=)_2)_(2,3)} in this context refers to the value in the second row and third column of element 2, in this case \eqn{1}{1}.
#'
#' In the example, \eqn{2}{2} will be immediately put above \eqn{1}{1} and \eqn{3}{3} because \eqn{\lbrace 2 \rbrace \succ \lbrace 1 \rbrace}{\{2\} > \{1\}} and \eqn{\lbrace 2 \rbrace \succ \lbrace 3 \rbrace}{\{2\} > \{3\}}.
#' Since \eqn{\lbrace 1 \rbrace \sim \lbrace 3 \rbrace}{\{1\} ~ \{3\}}, we next consider the coalitions of size 2. Here, it turns out that \eqn{(M^\succsim_1)_{2,1} = 1 > 0 = (M^\succsim_3)_{2,1}}{(M^(>=)_1)_(2,1) = 1 > 0 = (M^(>=)_3)_(2,1)},
#' setting \eqn{3}{3} to be the least preferred option (this is opposed to the \eqn{L^p}{L^p} relation, which has no strict preference of \eqn{1}{1} over \eqn{3}{3}).
#'
#' As alluded to, \eqn{L^{p^*}}{L^p*} is similar to \eqn{L^p}{L^p}, [`LPRanking()`], in that it first considers the singleton coalitions, then sequentially every coalition of size 2 and above that ranks better than the corresponding singleton.
#' It can be assumed, however, that \eqn{L^{p^*}}{L^p*} is more granular, as it doesn't throw away any information about _which_ equivalence class these bigger coalitions belong to.
#'
#' @section Alterations:
#'
#' The matrices as described above and in \insertRef{beal2022lexicographic}{socialranking} can be investigated with the [`L1Scores()`] function.
#'
#' `LPSScores()` discards some redundant information, most notably all columns from each element's singleton class and the ones thereafter.
#' The first row is also removed, as all values there are guaranteed to be 0.
#'
#' For the example above, this would actually result in the matrices
#'
#' \preformatted{
#' matrix(c(1,1, 1,0), nrow=2)
#' matrix(numeric(), nrow=2)
#' matrix(c(0,1, 2,0), nrow=2)
#' }
#'
#' @section Aliases:
#'
#' For better discoverability, `lexcelPSScores()` and `lexcelPSRanking()` serve as aliases for `LPSScores()` and `LPSRanking()`, respectively.
#'
#' @template param/powerRelation
#' @template param/elements
#'
#' @family ranking solution functions
#'
#' @references
#' \insertRef{beal2022lexicographic}{socialranking}
#'
#' @return Score function returns a list of type `LP*Scores` and length of `powerRelation$elements`
#' (unless parameter `elements` is specified).
#' Each index contains a matrix with `length(powerRelation$elements)` rows and a variable number of columns, depending on the equivalence class index containing the singleton coalition of that element (matrix can have 0 columns).
#'
#' @examples
#' pr <- as.PowerRelation("(123 ~ 12 ~ 2) > (13 ~ 23) > (1 ~ 3 ~ {})")
#' scores <- LPSScores(pr)
#' scores$`1`
#' #      [,1] [,2]
#' # [1,]    1    1
#' # [2,]    1    0
#'
#' scores$`2`
#' #
#' # [1,]
#' # [2,]
#'
#' LPSRanking(pr)
#' # 2 > 1 > 3
#'
#' @export
LPSScores <- function(powerRelation, elements = powerRelation$elements) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #

  res <- L1Scores(powerRelation, elements)
  cols <- sapply(res, function(m) which(m[1,] > 0)[1])
  structure(
    lapply(seq_along(res), function(i) if(is.na(cols[i])) res[[i]][-1,] else res[[i]][-1,-(cols[i]:ncol(res[[i]])), drop=FALSE]),
    names=names(res),
    class='LP*Scores'
  )
}


#' `LPSRanking()` returns the corresponding ranking.
#'
#' @rdname LPSScores
#'
#' @template return/ranking
#'
#' @export
LPSRanking <- function(powerRelation) {
  doRanking(LPSScores(powerRelation))
}

#' @rdname LPSScores
#' @export
lexcelPSScores <- LPSScores

#' @rdname LPSScores
#' @export
lexcelPSRanking <- LPSRanking












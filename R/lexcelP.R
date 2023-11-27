#' @export
`[.LPScores` <- function(x, i, ...) structure(unclass(x)[i], class = 'LPScores')

#' @export
`==.LPScores` <- function(a, b) {identical(a[[1]], b[[1]])}

#' @export
`>.LPScores` <- function(a, b) {
  a <- a[[1]]
  b <- b[[1]]
  if(a[1] != b[1]) {
    return(a[1] < b[1])
  }
  i <- which(a != b)
  length(i) > 0 && a[i[1]] > b[i[1]]
}

#' @export
is.na.LPScores <- function(x) FALSE

#' LP Ranking
#'
#' Calculate the \eqn{L^{p}}{L^p} scores.
#'
#' @template lexcel/frequencyMatrixDoc
#' @details
#' For \eqn{i, j \in N}{i, j in N}, the social ranking solution \eqn{L^p}{L^p} then ranks \eqn{i} strictly above \eqn{j} if one of the following conditions hold:
#'
#' 1. \eqn{\lbrace i \rbrace \succ \lbrace j \rbrace}{\{i\} > \{j\}};
#' 2. \eqn{\lbrace i \rbrace, \lbrace j \rbrace \in \Sigma_k}{\{i\}, \{j\} in E_k} and there exists a row \eqn{p_0 \in \lbrace 2, \dots, |N|\rbrace}{p_0 in \{2, ..., |N|-1\}} such that:
#' \deqn{\sum_{q < k} (M^\succsim_i)_{p,q} = \sum_{q < k} (M^\succsim_j)_{p,q}\quad \forall p < p_0,\text{ and}}{sum_(q < k) M^(>=,i)_(p,q) = sum_(q < k) M^(>=,j)_(p,q) for all p < p_0, and}
#' \deqn{\sum_{q < k} (M^\succsim_i)_{p_0,q} > \sum_{q < k} (M^\succsim_j)_{p_0,q}.}{sum_(q < k) M^(>=,i)_(p_0,q) = sum_(q < k) M^(>=,j)_(p_0,q).}
#'
#' In `R`, given two matrices `M_i` and `M_j`, this comparison could be expressed as
#'
#' \preformatted{
#' # function that returns TRUE if i should be ranked strictly above j
#' k_i <- which(M_i[1,] == 1)
#' k_j <- which(M_j[1,] == 1)
#' if(k_i != k_j) return(k_i < k_j)
#' if(k_i == 1)   return(FALSE)
#' # get sum for each row
#' # removing the first row implies that we start in row 2
#' sums_i <- apply(M_i[-1,seq(k_i-1)], 1, sum)
#' sums_j <- apply(M_j[-1,seq(k_j-1)], 1, sum)
#' # apply lexcel comparison
#' i <- which(a != b)
#' return(length(i) > 0 && a[i[1]] > b[i[1]])
#' }
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
#' Since \eqn{\lbrace 1 \rbrace \sim \lbrace 3 \rbrace}{\{1\} ~ \{3\}}, we next consider the coalitions of size 2. Here, it turns out that \eqn{(M^\succsim_1)_{2,1} + (M^\succsim_1)_{2,2} = 1 + 1}{(M^(>=)_1)_(2,1) + (M^(>=)_1)_(2,2) = 1 + 1}
#' is equal to \eqn{(M^\succsim_3)_{2,1} + (M^\succsim_3)_{2,2} = 0 + 2}{(M^(>=)_3)_(2,1) + (M^(>=)_3)_(2,2) = 0 + 2}.
#' For obvious reasons the grand coalition does not have to be considered, thus \eqn{1}{1} and \eqn{3}{3} are considered equally powerful by the \eqn{L^p}{L^p} solution.
#'
#' \eqn{L^{p}}{L^p} is a social ranking solution belonging to the family of lexicographical ranking functions.
#' While related to [`L1Ranking()`], it incorporates the property of "standardness", stating that if the
#' singleton coalition \eqn{\lbrace i\rbrace \succ \lbrace j\rbrace}{\{i\} > \{j\}}, then the ranking solution
#' should also prefer \eqn{i}{i} over \eqn{j}{j}.
#'
#' If \eqn{\lbrace i\rbrace \sim \lbrace j\rbrace}{\{i\} ~ \{j\}}, then all coalitions from size 2 and upward are inspected,
#' giving higher precedence to coalitions with a lower number of elements.
#' While this preference is similar to the \eqn{L^{(1)}}{L^1}, it differs in two notable ways:
#'
#' 1. If \eqn{\lbrace i\rbrace, \lbrace j\rbrace \in \Sigma_k}{\{i\},\{k\} in E_k}, then only coalitions
#' \eqn{S \succsim (\lbrace i \rbrace \sim \lbrace j \rbrace)}{S > (\{i\} ~ \{j\})} are considered,
#' 2. From this subset of coalitions, consider the total number of coalitions \eqn{i}{i} (or \eqn{j}{j}) belongs to, given each coalition size.
#' This may ignore information about the distribution of these coalitions within the different equivalence classes,
#' which \eqn{L^{(1)}}{L^(1)} and the slight variation \eqn{L^{p^*}}{L^p*} of the \eqn{L^p}{L^p} solution take into account.
#'
#' @section Alterations:
#'
#' The matrices as described above and in \insertRef{beal2022lexicographic}{socialranking} can be investigated with the [`L1Scores()`] function.
#'
#' For efficiency, `LPScores()` discards much of the redundant information.
#' Instead of a matrix for each element, it returns a vector of size \eqn{|N|}{|N|}.
#'
#' Given a score vector `v` for an element `i`, `v[1]` is the position of the singleton coalition `{i}`.
#' This implies that if `v[1] < w[1]`, where `w` is the score vector of an element `j`, then `i` is ranked strictly above `j`.
#'
#' `v[2]`, `v[3]`, ..., `v[n]` then indicates the number of coalitions of size `2`, `3`, ..., `n` that the element `i` appears in.
#'
#' @section Aliases:
#'
#' For better discoverability, `lexcelPScores()` and `lexcelPRanking()` serve as aliases for `LPScores()` and `LPRanking()`, respectively.
#'
#' @template param/powerRelation
#' @template param/elements
#'
#' @family ranking solution functions
#'
#' @references
#' \insertRef{beal2022lexicographic}{socialranking}
#'
#' @return Score function returns a list of type `LPScores` and length of `powerRelation$elements`
#' (unless parameter `elements` is specified).
#' Each index contains a vector of length `length(powerRelation$elements)`.
#'
#' @examples
#' pr <- as.PowerRelation("(123 ~ 13 ~ 2) > (12 ~ 1 ~ 3) > (23 ~ {})")
#' scores <- LPScores(pr)
#' scores$`2`
#' # [1] 1 0 0
#'
#' LPRanking(pr)
#' # 2 > 1 ~ 3
#'
#' # Since L^(1) also the relation {1,2}, which ranks above {2,3}, it will place 1 above 3
#' L1Ranking(pr)
#' # 2 > 1 > 3
#'
#' @export
LPScores <- function(powerRelation, elements = powerRelation$elements) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #

  elements <- paste(elements)
  res <- structure(
    lapply(rep(0, length(elements)), rep, length(powerRelation$elements)),
    names = elements
  )

  for(el in elements) {
    pi <- powerRelation$coalitionLookup(el)
    res[[el]][1] <- pi
    for(coalIn in powerRelation$elementLookup(el)) {
      if(coalIn[1] >= pi) {
        next
      }
      s <- length(powerRelation$eqs[[coalIn[1]]][[coalIn[2]]])
      res[[el]][s] <- res[[el]][s] + 1
    }
  }

  structure(res, class = 'LPScores')
}


#' `LPRanking()` returns the corresponding ranking.
#'
#' @rdname LPScores
#'
#' @template return/ranking
#'
#' @export
LPRanking <- function(powerRelation) {
  doRanking(LPScores(powerRelation))
}

#' @rdname LPScores
#' @export
lexcelPScores <- LPScores

#' @rdname LPScores
#' @export
lexcelPRanking <- LPRanking












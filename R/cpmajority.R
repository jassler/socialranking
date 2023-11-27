#' CP-Majority relation
#'
#' The Ceteris Paribus-majority relation compares the relative success between two players joining a coalition.
#'
#' Given two elements \eqn{i}{i} and \eqn{j}{j}, go through each coalition \eqn{S \in 2^{N \setminus \lbrace i, j \rbrace}}{S in 2^(N - \{i,j\})}.
#' \eqn{D_{ij}(\succsim)}{D_ij(>=)} then contains all coalitions \eqn{S}{S} where
#' \eqn{S \cup \lbrace i \rbrace \succsim S \cup \lbrace j \rbrace}{S u \{i\} >= S u \{j\}} and \eqn{D_{ji}(\succsim)}{D_ji(>=)} contains all coalitions where
#' \eqn{S \cup \lbrace j \rbrace \succsim S \cup \lbrace i \rbrace}{S u \{j\} >= S u \{i\}}.
#'
#' The cardinalities
#' \eqn{d_{ij}(\succsim) = |D_{ij}|}{d_ij(>=) = |D_ij|} and
#' \eqn{d_{ji}(\succsim) = |D_{ji}|}{d_ij(>=) = |D_ji|} represent the score of the two elements, where
#' \eqn{i \succ j}{i > j}    if \eqn{d_{ij}(\succsim)   >  d_{ji}(\succsim)}{d_ij(>=) >  d_ji(>=)} and
#' \eqn{i \sim  j}{i ~ j}    if \eqn{d_{ij}(\succsim)  ==  d_{ji}(\succsim)}{d_ij(>=) == d_ji(>=)}.
#'
#' [`cpMajorityComparison()`] tries to retain all that information. The list returned contains the following information.
#' Note that in this context the two elements \eqn{i}{i} and \eqn{j}{j} refer to element 1 and element 2 respectively.
#'
#' * `$e1`: list of information about element 1
#'   * `$e1$name`: name of element 1
#'   * `$e1$score`: score \eqn{d_{ij}(\succsim)}{d_ij(>=)}. \eqn{d_{ij}(\succ)}{d_ij(>)} if `strictly == TRUE`
#'   * `$e1$winningCoalitions`: list of coalition [`vectors`][base::c()] \eqn{S \in D_{ij}(\succsim)}{S in D_ij(>=)}. \eqn{S \in D_{ij}(\succ)}{S in D_ij(>)} if `strictly == TRUE`
#' * `$e2`: list of information about element 2
#'   * `$e2$name`: name of element 2
#'   * `$e1$score`: score \eqn{d_{ji}(\succsim)}{d_ji(>=)}. \eqn{d_{ji}(\succ)}{d_ji(>)} if `strictly == TRUE`
#'   * `$e1$winningCoalitions`: list of coalition [`vectors`][base::c()] \eqn{S \in D_{ji}(\succsim)}{S in D_ji(>=)}.  \eqn{S \in D_{ji}(\succ)}{S in D_ji(>)} if `strictly == TRUE`
#' * `$winner`: name of higher scoring element. `NULL` if they are indifferent.
#' * `$loser`: name of lower scoring element. `NULL` if they are indifferent.
#' * `$tuples`: a list of coalitions \eqn{S \in 2^{N \setminus \lbrace i, j \rbrace }}{S in 2^(N - \{i,j\})} with:
#'   * `$tuples[[x]]$coalition`: [`vector`][base::c()], the coalition \eqn{S}{S}
#'   * `$tuples[[x]]$included`: logical, `TRUE` if \eqn{S \cup \lbrace i \rbrace}{Su\{i\}} and \eqn{S \cup \lbrace j \rbrace}{Su\{j\}} are in the power relation
#'   * `$tuples[[x]]$winner`: name of the winning element \eqn{i}{i} where \eqn{S \cup \lbrace i \rbrace \succ S \cup \lbrace j \rbrace}{S u \{i\} > S u \{j\}}. It is `NULL` if \eqn{S \cup \lbrace i \rbrace \sim S \cup \lbrace j \rbrace}{S u \{i\} ~ S u \{j\}}
#'   * `$tuples[[x]]$e1`: index \eqn{x_1}{x_1} at which \eqn{S \cup \lbrace i \rbrace \in \sum_{x_1}}{S u \{i\} in Sum_(x_1)}
#'   * `$tuples[[x]]$e2`: index \eqn{x_2}{x_2} at which \eqn{S \cup \lbrace j \rbrace \in \sum_{x_2}}{S u \{j\} in Sum_(x_2)}
#'
#' The much more efficient [`cpMajorityComparisonScore()`] only calculates `$e1$score`.
#'
#' Unlike Lexcel, Ordinal Banzhaf, etc., this power relation can introduce cycles. For this reason the function
#' [`cpMajorityComparison()`] and [`cpMajorityComparisonScore()`] only offers direct comparisons between two elements
#' and not a ranking of all players. See the other CP-majority based functions that offer a way to rank all players.
#'
#' @template param/powerRelation
#' @template param/e1and2
#' @param strictly Only include \eqn{D_{ij}(\succ)}{D_ij(>)} and \eqn{D_{ji}(\succ)}{D_ji(>)}, i.e., coalitions
#' \eqn{S \in 2^{N \setminus \lbrace i,j\rbrace}}{S in 2^(N-{i,j})} where
#' \eqn{S \cup \lbrace i\rbrace \succ S \cup \lbrace j\rbrace}{Sui > Suj} and
#' vice versa.
#' @template param/includeEmptySet
#'
#' @family CP-majority based functions
#'
#' @references
#' \insertRef{2018CPMajority}{socialranking}
#'
#' \insertRef{2018CPMajoritySims}{socialranking}
#'
#' @return `cpMajorityComparison()` returns a list with elements described in the details.
#'
#' @examples
#' pr <- as.PowerRelation("ac > (a ~ b) > (c ~ bc)")
#'
#' scores <- cpMajorityComparison(pr, "a", "b")
#' scores
#' # a > b
#' # D_ab = {c, {}}
#' # D_ba = {{}}
#' # Score of a = 2
#' # Score of b = 1
#'
#' stopifnot(scores$e1$name == "a")
#' stopifnot(scores$e2$name == "b")
#' stopifnot(scores$e1$score == 2)
#' stopifnot(scores$e2$score == 1)
#' stopifnot(scores$e1$score == length(scores$e1$winningCoalitions))
#' stopifnot(scores$e2$score == length(scores$e2$winningCoalitions))
#'
#' # get tuples with coalitions S in 2^(N - \{i,j\})
#' emptySetTuple <- Filter(function(x) identical(x$coalition, c()), scores$tuples)[[1]]
#' playerCTuple  <- Filter(function(x) identical(x$coalition, "c"), scores$tuples)[[1]]
#'
#' # because {}u{a} ~ {}u{b}, there is no winner
#' stopifnot(is.null(emptySetTuple$winner))
#' stopifnot(emptySetTuple$e1 == emptySetTuple$e2)
#'
#' # because {c}u{a} > {c}u{b}, player "a" gets the score
#' stopifnot(playerCTuple$winner == "a")
#' stopifnot(playerCTuple$e1 < playerCTuple$e2)
#' stopifnot(playerCTuple$e1 == 1L)
#' stopifnot(playerCTuple$e2 == 3L)
#'
#' @export
cpMajorityComparison <- function(powerRelation, e1, e2, strictly = FALSE, includeEmptySet = TRUE) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  stopifnot(e1 %in% powerRelation$elements)
  stopifnot(class(e1) == class(powerRelation$elements))
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  result <- list(
    tuples = list(),
    e1 = list(
      name = e1,
      winningCoalitions = list(),
      score = 0
    ),
    e2 = list(
      name = e2,
      winningCoalitions = list(),
      score = 0
    ),
    winner = c(),
    loser = c()
  )
  class(result) <- c('cpMajority', class(powerRelation)[-1])

  # 2^(N-{i,j})
  coalitions <- createPowerset(setdiff(powerRelation$elements, c(e1,e2)), includeEmptySet = includeEmptySet)

  result$tuples <- lapply(
    coalitions,
    function(S) {
      t <- list(
        coalition = S,
        included = FALSE,
        winner = NULL,
        e1 = -1,
        e2 = -1
      )

      eq1 <- powerRelation$coalitionLookup(c(S, e1))
      eq2 <- powerRelation$coalitionLookup(c(S, e2))
      if(!is.null(eq1) && !is.null(eq2)) {
        t$included <- TRUE
        t$e1 <- eq1
        t$e2 <- eq2

        if(t$e1 < t$e2) {
          t$winner <- e1
        } else if(t$e2 < t$e1) {
          t$winner <- e2
        }
      }
      return(t)
    }
  )

  # D_ij | D_ji
  result$e1$winningCoalitions <- lapply(
    Filter(
      function(x) x$included && ifelse(strictly, x$e1<x$e2, x$e1<=x$e2),
      result$tuples
    ),
    function(x) x$coalition
  )

  result$e2$winningCoalitions <- lapply(
    Filter(
      function(x) x$included && ifelse(strictly, x$e2<x$e1, x$e2<=x$e1),
      result$tuples
    ),
    function(x) x$coalition
  )

  # d_ij | d_ji
  result$e1$score <- length(result$e1$winningCoalitions)
  result$e2$score <- length(result$e2$winningCoalitions)

  # iRj
  if(result$e1$score > result$e2$score) {
    result$winner <- e1
    result$loser <- e2

  } else if(result$e1$score < result$e2$score) {
    result$winner <- e2
    result$loser <- e1
  }

  return(result)
}

#' CP-Majority score
#'
#' `cpMajorityComparisonScore()` only returns two numbers, a positive number of coalitions where `e1` beats `e2`,
#' and a negative number of coalitions where `e1` is beaten by `e2`.
#'
#' @rdname cpMajorityComparison
#'
#' @return `cpMajorityComparisonScore()` returns a vector of two numbers, a positive number of coalitions where `e1` beats `e2`
#' (\eqn{d_{ij}(\succsim)}{d_ij(>=)}), and a negative number of coalitions where `e1` is beaten by `e2` (\eqn{-d_{ji}(\succsim)}{-d_ji(>=)}).
#'
#' @examples
#' cpMajorityComparisonScore(pr, "a", "b") # c(1,0)
#' cpMajorityComparisonScore(pr, "b", "a") # c(0,-1)
#'
#' @export
cpMajorityComparisonScore <- function(powerRelation, e1, e2, strictly = FALSE, includeEmptySet = TRUE) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  stopifnot(e1 %in% powerRelation$elements)
  stopifnot(e2 %in% powerRelation$elements)
  # --- end checks --- #

  if(e1 == e2) {
    if(strictly) {
      return(c(0,0))
    } else {
      n <- 2^(length(powerRelation$elements)-1)
      return(c(n,-n))
    }
  }

  # 2^(N-{i,j})
  coalitions <- createPowerset(setdiff(powerRelation$elements, c(e1,e2)), includeEmptySet = includeEmptySet)

  pos <- 0
  neg <- 0

  for(S in coalitions) {
    c1 <- powerRelation$coalitionLookup(c(S, e1))
    if(is.null(c1)) next

    c2 <- powerRelation$coalitionLookup(c(S, e2))
    if(is.null(c2)) next

    if(strictly) {
      if(c1 < c2) pos <- pos + 1
      else if(c1 > c2) neg <- neg - 1
    } else {
      if(c1 <= c2) pos <- pos + 1
      if(c1 >= c2) neg <- neg - 1
    }
  }

  return(c(pos, neg))
}

#' @export
print.cpMajority <- function(x, ...) {
  if(length(x$winner) == 1)
    cat(x$winner, ' > ', x$loser, sep = '')
  else
    cat(x$e1$name, '~', x$e2$name)
  cat('\n')

  pCoalitions <- if('SingleCharElements' %in% class(x)) function(cs) {
    isFirst <- T
    for(coal in cs) {
      if(!isFirst) cat(', ')
      else isFirst <- F

      if(length(coal) == 0) cat('{}')
      else cat(paste(coal), sep = '')
    }
  } else function(cs) {
    isFirst <- T
    for(coal in cs) {
      if(!isFirst) cat(', ')
      else isFirst <- F
      cat('{')
      cat(paste(coal), sep = ', ')
      cat('}')
    }
  }

  cat('D_', x$e1$name, sep = '')
  if(!('SingleCharElements' %in% class(x)))
    cat(',')
  cat(x$e2$name, ' = {', sep = '')
  pCoalitions(x$e1$winningCoalitions)
  cat('}\n')


  cat('D_', x$e2$name, sep = '')
  if(!('SingleCharElements' %in% class(x)))
    cat(',')
  cat(x$e1$name, ' = {', sep = '')
  pCoalitions(x$e2$winningCoalitions)
  cat('}')
  cat('\nScore of', x$e1$name, '=', x$e1$score)
  cat('\nScore of', x$e2$name, '=', x$e2$score)
  cat('\n')
}

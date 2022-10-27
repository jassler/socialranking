#' CP-Majority relation
#'
#' The Ceteris Paribus-majority relation compares the relative success between two players joining a coalition.
#'
#' \loadmathjax
#' Given two elements \mjseqn{i} and \mjseqn{j}, go through each coalition \mjeqn{S \in 2^{N \setminus \lbrace i, j \rbrace}}{S in 2^(N - \{i,j\})}.
#' \mjeqn{D_{ij}(\succeq)}{D_ij(>=)} then contains all coalitions \mjseqn{S} where
#' \mjeqn{S \cup \lbrace i \rbrace \succeq S \cup \lbrace j \rbrace}{S u \{i\} >= S u \{j\}} and \mjeqn{D_{ji}(\succeq)}{D_ji(>=)} contains all coalitions where
#' \mjeqn{S \cup \lbrace j \rbrace \succeq S \cup \lbrace i \rbrace}{S u \{j\} >= S u \{i\}}.
#'
#' The cardinalities
#' \mjeqn{d_{ij}(\succeq) = |D_{ij}|}{d_ij(>=) = |D_ij|} and
#' \mjeqn{d_{ji}(\succeq) = |D_{ji}|}{d_ij(>=) = |D_ji|} represent the score of the two elements, where
#' \mjeqn{i \succ j}{i > j}    if \mjeqn{d_{ij}(\succeq)   >  d_{ji}(\succeq)}{d_ij(>=) >  d_ji(>=)} and
#' \mjeqn{i \sim  j}{i ~ j}    if \mjeqn{d_{ij}(\succeq)  ==  d_{ji}(\succeq)}{d_ij(>=) == d_ji(>=)}.
#'
#' [`cpMajorityComparison()`] tries to retain all that information. The list returned contains the following information.
#' Note that in this context the two elements \mjseqn{i} and \mjseqn{j} refer to element 1 and element 2 respectively.
#'
#' * `$e1`: list of information about element 1
#'   * `$e1$name`: name of element 1
#'   * `$e1$score`: score \mjeqn{d_{ij}(\succeq)}{d_ij(>=)}. \mjeqn{d_{ij}(\succ)}{d_ij(>)} if `strictly == TRUE`
#'   * `$e1$winningCoalitions`: list of coalition [`sets::set`]s \mjeqn{S \in D_{ij}(\succeq)}{S in D_ij(>=)}. \mjeqn{S \in D_{ij}(\succ)}{S in D_ij(>)} if `strictly == TRUE`
#' * `$e2`: list of information about element 2
#'   * `$e2$name`: name of element 2
#'   * `$e1$score`: score \mjeqn{d_{ji}(\succeq)}{d_ji(>=)}. \mjeqn{d_{ji}(\succ)}{d_ji(>)} if `strictly == TRUE`
#'   * `$e1$winningCoalitions`: list of coalition [`sets::set`]s \mjeqn{S \in D_{ji}(\succeq)}{S in D_ji(>=)}.  \mjeqn{S \in D_{ji}(\succ)}{S in D_ji(>)} if `strictly == TRUE`
#' * `$winner`: name of higher scoring element. `NULL` if they are indifferent.
#' * `$loser`: name of lower scoring element. `NULL` if they are indifferent.
#' * `$tuples`: a list of coalitions \mjeqn{S \in 2^{N \setminus \lbrace i, j \rbrace }}{S in 2^(N - \{i,j\})} with:
#'   * `$tuples[[x]]$coalition`: [`sets::set`], the coalition \mjseqn{S}
#'   * `$tuples[[x]]$included`: logical, `TRUE` if \mjeqn{S \cup \lbrace i \rbrace}{Su\{i\}} and \mjeqn{S \cup \lbrace j \rbrace}{Su\{j\}} are in the power relation
#'   * `$tuples[[x]]$winner`: name of the winning element \mjseqn{i} where \mjeqn{S \cup \lbrace i \rbrace \succ S \cup \lbrace j \rbrace}{S u \{i\} > S u \{j\}}. It is `NULL` if \mjeqn{S \cup \lbrace i \rbrace \sim S \cup \lbrace j \rbrace}{S u \{i\} ~ S u \{j\}}
#'   * `$tuples[[x]]$e1`: index \mjseqn{x_1} at which \mjeqn{S \cup \lbrace i \rbrace \in \sum_{x_1}}{S u \{i\} in Sum_(x_1)}
#'   * `$tuples[[x]]$e2`: index \mjseqn{x_2} at which \mjeqn{S \cup \lbrace j \rbrace \in \sum_{x_2}}{S u \{j\} in Sum_(x_2)}
#'
#' The much more efficient [`cpMajorityComparisonScore()`] only calculates `$e1$score`.
#'
#' Unlike Lexcel, Ordinal Banzhaf, etc., this power relation can introduce cycles. For this reason the function
#' [`cpMajorityComparison()`] and [`cpMajorityComparisonScore()`] only offers direct comparisons between two elements
#' and not a ranking of all players. See the other CP-majority based functions that offer a way to rank all players.
#'
#' @template param/powerRelation
#' @template param/e1and2
#' @param strictly Only include \mjeqn{D_{ij}(\succ)}{D_ij(>)} and \mjeqn{D_{ji}(\succ)}{D_ji(>)}, i.e., coalitions
#' \mjeqn{S \in 2^{N \setminus \lbrace i,j\rbrace}}{S in 2^(N-{i,j})} where
#' \mjeqn{S \cup \lbrace i\rbrace \succ S \cup \lbrace j\rbrace}{Sui > Suj} and
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
#' pr <- newPowerRelationFromString("ac > (a ~ b) > (c ~ bc)")
#'
#' # a > b
#' # D_ab = {c, {}}
#' # D_ba = {{}}
#' # Score of a = 2
#' # Score of b = 1
#' scores <- cpMajorityComparison(pr, "a", "b")
#' stopifnot(scores$e1$name == "a")
#' stopifnot(scores$e2$name == "b")
#' stopifnot(scores$e1$score == 2)
#' stopifnot(scores$e2$score == 1)
#' stopifnot(scores$e1$score == length(scores$e1$winningCoalitions))
#' stopifnot(scores$e2$score == length(scores$e2$winningCoalitions))
#'
#' # get tuples with coalitions S in 2^(N - \{i,j\})
#' emptySetTuple <- Filter(function(x) x$coalition == sets::set(), scores$tuples)[[1]]
#' playerCTuple  <- Filter(function(x) x$coalition == sets::set("c"), scores$tuples)[[1]]
#'
#' # because {} u a ~ {} u b, there is no winner
#' stopifnot(is.null(emptySetTuple$winner))
#' stopifnot(emptySetTuple$e1 == emptySetTuple$e2)
#'
#' # because c u a > c u b, player "a" gets the score
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
  class(result) <- c('cpMajority', if('SingleCharElements' %in% class(powerRelation)) 'SingleCharElements')

  # 2^(N-{i,j})
  coalitions <- createPowerset(setdiff(powerRelation$elements, c(e1,e2)), includeEmptySet = includeEmptySet)

  result$tuples <- lapply(
    coalitions,
    function(S) {
      t <- sets::tuple(
        coalition = sets::as.set(S),
        included = FALSE,
        winner = NULL,
        e1 = -1,
        e2 = -1
      )

      eq1 <- equivalenceClassIndex(powerRelation, c(S,e1), stopIfNotExists = FALSE)
      eq2 <- equivalenceClassIndex(powerRelation, c(S,e2), stopIfNotExists = FALSE)
      if(eq1 >= 1 && eq2 >= 1) {
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
#' (\mjeqn{d_{ij}(\succeq)}{d_ij(>=)}), and a negative number of coalitions where `e1` is beaten by `e2` (\mjeqn{-d_{ji}(\succeq)}{-d_ji(>=)}).
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
  stopifnot(class(e1) == class(powerRelation$elements))
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  # 2^(N-{i,j})
  coalitions <- createPowerset(setdiff(powerRelation$elements, c(e1,e2)), includeEmptySet = includeEmptySet)

  pos <- 0
  neg <- 0

  for(S in coalitions) {
    c1 <- equivalenceClassIndex(powerRelation, c(S, e1), FALSE)
    if(c1 == -1) next

    c2 <- equivalenceClassIndex(powerRelation, c(S, e2), FALSE)
    if(c2 == -1) next

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
  cat('\nScore of', x$e2$name, '=', x$e2$score, '\n')
}

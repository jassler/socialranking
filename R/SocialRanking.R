#' `SocialRanking` object
#'
#' Create a `SocialRanking` object.
#'
#' Similar to [`PowerRelation()`], `SocialRanking` expects expects a list to represent a power relation.
#' Unlike [`PowerRelation()`] however, this list should not be nested and should only contain vectors, each vector containing elements that are deemed equally preferable.
#'
#' Use [`doRanking()`] to rank elements based on arbitrary score objects.
#'
#' A social ranking solution, or ranking solution, or solution, maps each power relation between coalitions to a power relation between its elements.
#' I.e., from the power relation \eqn{\succeq: \{1,2\} \succ \{2\} \succ \{1\}}{\{1,2\} > \{2\} > \{1\}}, we may expect the result of a ranking solution \eqn{R^\succeq}{R^(>=)}
#' to rank element 2 over 1. Therefore \eqn{2 R^\succeq 1}{2 R^(>=) 1} will be present, but not \eqn{1 R^\succeq 2}{1 R^(>=) 2}.
#'
#' Formally, a ranking solution \eqn{R: \mathcal{T}(\mathcal{P}) \rightarrow \mathcal{T}(N)} is a function that,
#' given a power relation \eqn{\succeq \in \mathcal{T}(\mathcal{P})}{>= in T(P)}, always produces a power relation
#' \eqn{R(\succeq)}{R(>=)} (or \eqn{R^\succeq}{R^(>=), or just R here for better readability}) over its set of elements.
#' For two elements \eqn{i, j \in N}{i,j in N}, \eqn{i R^\succeq j}{iRj} means that applying the solution \eqn{R}{R} on the ranking \eqn{\succeq}{>=}
#' makes \eqn{i}{i} at least as preferable as \eqn{j}{j}.
#' Often times \eqn{iI^\succeq j}{iIj} and \eqn{iP^\succeq j}{iPj} are used to indicate its symmetric and asymmetric part, respectively.
#' As in, \eqn{iI^\succeq j}{iIj} implies that \eqn{iR^\succeq j}{iRj} and \eqn{jR^\succeq i}{jRi},
#' whereas \eqn{iP^\succeq j}{iIj} implies that \eqn{iR^\succeq j}{iRj} but not \eqn{jR^\succeq i}{jRi}.
#'
#' @param l A list of vectors
#'
#' @template return/SocialRanking
#'
#' @seealso Function that ranks elements based on their scores, [`doRanking()`]
#'
#' @examples
#' SocialRanking(list(c("a", "b"), "f", c("c", "d")))
#' # a ~ b > f > c ~ d
#'
#' @export
SocialRanking <- function(l) {
  structure(l, class = 'SocialRanking')
}

#' Create a `SocialRanking` object
#'
#' Rank elements based on their scores.
#'
#' All ranking solutions in the package are tied to the scores or score vectors of the elements.
#' For these kinds of solutions, `doRanking()` offers a simple way that turns a (named) vector or list of scores for each element into a `SocialRanking` object.
#' For example, `doRanking(c(a=1,b=2))` produces `b > a` (\eqn{b P^\succeq a}{bPa}), because `b` with a score of `2` should be placed higher than `a` with a score of `1`.
#'
#' Ranking solutions in the package include [`lexcelRanking()`], [`ordinalBanzhafRanking()`] and [`L1Ranking()`], among others.
#' These functions take a power relation, calculate the scores of each element and returns a `SocialRanking` object.
#'
#' R natively supports sorting for [vectors][base::c()], but not for [lists][base::list()].
#' If the use of lists is necessary, or if the native sort method in vectors does not produce the desired results, there are two possible ways to solve this:
#'
#' 1. by the introduction of custom S3 classes, or
#' 2. by setting the `compare` parameter in `doRanking()`.
#'
#' For S3 classes, the class for the score object has to be set and the `==` and `>` (and `[` for lists) operators overloaded.
#' I.e., [`lexcelScores()`] returns a list with the custom class `LexcelScores` that implements `==.LexcelScores`, `>.LexcelScores`, `[.LexcelScores` and `is.na.LexcelScores`.
#'
#' In cases where we only want to experiment, introducing new S3 classes can be cumbersome.
#' As an alternative, the `compare` parameter can be assigned a function.
#' This function must take two parameters, i.e., `function(a, b)`, where `a` and `b` are the scores of two arbitrary elements.
#' The function then must return one of the following:
#'
#' * `> 0` (positive value) if score `a` is ranked higher than score `b`,
#' * `< 0` (negative value) if score `a` is ranked lower than score `b`, or
#' * `= 0` if both scores `a` and `b` are considered equal.
#'
#' In `doRanking(c(a=3,b=2,c=2), compare = function(a,b) a - b)`, the compare function returns a positive value of the first parameter is larger than the second.
#' `a` has the highest value and will there for be ranked highest, `a > b ~ c`.
#'
#' Conversely, `doRanking(c(a=3,b=2,c=2), compare = function(a,b) b - a)` favors elements with lower scores, resulting in the element ranking `b ~ c > a`.
#'
#' @param scores A vector or list representing each element's score. If `names(scores)` is not `NULL`, those will be used as element names.
#' Else a number sequence corresponding to the elements is generated.
#' @param compare Optional comparison function taking in two elements and returning a numerical value based on the relation between
#' these two elements. If set to `NULL`, the default [`order()`] function is called. See details for more information.
#' @param decreasing If `TRUE` (default), elements with higher scores are ranked higher.
#'
#' @template return/SocialRanking
#'
#' @seealso [`SocialRanking()`]
#'
#' @examples
#' doRanking(c(a=1,b=2))
#' # b > a
#'
#' doRanking(c(a=2,b=2))
#' # a ~ b
#'
#' # a custom ranking function. Here, we implement the following ranking solution:
#' # disregard any big coalitions and only rank elements based on their individual performances
#' # iRj if and only if {i} >= {j}
#' singletonRanking <- function(pr) {
#'   scores <- sapply(pr$elements, equivalenceClassIndex, powerRelation = pr)
#'   # note that coalitions in higher indexed equivalence classes are less preferable
#'   # hence, scores should be sorted in an increasing order
#'   doRanking(scores, decreasing = FALSE)
#' }
#'
#' pr <- as.PowerRelation("abc > ab > ac > b ~ c ~ bc > a")
#' singletonRanking(pr)
#' # b ~ c > a
#'
#' # a reverse lexcel ranking, where vectors are compared right to left
#' # here, we introduce a compare function. It returns:
#' # * 0, if a and b are identical
#' # * a positive value, if a[i] > b[i] and every value after that is equal
#' # * a negative value, if a[i] < b[i] and every value after that is equal
#' reverseLexcelCompare <- function(a, b) {
#'   i <- which(a != b) |> rev()
#'   if(length(i) == 0) 0
#'   else a[i[1]] - b[i[1]]
#' }
#'
#' scores <- unclass(cumulativeScores(pr))
#'
#' # R cannot natively sort a class. Instead:
#' # Method 1 - utilize the compare parameter
#' doRanking(scores, compare = reverseLexcelCompare)
#'
#'
#' # Method 2 - introduce S3 class
#' `[.RevLex` <- function(x, i, ...) structure(unclass(x)[i], class = "RevLex")
#' `==.RevLex` <- function(a, b) reverseLexcelCompare(a[[1]],b[[1]]) == 0
#' `>.RevLex` <- function(a, b) reverseLexcelCompare(a[[1]],b[[1]]) > 0
#' is.na.RevLex <- function(x) FALSE
#' doRanking(structure(scores, class = "RevLex"))
#'
#' stopifnot(
#'   doRanking(scores, compare = reverseLexcelCompare) ==
#'   doRanking(structure(scores, class = "RevLex"))
#' )
#'
#' @export
doRanking <- function(scores, compare = NULL, decreasing = TRUE) {
  elements <- names(scores)
  if(is.null(elements)) {
    elements <- seq(scores)
  } else if(all(grepl("^[0-9]+$", elements))) {
    elements <- as.numeric(elements)
  }

  if(is.null(compare)) {
    orderedIndexes <- order(scores)
    isEquiv <- function(a,b) { a == b }

  } else {
    orderedIndexes <- customOrder(scores, compare)
    isEquiv <- function(a,b) { compare(a[[1]], b[[1]]) == 0 }
  }
  if(decreasing) {
    orderedIndexes <- rev(orderedIndexes)
  }

  orderItem <- c(orderedIndexes[1])
  orderList <- list()

  for(o in orderedIndexes[-1]) {
    if(any(sapply(orderItem, function(x) isEquiv(scores[o], scores[x])))) {
      orderItem <- c(orderItem, o)
    } else {
      orderList[[length(orderList)+1]] <- sort(orderItem)
      orderItem <- c(o)
    }
  }
  orderList[[length(orderList)+1]] <- sort(orderItem)

  orderList <- lapply(orderList, function(r) {
    sapply(r, function(x) elements[x])
  })

  SocialRanking(orderList)
}

customOrder <- function(scores, compare) {
  indices <- seq_along(scores)
  comps <- expand.grid(x = indices, y = indices)
  comps$diff <- apply(comps, 1, function(x) {
    compare(scores[[x[1]]], scores[[x[2]]]) <= 0
  })
  table(comps$x, comps$diff)[,1] |> sort() |> names() |> as.numeric()
}

#' @export
print.SocialRanking <- function(x, ...) {
  l <- lapply(x, function(r) paste(r, collapse = ' ~ '))
  cat(unlist(l), sep = ' > ')
  cat('\n')
}

#' @export
`==.SocialRanking` <- function(a, b) {
  if(length(a) != length(b))
    return(FALSE)
  for(i in seq_along(a)) {
    if(length(a[[i]]) != length(b[[i]]))
      return(FALSE)
    if(!all(a[[i]] %in% b[[i]]))
      return(FALSE)
  }
  return(TRUE)
}


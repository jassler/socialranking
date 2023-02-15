#' `SocialRankingSolution` object
#'
#' Use [`doRanking()`] to create a `SocialRankingSolution` object.
#'
#' @param x An object
#' @param ... Arguments passed to or from other methods
#'
#' @template return/noreturn
#'
#' @export
SocialRankingSolution <- function(x, ...) {
  UseMethod('SocialRankingSolution', x)
}

#' `SocialRankingSolution` object
#'
#' Use [`doRanking()`] to create a `SocialRankingSolution` object.
#'
#' @param x An object
#' @param ... Arguments passed to or from other methods
#'
#' @template return/noreturn
#'
#' @export
SocialRankingSolution.default <- function(x, ...) {
  stop('Use doRanking() to create a PowerRelation object.')
}

#' Create `SocialRankingSolution`
#'
#' Map a power relation between coalitions to a power relation between elements, also known as a social ranking solution.
#'
#' A normal sort function is used to get the order of `scores` and determine the ranking from highest to lowest scoring element
#' (or lowest to highest scoring if `decreasing` is set to `FALSE`). This works in instances where the score vector contains simple
#' numerical values that are easily sortable.
#'
#' In more complex scenarios it may be necessary to introduce a custom comparison function to determine the relation between elements.
#' In the case of `LexcelScore`, a custom S3 class is introduced that implements the `[`, `==` and `>` operator (see examples).
#'
#' To circumvent the introduction of S3 classes, a function can be assigned to the `compare` parameter. It must take two parameters
#' (i.e. `function(a, b)`) and returns one of the following:
#'
#' * `> 0` (positive value) if score `a` is ranked higher than score `b`
#' * `< 0` (negative value) if score `a` is ranked lower than score `b`
#' * `= 0` if both scores `a` and `b` are considered equal.
#'
#' This means for instance that `doRanking(c(a=3,b=2,c=2), compare = function(a,b) a - b)` ranks elements with higher scores higher
#' (here `a > b ~ c`), whereas `doRanking(c(a=3,b=2,c=2), compare = function(a,b) b - a)` favors elements with lower scores (`b ~ c > a`).
#'
#' If the `compare` is set to `NULL`, the default [`order()`] function is called. This means that for lists an S3 class has to be
#' implemented supporting the indexing and comparison operators. See examples for more.
#'
#' @param scores A sortable vector or list of element scores. If `names(scores)` is not `NULL`, those will be used as element names.
#' Else a number sequence corresponding to the elements is generated.
#' @param compare Optional comparison function taking in two elements and returning a numerical value based on the relation between
#' these two elements. If set to `NULL`, the default [`order()`] function is called. See details for more information.
#' @param decreasing If `TRUE` (default), elements with higher scores are ranked higher.
#'
#' @return A list of type `SocialRankingSolution`.
#' Each element of the list contains a [`sets::set()`] of elements in `powerRelation` that are indifferent to one another.
#'
#' @examples
#' # TODO work on examples
#' pr <- as.PowerRelation("2 > 12 > 1")
#'
#' # we define our own social ranking solution.
#' # a player's score is determined by the equivalence class index it first appears in.
#' # lower is better
#' scores <- c(`1` = 2, `2` = 1)
#'
#' # 2 > 1
#' doRanking(scores)
#'
#' @export
doRanking <- function(scores, compare = NULL, decreasing = TRUE) {
  elements <- names(scores)
  if(is.null(elements)) {
    elements <- seq(scores)
  } else if(all(grepl("^[0-9]+$", elements))) {
    elements <- as.integer(elements)
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

  orderItem <- sets::set(orderedIndexes[1])
  orderList <- list()

  for(o in orderedIndexes[-1]) {
    if(any(sapply(orderItem, function(x) isEquiv(scores[o], scores[x])))) {
      orderItem <- orderItem | sets::set(o)
    } else {
      orderList[[length(orderList)+1]] <- orderItem
      orderItem <- sets::set(o)
    }
  }
  orderList[[length(orderList)+1]] <- orderItem

  orderList <- lapply(orderList, function(r) {
    sapply(r, function(x) elements[x])
  })

  class(orderList) <- 'SocialRankingSolution'
  return(orderList)
}

customOrder <- function(scores, compare) {
  indices <- seq_along(scores)
  comps <- expand.grid(x = indices, y = indices)
  comps$diff <- apply(comps, 1, function(x) {
    if(is.list(scores)) {
      compare(scores[[x[1]]], scores[[x[2]]]) <= 0
    } else {
      compare(scores[x[1]], scores[x[2]]) <= 0
    }
  })
  as.numeric(names(sort(table(comps$x, comps$diff)[,1])))
}

#' @export
print.SocialRankingSolution <- function(x, ...) {
  l <- lapply(x, function(r) paste(r, collapse = ' ~ '))
  cat(unlist(l), sep = ' > ')
  cat('\n')
}

#' @export
`==.SocialRankingSolution` <- function(a, b) {
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

mySort <- function(someList, compare) {
  indices <- seq_along(someList)
  comps <- expand.grid(x = indices, y = indices)
  comps$diff <- !apply(comps, 1, function(x) {
    if(is.list(someList)) {
      compare(someList[[x[1]]], someList[[x[2]]])
    } else {
      compare(someList[x[1]], someList[x[2]])
    }
  })
  answer <- as.numeric(names(sort(table(comps$x, comps$diff)[,1])))
  result <- someList[answer]
  attributes(result) <- attributes(someList)
  names(result) <- names(someList)[answer]
  return(result)
}

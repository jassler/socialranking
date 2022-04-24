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
#' @template param/powerRelation
#' @param scores A sortable vector or list of element scores
#' @param isIndifferent A function that returns `TRUE`, if given two elements from `scores` the order doesn't matter.
#' In that case the two elements are indifferent from each other, symbolized with the `"~"` operator.
#' @param decreasing If `TRUE` (default), elements with the higher scores are ranked higher.
#'
#' @return A list of type `SocialRankingSolution`.
#' Each element of the list contains a [`sets::set()`] of elements in `powerRelation` that are indifferent to one another.
#'
#' @examples
#' pr <- newPowerRelationFromString("2 > 12 > 1", asWhat = as.numeric)
#'
#' # we define our own social ranking solution.
#' # a player's score is determined by the equivalence class index it first appears in.
#' # lower is better
#' scores <- c(`1` = 2, `2` = 1)
#'
#' # 2 > 1
#' doRanking(
#'   pr,
#'   scores,
#'   isIndifferent = function(x, y) x == y,
#'   decreasing = FALSE
#' )
#'
#' # Suppose for a player to be ranked higher than the other,
#' # their positions have to be at least 2 apart.
#' # This means player 1 and 2 are indifferent,
#' # if they are right next to each other in the power relation.
#' # 2 ~ 1
#' doRanking(
#'   pr,
#'   scores,
#'   isIndifferent = function(x, y) abs(x - y) < 2,
#'   decreasing = FALSE
#' )
#'
#' @export
doRanking <- function(powerRelation, scores, isIndifferent = function(a, b) a == b, decreasing = TRUE) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #

  orderedIndexes <- order(scores, decreasing = decreasing)
  orderItem <- sets::set(orderedIndexes[1])
  orderList <- list()

  for(o in orderedIndexes[-1]) {
    if(any(sapply(orderItem, function(x) isIndifferent(scores[o], scores[x])))) {
      orderItem <- orderItem | sets::set(o)
    } else {
      orderList[[length(orderList)+1]] <- orderItem
      orderItem <- sets::set(o)
    }
  }
  orderList[[length(orderList)+1]] <- orderItem

  orderList <- lapply(orderList, function(r) {
    sapply(r, function(x) powerRelation$elements[x])
  })

  class(orderList) <- 'SocialRankingSolution'
  return(orderList)
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

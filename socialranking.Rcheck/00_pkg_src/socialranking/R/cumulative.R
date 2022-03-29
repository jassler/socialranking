#' @export
`[.CumulativeScores` <- function(x, i, ...) structure(unclass(x)[i], class = 'CumulativeScores')

#' @export
`==.CumulativeScores` <- function(a, b) {
  identical(a, b) || (!(a > b) && !(b > a))
}

#' @export
`>.CumulativeScores` <- function(a, b) {
  a <- a[[1]]
  b <- b[[1]]
  all(a >= b) && any(a > b)
}

#' @export
is.na.CumulativeScores <- function(x) FALSE

#' Cumulative scores
#'
#' Calculate cumulative score vectors for each element.
#'
#' \loadmathjax
#' An element's cumulative score vector is calculated by cumulatively adding up the
#' amount of times it appears in each equivalence class in the `powerRelation`. E.g. in a linear power relation with
#' eight coalitions, if element 1 appears in coalitions placed at 1, 3, and 6,
#' its score vector is \[1, 1, 2, 2, 2, 3, 3, 3\].
#'
#' @template param/powerRelation
#' @template param/elements
#'
#' @family score vector functions
#'
#' @references
#' \insertRef{2015Cumulative}{socialranking}
#'
#' \insertRef{2017axiomaticAndAlgorithmicPerspectives}{socialranking}
#'
#' @return Score function returns a list of type `CumulativeScores` and length of `powerRelation$elements`
#' (unless parameter `elements` is specified).
#' Each index contains a vector of length `powerRelation$equivalenceClasses`, cumulatively counting up the number of
#' times the given element appears in each equivalence class.
#'
#' @examples
#' pr <- newPowerRelationFromString("12 > 1 > 2", asWhat = as.numeric)
#'
#' # `1`: c(1, 2, 2)
#' # `2`: c(1, 1, 2)
#' cumulativeScores(pr)
#'
#' # calculate for selected number of elements
#' cumulativeScores(pr, c(2))
#'
#' @export
cumulativeScores <- function(powerRelation, elements = NULL) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  if(is.null(elements)) elements <- powerRelation$elements
  else if(!is.null(err <- powerRelationHasElements(powerRelation, elements))) stop(err)
  # --- end checks --- #

  result <- list()
  for(e in elements) {

    result[[paste(e)]] <- unlist(unlist(lapply(
      powerRelation$equivalenceClasses,
      function(coalitions) sum(e == unlist(coalitions))
    )))

  }

  result <- lapply(result, cumsum)
  structure(result, class = 'CumulativeScores')
}


#' Cumulative domination
#'
#' @section Dominance:
#'
#' \mjseqn{i} dominates \mjseqn{j} if for each index
#' \mjeqn{x, \textrm{Score}(i)_x \geq \textrm{Score}(j)_x}{x, Score(i)_x >= Score(j)_x}.
#'
#' \mjseqn{i} _strictly_ dominates \mjseqn{j}, if additionally
#' \mjeqn{\textrm{Score}(i) \neq \textrm{Score}(j)}{Score(i) != Score(j)}.
#'
#' @template param/powerRelation
#' @template param/e1and2
#' @template param/strictly
#'
#' @rdname cumulativeScores
#'
#' @return `cumulativelyDominates()` returns `TRUE` if `e1` cumulatively dominates `e2`.
#'
#' @examples
#' # TRUE
#' d1 <- cumulativelyDominates(pr, 1, 2)
#'
#' # TRUE
#' d2 <- cumulativelyDominates(pr, 1, 1)
#'
#' # FALSE
#' d3 <- cumulativelyDominates(pr, 1, 1, strictly = TRUE)
#'
#' stopifnot(all(d1, d2, !d3))
#'
#' @export
cumulativelyDominates <- function(powerRelation, e1, e2, strictly = FALSE) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  stopifnot(e1 %in% powerRelation$elements)
  stopifnot(class(e1) == class(powerRelation$elements))
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  if(e1 == e2) {
    return(strictly == FALSE)
  }

  scores <- cumulativeScores(powerRelation, c(e1, e2))
  if(strictly)
    all(scores[[1]] >= scores[[2]]) && !identical(scores[[1]], scores[[2]])
  else
    all(scores[[1]] >= scores[[2]])
}

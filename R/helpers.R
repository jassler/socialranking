#' Create powerset
#'
#' Given a vector of elements generate a [`newPowerRelation()`]-valid function call with all possible coalitions.
#'
#' @param elements vector of elements
#' @param copyToClipboard Copy code string to clipboard
#' @param writeLines Write code string to console
#' @param includeEmptySet If `TRUE`, an empty vector is added at the end
#'
#' @return List of power set vectors. If `copyToClipboard = TRUE`, it returns nothing and only copies a
#' function-call string into the clipboard. If `writeLines = TRUE`, it returns nothing and prints a function-call
#' string that is ready to be copy-pasted.
#'
#' @examples
#' if(interactive()) {
#'   createPowerset(1:3, copyToClipboard = TRUE)
#'   createPowerset(c("a", "b", "c", "d"), writeLines = TRUE, includeEmptySet = FALSE)
#' }
#'
#' # without copyToClipboard or writeLines set to TRUE, it returns a list
#' createPowerset(c("Alice", "Bob"), includeEmptySet = FALSE)
#' ## [[1]]
#' ## [1] "Alice" "Bob"
#' ##
#' ## [[2]]
#' ## [1] "Alice"
#' ##
#' ## [[3]]
#' ## [1] "Bob"
#'
#' @export
createPowerset <- function(elements, copyToClipboard = FALSE, writeLines = FALSE, includeEmptySet = TRUE) {
  # masks <- 2^(1:N-1)
  # lapply( 1:2^N-1, function(u) (1:N)[ bitwAnd(u, masks) != 0 ] )
  if(length(elements) == 0) {
    sets <- list()
  } else {
    n <- 1:length(elements)

    sets <- unlist(lapply(rev(n), function(i) utils::combn(n, i, simplify = F)), recursive = FALSE)
    sets <- lapply(sets, function(i) elements[i])
  }
  if(includeEmptySet) {
    sets[length(sets)+1] <- list(c())
  }

  if(copyToClipboard == FALSE && writeLines == FALSE)
    return(sets)

  makeListCopyable(sets, class(elements), writeLines = writeLines, copyToClipboard = copyToClipboard)
}

makeListCopyable <- function(l, class, writeLines, copyToClipboard) {
  if(class == 'character') {
    formatted <- lapply(l, paste, collapse = '", "')
    formatted <- paste0('"', formatted, '"')
    for(i in which(sapply(l, is.null)))
      formatted[i] <- ""
  } else {
    formatted <- lapply(l, paste, collapse = ',')
  }
  formatted <- paste0('c(', unlist(formatted), ')')
  formatted <- paste0(formatted, collapse = ',\n  ">", ')
  formatted <- paste0('newPowerRelation(\n  ', formatted, ',\n)')

  if(copyToClipboard)
    clipr::write_clip(formatted)
  if(writeLines)
    writeLines(formatted)
}


# This function does not call stop, because then the R error handler is able
# to tell then in which function this call actually failed
powerRelationHasElements <- function(powerRelation, elements) {
  notAnElement <- setdiff(elements, powerRelation$elements)
  if(length(notAnElement) > 0) {
    return(paste(
      if(length(notAnElement) == 1) 'The element' else 'The elements',
      paste(notAnElement, collapse = ', '),
      if(length(notAnElement) == 1) 'does' else 'do',
      'not appear in any of the coalitions of the power ranking given. Only valid elements are',
      paste(powerRelation$elements, collapse = ', ')
    ))
  } else {
    return(NULL)
  }
}


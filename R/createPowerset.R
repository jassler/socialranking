#' Create powerset
#'
#' Given a vector of elements generate a power set.
#'
#' @param elements vector of elements
#' @param includeEmptySet If `TRUE`, an empty vector is added at the end
#' @param result What to do with the result. Can be either:
#' * `"return"`: return list object
#' * `"print"`: create valid string to call [`PowerRelation()`] or [`as.PowerRelation()`] and print it
#' * `"copy"`: create valid string to call [`PowerRelation()`] or [`as.PowerRelation()`] and copy it to clipboard
#'
#' @return List of power set vectors.
#' If the parameter `result` is set to `"print"` or `"copy"`, nothing is returned.
#' Instead, a character string is generated that can be used in R to call and create a new [`PowerRelation`] object.
#' This string is either printed or copied to clipboard (see argument `result`).
#'
#' @examples
#' #' # normal return type is a list of vectors
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
#' # instead of creating a list, print the power set such that it can be copy-pasted
#' # and used to create a new PowerRelation object
#' createPowerset(letters[1:4], result = "print")
#' # prints
#' # as.PowerRelation("
#' #   abcd
#' #   < abc
#' #   < abd
#' #   < acd
#' #   < bcd
#' #   < ab
#' #   ...
#' #   < {}
#' # ")
#'
#' # create the same string as before, but now copy it to the clipboard instead
#' if(interactive()) {
#'   createPowerset(1:3, result = "copy")
#' }
#'
#' # Note that as.PowerRelation(character) only assumes single-char elements.
#' # As such, the generated function call string if element names are longer
#' # looks a little different.
#' createPowerset(c("Alice", "Bob"), result = 'print')
#' # PowerRelation(rlang::list2(
#' #   list(c("Alice", "Bob")),
#' #   list(c("Alice")),
#' #   list(c("Bob")),
#' #   list(c()),
#' # ))
#'
#' @export
createPowerset <- function(elements, includeEmptySet = TRUE, result = c('return', 'print', 'copy')) {
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

  if(result[1] == 'return')
    return(sets)

  s <- makeListCopyable(elements, sets, writeLines = writeLines, copyToClipboard = copyToClipboard)
  if(result[1] == 'print')
    writeLines(s)
  else if(result[1] == 'copy')
    clipr::write_clip(s)
  else
    stop('Invalid argument for result, should either be "return", "print" or "copy".')
}

makeListCopyable <- function(elements, l, writeLines, copyToClipboard) {
  if(all(nchar(elements) == 1)) {
    formatted <- sapply(l, paste, collapse = '')
    for(i in which(sapply(l, is.null))) formatted[i] <- "{}"
    formatted <- paste(formatted, collapse = '\n  > ')
    paste0('as.PowerRelation("\n  ', formatted, '\n")')
  } else {
    if(class(elements) == 'character') {
      formatted <- sapply(l, paste, collapse = '", "')
      formatted <- sapply(formatted, function(x) paste0('"', x, '"'))
      for(i in which(sapply(l, is.null)))
        formatted[i] <- ""
    } else {
      formatted <- lapply(l, paste, collapse = ',')
    }

    formatted <- paste0('list(c(', unlist(formatted), '))')
    formatted <- paste0(formatted, collapse = ',\n  ')
    paste0('PowerRelation(rlang::list2(\n  ', formatted, ',\n))')
  }
}

#' Create PowerRelation object
#'
#' Alternative ways of creating [`PowerRelation`] objects.
#'
#' @param x An object
#' @param ... Optional additional parameters
#' @param comparators Vector of ">" or "~" characters
#'
#' @export
as.PowerRelation <- function(x, ...) {
  UseMethod('as.PowerRelation')
}

#' @section Using a character string:
#'
#' The same way a power relation \eqn{\succsim}{>=} may be represented in literature (or printed by an [`PowerRelation`] object),
#' a simple string containing letters, numbers, `>` or `~` can be used to input a new power relation.
#'
#' Every special character is ignored, with the exception of \eqn{\succsim}{\\succsim} (`"\u227B"`) and \eqn{\sim}{\\sim} (`"\u223C"`).
#'
#' Every letter or number is assumed to be an individual element.
#' `"abc > ac"` therefore would represent two coalitions, the first one of size 3 with the elements `a`, `b`, and `c`.
#' This method does not allow for elements to be entered that are supposed to be multiple characters long.
#'
#' An empty coalitions can be simply left blank (i.e., `"abc > ~ ac"`),
#' though it is often clearer if curly braces are used to indicate such (i.e., `"abc > {} ~ ac"`).
#'
#' @rdname as.PowerRelation
#'
#' @examples
#' # Using character strings
#' as.PowerRelation("abc > ab > ({} ~ c) > (a ~ b ~ ac) > bc")
#' # abc > ab > ({} ~ c) > (a ~ b ~ ac) > bc
#'
#' # using createPowerset(), then shifting coalitions up and down using Alt+Up and Alt+Down
#' if(interactive()) {
#'   createPowerset(1:2, result = "copy")
#' }
#' as.PowerRelation("
#'   12
#'   > 1
#'   ~ {}
#'   > 2
#' ")
#'
#' @export
as.PowerRelation.character <- function(x, ...) {
  x <- gsub('[^0-9a-zA-Z>~\u227B\u223C]', '', x)

  eqs <- list()
  eq <- list()
  coal <- c()

  for(char in strsplit(x, '')[[1]]) {
    if(char == '~' || char == '\u223C') {
      eq <- append(eq, list(coal))
      coal <- c()
    } else if(char == '>' || char == '\u227B') {
      eq <- append(eq, list(coal))
      eqs <- append(eqs, list(eq))
      eq <- list()
      coal <- c()
    } else if(grepl('[0-9a-zA-Z]', char)) {
      coal <- c(coal, char)
    }
  }
  eq <- append(eq, list(coal))
  eqs <- append(eqs, list(eq))

  # eqs <- stringr::str_replace_all(x, '[^0-9a-zA-Z>~\u227B\u223C]', '')
  # eqs <- stringr::str_replace_all(eqs, '(?<=~|>|^)(?=~|>|$)', ' ')
  # eqs <- strsplit(eqs, '>|\u227B')[[1]]
  # eqs <- lapply(eqs, function(i) strsplit(i, '~|\u223C')[[1]])
  # eqs <- lapply(eqs, strsplit, '')
  # eqs <- lapply(eqs, function(x) if(length(x) == 0) list(c()) else x)

  if(all(grepl("^[0-9]$", unlist(eqs)))) {
    eqs <- rapply(eqs, as.numeric, how = 'replace')
  }

  PowerRelation(eqs)
}

#' @section Using a list:
#'
#' Create a [`PowerRelation`] object with an unnested list of coalition [vectors][base::c()].
#'
#' By default, a linear order is assumed on the coalitions.
#' I.e., if it is given `list(c(1,2),1,2)`, these three coalitions are put into their own equivalence class,
#' producing `12 > 1 > 2`.
#'
#' The comparators in-between can be adjusted to indicate
#' whether the relation between two coalitions is that of strict preference `>` or indifference `~`.
#'
#' @rdname as.PowerRelation
#'
#' @examples
#' # Using lists
#' as.PowerRelation(list(c(1,2), 2, c(), 1))
#' # 12 > 2 > {} > 1
#'
#' as.PowerRelation(list(c(1,2), 2, c(), 1), comparators = c("~", ">", ">"))
#' # (12 ~ 2) > {} > 1
#'
#' # the length of comparators doesn't necessarily matter.
#' # If comparators are missing, the existing ones are simply repeated...
#' as.PowerRelation(list(c(1,2), 2, c(), 1), comparators = "~")
#' # (12 ~ 2 ~ {} ~ 1)
#'
#' as.PowerRelation(list(c(1,2), 2, c(), 1), comparators = c("~", ">"))
#' # (12 ~ 2) > ({} ~ 1)
#'
#' # ... or the rest is cut off
#' as.PowerRelation(list(c(1,2), 2, c(), 1), comparators = c("~", ">", "~", "~", ">"))
#' # (12 ~ 2) > ({} ~ 1)
#' @export
as.PowerRelation.list <- function(x, ..., comparators = c(">")) {
  eqs <- list()
  eq <- list()
  comparators <- rep(comparators, length.out = length(x) - 1)
  comparators <- c(comparators, '>')
  for(i in seq_along(comparators)) {
    eq <- append(eq, x[i])
    if(comparators[i] == '>') {
      eqs <- append(eqs, list(eq))
      eq <- list()
    }
  }

  PowerRelation(eqs)
}

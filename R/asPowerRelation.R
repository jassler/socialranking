#' @export
as.PowerRelation <- function(x, ..., comparators = NULL) {
  UseMethod('as.PowerRelation')
}

#' @rdname PowerRelation
#'
#' @export
as.PowerRelation.character <- function(x) {
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

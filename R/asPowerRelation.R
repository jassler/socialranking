#' @export
as.PowerRelation <- function(x, ..., comparators = NULL) {
  UseMethod('as.PowerRelation')
}

#' @export
as.PowerRelation.character <- function(x) {
  eqs <- stringr::str_replace_all(x, '[^0-9a-zA-Z>~\u227B\u223C]', '')
  eqs <- strsplit(eqs, '>|\u227B')[[1]]
  eqs <- lapply(eqs, function(i) strsplit(i, '~|\u223C')[[1]])
  eqs <- lapply(eqs, strsplit, '')

  if(all(grepl("^[0-9]$", unlist(eqs)))) {
    eqs <- rapply(eqs, as.numeric, how = 'replace')
  }

  PowerRelation(eqs)
}

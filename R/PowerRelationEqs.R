#' Internal class for equivalence classes of coalitions
#'
#' This object represents a collection of equivalence classes used internally to group coalitions.
#' Each class is a list of coalitions, encoded either as integers (bitmasks) or player vectors.
#'
#' Users typically do not need to interact with this class directly, but several methods are defined for internal or advanced use.
#'
#' @param x An `eqClasses` object.
#' @param i Index or indices.
#' @param asBits Logical flag. If `TRUE`, returns raw integer bitmasks. Otherwise, returns player vectors.
#' @param decreasing Logical, used in `sort`.
#' @param ... Additional arguments (e.g., passed through to indexing methods).
#'
#' @return Varies by method: can return an integer, list, or `eqClasses` object.
#'
#' @name eqClasses
#' @keywords internal
#' @export
print.eqClasses <- function(x, ...) {
  sep <- strrep(' ', nchar(length(x)))
  for(k in seq(x)) {
    cat(paste0('E_', k), '= {', sep = sep)
    res <- sapply(x[[k]], function(coal) {
      paste0('{', paste(coal, collapse = ', '), '}')
    })
    cat(paste(res, collapse = ', '))
    cat('}\n')
  }
}

#' @rdname eqClasses
#' @export
`[.eqClasses` <- function(x, i, ...) {
  structure(
    unclass(x)[i],
    class = 'eqClasses'
  )
}

#' @rdname eqClasses
#' @export
`[[.eqClasses` <- function(x, i, ...) {
  args <- list(...)
  if(any(args$asBits)) {
    unclass(x)[[i]]
  } else {
    lapply(unclass(x)[[i]], function(v) which(intToBits(v) == 1))
  }
}

#' @rdname eqClasses
#' @export
length.eqClasses <- function(x) {
  length(unclass(x))
}

#' @rdname eqClasses
#' @export
as.list.eqClasses <- function(x, ...) {
  lapply(seq_along(x), function(i) x[[i, ...]])
}

#' @rdname eqClasses
#' @export
is.eqClasses <- function(x, ...) {
  'eqClasses' %in% class(x)
}

#' @rdname eqClasses
#' @export
sort.eqClasses <- function(x, decreasing = FALSE, ...) {
  for(i in seq_along(x)) {
    coals <- x[[i, asBits = TRUE]]
    isNull <- vapply(coals, is.null, logical(1))

    coals[isNull] <- 0L

    bitlists <- lapply(coals, function(c) {
      as.integer(intToBits(c))
    })

    hamming <- vapply(bitlists, sum, integer(1))
    bitmatrix <- do.call(rbind, bitlists)
    ord <- order(if (decreasing) -hamming else hamming,
                 -do.call(order, as.data.frame(bitmatrix)))
    x[[i]] <- x[[i, asBits = TRUE]][ord]
  }
  x
}

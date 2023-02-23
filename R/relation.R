#' Create relation matrix
#'
#' For a given [`PowerRelation`] object create a [`relations::relation()`] object.
#'
#' Turn a [`PowerRelation`] object into a [`relations::relation()`] object. The incidence matrix can be viewed with
#' [`relations::relation_incidence()`].
#'
#' The columns and rows of a [`PowerRelation`] object are ordered by TODO `powerRelation$rankingCoalitions`.
#' The `relations` package automatically sorts the columns and rows by their domain names, which is the reason the
#' parameter `domainNames` is included. This way we ensure that the columns and rows are sorted by
#' the order of the power relation.
#'
#' @section Cycles:
#'
#' A [`PowerRelation`] object is defined as being transitive. If a power relation includes a cycle,
#' meaning that the same coalition appears twice in the ranking, all coalitions within that cycle will be considered
#' to be indifferent from one another.
#'
#' For example, given the power relation \eqn{1 \succ 2 \succ 3 \succ 1 \succ 12}{1 > 2 > 3 > 1 > 12},
#' the relation is somewhat equivalent to \eqn{1 \sim 2 \sim 3 \succ 12}{1 ~ 2 ~ 3 > 12}. There is no way
#' to check for cycles in the incidence matrix only.
#'
#' Call [`transitiveClosure()`] to remove cycles in a [`PowerRelation`] object.
#'
#' @template param/powerRelation
#' @param domainNames How should the row and column names be formatted?
#' * `pretty`: Coalitions such as c(1,2) are formatted as 12. To ensure that it's correctly sorted alphabetically, every name is preceded by a certain amount of the invisible Unicode character \\u200b
#' * `numericPrec`: Coalitions such as c(1,2) are formatted as 1\{12\}, the number in front of the curly brace marking its sorted spot. While less pretty, it won't use Unicode characters.
#' * `numeric`: Drop coalition names, only count from 1 upwards. Each number corresponds to the index in TODO `powerRelation$rankingCoalitions`
#' * `function(x)`: A custom function that is passed a number from `1` through `length(powerRelation$rankingCoalitions)`. Must return a `character` object.
#'
#' @seealso [`relations::as.relation()`]
#'
#' @return [`relations::relation()`] object to the corresponding power relation.
#'
#' @examples
#' pr <- as.PowerRelation("12 > 1 > 2")
#' relation <- powerRelationMatrix(pr)
#'
#' # do relation stuff
#' # Incidence matrix
#' # 111
#' # 011
#' # 001
#' relations::relation_incidence(relation)
#'
#' # all TRUE
#' stopifnot(all(
#'   relations::relation_is_acyclic(relation),
#'   relations::relation_is_antisymmetric(relation),
#'   relations::relation_is_linear_order(relation),
#'   relations::relation_is_complete(relation),
#'   relations::relation_is_reflexive(relation),
#'   relations::relation_is_transitive(relation)
#' ))
#'
#'
#' # a power relation where coalitions {1} and {2} are indifferent
#' pr <- as.PowerRelation("12 > (1 ~ 2)")
#' relation <- powerRelationMatrix(pr)
#'
#' # Incidence matrix
#' # 111
#' # 011
#' # 011
#' relations::relation_incidence(relation)
#'
#' # FALSE
#' stopifnot(!any(
#'   relations::relation_is_acyclic(relation),
#'   relations::relation_is_antisymmetric(relation),
#'   relations::relation_is_linear_order(relation)
#' ))
#' # TRUE
#' stopifnot(all(
#'   relations::relation_is_complete(relation),
#'   relations::relation_is_reflexive(relation),
#'   relations::relation_is_transitive(relation)
#' ))
#'
#'
#' # a pr with cycles
#' pr <- suppressWarnings(as.PowerRelation("12 > 1 > 2 > 1"))
#' relation <- powerRelationMatrix(pr)
#'
#' # Incidence matrix
#' # 1111
#' # 0111
#' # 0111
#' # 0111
#' relations::relation_incidence(relation)
#'
#' # custom naming convention
#' relation <- powerRelationMatrix(
#'   pr,
#'   function(x) paste0(letters[x], ":", paste(pr$rankingCoalitions[[x]], collapse = "|"))
#' )
#'
#' relations::relation_incidence(relation)
#' # Incidences:
#' #       a:1|2 b:1 c:2 d:1
#' # a:1|2     1   1   1   1
#' # b:1       0   1   1   1
#' # c:2       0   1   1   1
#' # d:1       0   1   1   1
#'
#' @export
powerRelationMatrix <- function(powerRelation, domainNames = c("pretty", "numericPrec", "numeric")) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #

  rankingCoalitions <- unlist(powerRelation$eqs, recursive = FALSE)

  headerNameFunc <- if(is.function(domainNames)) {
    domainNames
  } else if(domainNames[1] == 'pretty') {
    function(x) paste0(
      strrep("\u200b", x-1),
      if(length(rankingCoalitions[[x]]) == 0) '{}'
      else paste(rankingCoalitions[[x]], collapse = if('SingleCharElements' %in% class(powerRelation)) '' else ',')
    )
  } else if(domainNames[1] == 'numericPrec') {
    function(x) sprintf(
      paste0('%0', nchar(length(rankingCoalitions)), 'd{%s}'),
      x,
      paste(rankingCoalitions[[x]], collapse = if('SingleCharElements' %in% class(powerRelation)) '' else ',')
    )
  } else if(domainNames[1] == 'numeric') {
    function(x) paste(x)
  } else {
    stop(paste('Parameter domainNames must be "pretty", "numericPrec", "numeric" or a function(x) accepting a numeric value x, returning a character object. Instead got:', domainNames))
  }

  headerNames <- sapply(seq_along(rankingCoalitions), headerNameFunc)

  m <- matrix(0, nrow = length(headerNames), ncol = 0)
  ones <- 0
  for(eq in powerRelation$eqs) {
    ones <- ones + length(eq)
    vec <- c(rep(1, ones), rep(0, nrow(m) - ones))
    m <- cbind(m, matrix(vec, nrow = nrow(m), ncol = length(eq)))
  }
  dimnames(m) <- list(headerNames, headerNames)

  # Check for duplicates
  for(coalition in unique(rankingCoalitions[duplicated(rankingCoalitions)])) {
    found <- which(rankingCoalitions == as.character(coalition))
    from <- min(found)
    to <- max(found)

    for(i in from:to) {
      for(j in from:i) {
        m[i,j] <- 1
      }
    }
  }

  relations::as.relation(m)
}

#' @rdname powerRelationMatrix
#'
#' @param x A [`PowerRelation`] object
#' @param ... Further parameters (ignored)
#'
#' @exportS3Method as.relation PowerRelation
as.relation.PowerRelation <- function(x, ...) {
  powerRelationMatrix(x)
}

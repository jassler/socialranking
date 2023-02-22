#' Make Power Relation monotonic
#'
#' Given a `powerRelation` object, make its order monotonic.
#'
#' A power relation is monotonic if
#'
#' \deqn{T \subset S \Leftrightarrow S \succeq T.}{T subset of S <=> S >= T.}
#'
#' for every coalition \eqn{S \subseteq N}{S subset of N}.
#'
#' Calling `makePowerRelationMonotonic()` on some [`PowerRelation`] object moves or adds coalitions to certain equivalence classes
#' so that the power relation becomes monotonic.
#'
#' @template param/powerRelation
#'
#' @template return/PowerRelation
#'
#' @family helper functions transorming existing [`PowerRelation`] objects
#'
#' @examples
#' pr <- as.PowerRelation("ab > ac > abc > b > a > {} > c < bc")
#' makePowerRelationMonotonic(pr)
#' # (abc ~ ab) > ac > (bc ~ b) > a > (c ~ {})
#'
#' # notice that missing coalitions are automatically added
#' # (except for the empty set)
#' pr <- as.PowerRelation("a > b > c")
#' makePowerRelationMonotonic(pr)
#' # (abc ~ ab ~ ac ~ a) > (bc ~ b) > c
#'
#' pr <- as.PowerRelation("a > {} > b > c")
#' makePowerRelationMonotonic(pr)
#' # (abc ~ ab ~ ac ~ a) > (bc ~ b ~ c ~ {})
#'
#' @export
makePowerRelationMonotonic <- function(powerRelation) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #

  els <- powerRelation$elements
  allCoals <- createPowerset(els)
  newEqs <- list()
  for(eq in powerRelation$eqs) {
    # indeces <- sapply(allCoals, function(x) any(sets ::set_is_subset(eq, sets ::as.set(x))))
    indeces <- sapply(allCoals, function(x) eq |> sapply(function(coalition) identical(x, union(x, coalition))))
    if(any(indeces)) {
      newEqs[[length(newEqs) + 1]] <- allCoals[indeces]
      allCoals <- allCoals[!indeces]
    } else if(length(allCoals) == 0) {
      break
    }
  }

  PowerRelation(equivalenceClasses = newEqs)
}

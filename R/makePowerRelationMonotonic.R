#' Make Power Relation monotonic
#'
#' Given a `powerRelation` object, make its order monotonic.
#'
#' A power relation is monotonic if
#'
#' \deqn{T \subset S \Leftrightarrow S \succsim T.}{T subset of S <=> S >= T.}
#'
#' for every coalition \eqn{S \subseteq N}{S subset of N}.
#'
#' Calling `makePowerRelationMonotonic()` on some [`PowerRelation`] object moves or adds coalitions to certain equivalence classes
#' so that the power relation becomes monotonic.
#'
#' @template param/powerRelation
#' @param addMissingCoalitions If `TRUE`, also include all coalitions in the power set of `powerRelation$elements` that are not present in the current power relation.
#'
#' @template return/PowerRelation
#'
#' @family helper functions for transforming power relations
#'
#' @examples
#' pr <- as.PowerRelation("ab > ac > abc > b > a > {} > c > bc")
#' makePowerRelationMonotonic(pr)
#' # (abc ~ ab) > ac > (bc ~ b) > a > (c ~ {})
#'
#' # notice that missing coalitions are automatically added,
#' # except for the empty set
#' pr <- as.PowerRelation("a > b > c")
#' makePowerRelationMonotonic(pr)
#' # (abc ~ ab ~ ac ~ a) > (bc ~ b) > c
#'
#' # setting addMissingCoalitions to FALSE changes this behavior
#' pr <- as.PowerRelation("a > ab > c ~ {} > b")
#' makePowerRelationMonotonic(pr, addMissingCoalitions = FALSE)
#' # (ab ~ a) > (b ~ c ~ {})
#'
#' # notice that an equivalence class containing an empty coalition
#' # automatically moves all remaining coalitions to that equivalence class.
#' pr <- as.PowerRelation("a > {} > b > c")
#' makePowerRelationMonotonic(pr)
#' # (abc ~ ab ~ ac ~ a) > (bc ~ b ~ c ~ {})
#'
#' @export
makePowerRelationMonotonic <- function(powerRelation, addMissingCoalitions = TRUE) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #

  els <- powerRelation$elements
  allCoals <- createPowerset(els)
  if(!addMissingCoalitions) {
    allCoals <- c(
      intersect(allCoals, unlist(powerRelation$eqs, recursive = FALSE)),
      setdiff(unlist(powerRelation$eqs, recursive = FALSE), allCoals)
    )
  }
  newEqs <- list()

  subsetInEq <- function(superset, eq) {
    eq |> sapply(function(coalition) length(eq) == 0 || identical(superset, union(superset, coalition))) |> any()
  }

  for(eq in powerRelation$eqs) {
    # indeces <- sapply(allCoals, function(x) any(sets ::set_is_subset(eq, sets ::as.set(x))))
    indeces <- sapply(allCoals, subsetInEq, eq)
    if(any(indeces)) {
      newEqs[[length(newEqs) + 1]] <- allCoals[indeces]
      allCoals <- allCoals[!indeces]
    } else if(length(allCoals) == 0) {
      break
    }
  }

  PowerRelation(equivalenceClasses = newEqs)
}

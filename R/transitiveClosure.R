#' Transitive Closure
#'
#' Apply transitive closure over power relation that has cycles.
#'
#' A power relation is a binary relationship between coalitions that is transitive.
#' For coalitions \eqn{a, b, c \in 2^N}{a, b, c in 2^N}, this means that if \eqn{a \succ b}{a > b} and
#' \eqn{b \succ c}{b > c}, then \eqn{a \succ c}{a > c}.
#'
#' A power relation with cycles is not transitive. A transitive closure over a power relation removes all cycles and turns it into a
#' transitive relation, placing all coalitions within a cycle in the same equivalence class.
#' If \eqn{a \succ b \succ a}{a > b > a}, from the symmetric definition in [`PowerRelation()`] we
#' therefore assume that \eqn{a \sim b}{a ~ b}. Similarly, if
#' \eqn{a \succ b_1 \succ b_2 \succ \dots \succ b_n \succ a}{a > b_1 > b_2 > ... > b_n > a}, the transitive closure turns it into
#' \eqn{a \sim b_1 \sim b_2 \sim \dots \sim b_n}{a ~ b_1 ~ b_2 ~ ... ~ b_n}.
#'
#' `transitiveClosure()` transforms a [`PowerRelation`] object with cycles into a `PowerRelation` object without cycles.
#' As described above, all coalitions within a cycle then are put into the same equivalence class
#' and all duplicate coalitions are removed.
#'
#' @template param/powerRelation
#'
#' @return [`PowerRelation`] object with no cycles.
#'
#' @examples
#' pr <- as.PowerRelation("1 > 2")
#'
#' # nothing changes
#' transitiveClosure(pr)
#'
#'
#' pr <- suppressWarnings(as.PowerRelation("1 > 2 > 1"))
#'
#' # 1 ~ 2
#' transitiveClosure(pr)
#'
#'
#' pr <- suppressWarnings(
#'   as.PowerRelation("1 > 3 > 1 > 2 > 23 > 2")
#' )
#'
#' # 1 > 3 > 1 > 2 > 23 > 2 =>
#' # 1 ~ 3 > 2 ~ 23
#' transitiveClosure(pr)
#'
#' @export
transitiveClosure <- function(powerRelation) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #

  rankingCoalitions <- unlist(powerRelation$eqs, recursive = FALSE)
  duplicateTrues <- duplicated(rankingCoalitions)
  duplicates <- unique(rankingCoalitions[duplicateTrues])

  newEqs <- powerRelation$eqs
  for(duplicate in rev(duplicates)) {
    indexes <- sort(unique(powerRelation$coalitionLookup(duplicate)))
    if(length(indexes) == 1)
      next

    toAdd <- seq.int(indexes[1]+1, indexes[length(indexes)])

    newEqs[[indexes[1]]] <- append(newEqs[[indexes[1]]], unlist(newEqs[toAdd], recursive = FALSE))
    newEqs[toAdd] <- rep(list(NULL), length(toAdd))
  }

  newEqs <- Filter(function(l) length(l) > 0, newEqs)
  newEqs <- lapply(newEqs, function(eq) eq[!duplicated(eq)])
  PowerRelation(newEqs)
}

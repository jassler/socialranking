#' Transitive Closure
#'
#' Apply transitive closure over power relation that has cycles.
#'
#' \loadmathjax
#' A power relation is a binary relationship between coalitions that is transitive.
#' For coalitions \mjeqn{a, b, c \in 2^N}{a, b, c in 2^N}, this means that if \mjeqn{a \succ b}{a > b} and
#' \mjeqn{b \succ c}{b > c}, then \mjeqn{a \succ c}{a > c}.
#'
#' A power relation with cycles is not transitive. A transitive closure over a power relation removes all cycles and turns it into a
#' transitive relation placing all coalitions within a cycle in the same equivalence class.
#' If \mjeqn{a \succ b \succ a}{a > b > a}, from the symmetric definition in [`newPowerRelation()`] we
#' therefore assume that \mjeqn{a \sim b}{a ~ b}. Similarly if
#' \mjeqn{a \succ b_1 \succ b_2 \succ \dots \succ b_n \succ a}{a > b_1 > b_2 > ... > b_n > a}, the transitive closure turns it into
#' \mjeqn{a \sim b_1 \sim b_2 \sim \dots \sim b_n}{a ~ b_1 ~ b_2 ~ ... ~ b_n}.
#'
#' `transitiveClosure()` transforms a `PowerRelation` object with cycles into a `Powerrelation` object without cycles.
#' As described in the previous paragraph, all coalitions within a cycle then are put into the same equivalence class
#' and all duplicate coalitions are removed.
#'
#' @template param/powerRelation
#'
#' @return [`PowerRelation`] object with no cycles.
#'
#' @examples
#' pr <- newPowerRelation(1, ">", 2)
#'
#' # nothing changes
#' transitiveClosure(pr)
#'
#'
#' pr <- suppressWarnings(newPowerRelation(1, ">", 2, ">", 1))
#'
#' # 1 ~ 2
#' transitiveClosure(pr)
#'
#'
#' pr <- suppressWarnings(
#'   newPowerRelation(1, ">", 3, ">", 1, ">", 2, ">", c(2,3), ">", 2)
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

  duplicateTrues <- duplicated(powerRelation$rankingCoalitions)
  duplicates <- unique(powerRelation$rankingCoalitions[duplicateTrues])

  newCompares <- powerRelation$rankingComparators

  for(duplicate in duplicates) {
    found <- which(powerRelation$rankingCoalitions == duplicate)
    for(i in min(found):(max(found)-1)) {
      newCompares[i] <- '~'
    }
  }

  newCoalitions <- powerRelation$rankingCoalitions[!duplicateTrues]
  if(any(duplicateTrues))
    newCompares <- newCompares[-(which(duplicateTrues)-1)]

  newList <- list(newCoalitions[[1]])
  for(i in seq_along(newCompares)) {
    newList <- append(newList, list(newCompares[i], newCoalitions[[i+1]]))
  }

  newPowerRelation(newList)
}

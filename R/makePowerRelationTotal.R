#' Make Power Relation total
#'
#' Append an equivalence to a power relation with all its missing coalitions to make it total.
#'
#' A power relation is total if for every \eqn{S, T \subseteq N}{S, T subset or equal to N},
#'
#' \deqn{S \succeq T\text{ or }T \succeq S.}{S>=T or T>=S.}
#'
#' In other words, we can compare every coalition against every other coalition there is.
#' The function simply adds the coalitions missing from the [`PowerRelation`] object to make it total behind the last equivalence class there is.
#'
#' @template param/powerRelation
#' @param includeEmptySet If `TRUE`, include the empty set in the last equivalence class if it is missing from the power relation.
#'
#' @template return/PowerRelation
#'
#' @family helper functions transorming existing [`PowerRelation`] objects
#'
#' @examples
#' pr <- as.PowerRelation(list(c(1,2), 3))
#' # 12 > 3
#'
#' makePowerRelationTotal(pr)
#' # 12 > 3 > (123 ~ 13 ~ 23 ~ 1 ~ 2 ~ {})
#'
#' makePowerRelationTotal(pr, includeEmptySet = FALSE)
#' # 12 > 3 > (123 ~ 13 ~ 23 ~ 1 ~ 2)
#'
#' @export
makePowerRelationTotal <- function(powerRelation, includeEmptySet = TRUE) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #
  els <- powerRelation$elements
  allCoals <- createPowerset(els, includeEmptySet = includeEmptySet)
  missing <- setdiff(lapply(allCoals, sets::as.set), powerRelation$rankingCoalitions)
  PowerRelation(append(powerRelation$equivalenceClasses, list(missing)))
}

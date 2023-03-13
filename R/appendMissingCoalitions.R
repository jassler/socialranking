#' Append missing coalitions
#'
#' Append an equivalence class to a power relation with all coalitions of elements that do not appear in the power relation.
#'
#' For a given set of elements \eqn{N = \lbrace 1, ..., n \rbrace}{N = \{1, ..., n\}}, a [`PowerRelation`] object describes a total preorder
#' of its subsets, or coalitions, \eqn{\mathcal{P} \subseteq 2^N}{P subseteq 2^N}, where \eqn{2^N}{2^N} is the superset of elements.
#'
#' If \eqn{\mathcal{P} \neq 2^N}{P != 2^N}, that means that there are some coalitions \eqn{S \in 2^N, S \notin \mathcal{P}}{S in 2^N, S not in P},
#' such that we cannot compare \eqn{S \succeq T}{S >= T} or \eqn{T \succeq S}{T >= S} for every \eqn{T \in \mathcal{P}}{T in P}.
#'
#' This may be caused by \eqn{2^N}{2^N} having too many coalitions to consider.
#' In certain cases, it may be more interesting to only consider the top ranking coalitions and "shoving" all remaining coalitions into the back.
#'
#' For this use-case, `appendMissingCoalitions()` takes the set \eqn{2^N \setminus \mathcal{P}}{2^N - P}
#' and attaches it in form of an equivalence class to the back of the power relation.
#'
#' I.e., take as an example \eqn{12 \succ 13 \succ (1 \sim 2)}{12 > 13 > (1 ~ 2)}. Here, we have
#'
#' \deqn{
#' \begin{aligned}
#' 2^N &= \lbrace 123, 12, 13, 23, 1, 2, 3, \emptyset \rbrace\\
#' \mathcal{P} &= \lbrace 12, 13, 1, 2 \rbrace\\
#' 2^N \setminus \mathcal{P} &= \lbrace 123, 23, 3, \emptyset \rbrace .
#' \end{aligned}
#' }{
#' 2^N = \{ 123, 12, 13, 23, 1, 2, 3, \{\} \}\\
#' P = \{12, 13, 1, 2\}\\
#' 2^N - P = \{123, 23, 3, \{\}\}
#' }
#'
#' Adding the missing coalitions to the power relation then gives us \eqn{12 \succ 13 \succ (1 \sim 2) \succ (123 \sim 23 \sim 3 \sim \emptyset)}{12 > 13 > (1 ~ 2) > (123 ~ 23 ~ 3 ~ \{\})}.
#'
#' @template param/powerRelation
#' @param includeEmptySet If `TRUE`, include the empty set in the last equivalence class if it is missing from the power relation.
#'
#' @template return/PowerRelation
#'
#' @family helper functions for transforming power relations
#'
#' @examples
#' pr <- as.PowerRelation(list(c(1,2), 3))
#' # 12 > 3
#'
#' appendMissingCoalitions(pr)
#' # 12 > 3 > (123 ~ 13 ~ 23 ~ 1 ~ 2 ~ {})
#'
#' appendMissingCoalitions(pr, includeEmptySet = FALSE)
#' # 12 > 3 > (123 ~ 13 ~ 23 ~ 1 ~ 2)
#'
#' @export
appendMissingCoalitions <- function(powerRelation, includeEmptySet = TRUE) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #
  els <- powerRelation$elements
  allCoals <- createPowerset(els, includeEmptySet = includeEmptySet)
  missing <- setdiff(allCoals, unlist(powerRelation$eqs, recursive = FALSE))
  PowerRelation(append(powerRelation$eqs, list(missing)))
}

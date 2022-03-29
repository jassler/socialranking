#' Domination
#'
#' Test if one element dominates the other.
#'
#' \loadmathjax
#' \mjseqn{i} is said to dominate \mjseqn{j}, if
#' \mjeqn{S \cup \lbrace i \rbrace \succeq S \cup \lbrace j \rbrace}{Sn\{i\} >= Sn\{j\}} for all
#' \mjeqn{S \in 2^{N \setminus \lbrace i,j \rbrace}}{S in 2^(N-\{i,j\})}.
#'
#' \mjseqn{i} *strictly* dominates \mjseqn{j}, if there exists one
#' \mjeqn{S \in 2^{N \setminus \lbrace i,j \rbrace}}{S in 2^(N-\{i,j\})} such that
#' \mjeqn{S \cup \lbrace i \rbrace \succ S \cup \lbrace j \rbrace}{Sn\{i\} > Sn\{j\}}.
#'
#' @template param/powerRelation
#' @template param/e1and2
#' @template param/strictly
#' @template param/includeEmptySet
#'
#' @return Logical value `TRUE` if `e1` dominates `e2`, else `FALSE`.
#'
#' @examples
#' pr <- newPowerRelationFromString("12 > 1 > 2", asWhat = as.numeric)
#'
#' # TRUE
#' d1 <- dominates(pr, 1, 2)
#'
#' # FALSE
#' d2 <- dominates(pr, 2, 1)
#'
#' # TRUE (because it's not strict dominance)
#' d3 <- dominates(pr, 1, 1)
#'
#' # FALSE
#' d4 <- dominates(pr, 1, 1, strictly = TRUE)
#'
#' stopifnot(all(d1, !d2, d3, !d4))
#'
#' @export
dominates <- function(powerRelation, e1, e2, strictly = FALSE, includeEmptySet = TRUE) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  stopifnot(e1 %in% powerRelation$elements)
  stopifnot(class(e1) == class(powerRelation$elements))
  stopifnot(e2 %in% powerRelation$elements)
  stopifnot(class(e2) == class(powerRelation$elements))
  # --- end checks --- #

  if(e1 == e2) {
    return(strictly == FALSE)
  }

  score <- cpMajorityComparisonScore(powerRelation, e1, e2, strictly = TRUE, includeEmptySet = includeEmptySet)
  if(strictly)
    score[1] > 0 && score[2] == 0
  else
    score[1] >= 0 && score[2] == 0
}

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
#' @template param/pr
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
transitiveClosure <- function(pr) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(pr))
  # --- end checks --- #

  rankingCoalitions <- lapply(
    seq_along(pr$eqs), function(k) sapply(pr$eqs[[k, asBits=TRUE]], function(v) c(k, v))
  ) |> do.call(what=cbind)
  idx <- which(duplicated(rankingCoalitions[2,]))

  if(length(idx) == 0) {
    return(pr)
  }

  mrg <- as.list(seq_along(pr$eqs))
  for(coal in idx) {
    v <- rankingCoalitions[,coal]
    first <- suppressWarnings(pr$coalitionLookup(v[2], asBits=TRUE))
    if(first == v[1]) {
      next
    }
    while(is.null(mrg[[first]])) {
      first <- first - 1
    }
    mrg[[first]] <- unlist(mrg[first:v[1]])
    mrg[(first+1):v[1]] <- list(c())
  }
  mrg <- Filter(function(l) length(l) > 0, mrg)
  PowerRelation(
    lapply(mrg, function(i) unique(unlist(unclass(pr$eqs)[i]))),
    elements = pr$elements,
    asBits = TRUE
  )
}

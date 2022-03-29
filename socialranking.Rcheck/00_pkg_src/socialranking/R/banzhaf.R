#' @export
`[.OrdinalBanzhafScores` <- function(x, i, ...) structure(unclass(x)[i], class = 'OrdinalBanzhafScores')

#' @export
`==.OrdinalBanzhafScores` <- function(a, b) {
  sum(a[[1]]) == sum(b[[1]])
}

#' @export
`>.OrdinalBanzhafScores` <- function(a, b) {
  sum(a[[1]]) > sum(b[[1]])
}

#' @export
is.na.OrdinalBanzhafScores <- function(x) FALSE

#' Ordinal Banzhaf
#'
#' Calculate the Ordinal Banzhaf scores, the number of
#' positive and negative marginal contributions.
#'
#' \loadmathjax
#' Inspired by the Banzhaf index \insertCite{1964Banzhaf}{socialranking}, the Ordinal Banzhaf
#' determines the score of element \mjseqn{i} by adding the amount of coalitions
#' \mjeqn{S \subseteq N \setminus \lbrace i \rbrace}{S \\subseteq N \\ \{i\}}
#' its contribution impacts positively (\mjeqn{S \cup \lbrace i \rbrace \succ S}{S u \{i\} > S })
#' and subtracting the amount of coalitions where its contribution
#' had a negative impact (\mjeqn{S \succ S \cup \lbrace i \rbrace}{S > S u \{i\}})\insertCite{2019OrdinalBanzhaf}{socialranking}.
#'
#' @template param/powerRelation
#'
#' @family score vector functions
#'
#' @references
#' \insertRef{2019OrdinalBanzhaf}{socialranking}
#'
#' \insertRef{1964Banzhaf}{socialranking}
#'
#' @return Score function returns list of class type `OrdinalBanzhafScores` and length of `powerRelation$elements`.
#' Each index contains a vector of two numbers, the number of positive and the number of negative marginal contributions.
#' Those two numbers summed together gives us the actual ordinal Banzhaf score.
#'
#' @examples
#' # 12 > (2 ~ {}) > 1
#' pr <- newPowerRelation(c(1,2), ">", 2, "~", c(), ">", 1)
#'
#' # Player 1 contributes positively to {2}
#' # Player 1 contributes negatively to {empty set}
#' # Therefore player 1 has a score of 1 - 1 = 0
#' #
#' # Player 2 contributes positively to {1}
#' # Player 2 does NOT have an impact on {empty set}
#' # Therefore player 2 has a score of 1 - 0 = 0
#' # `1` = c(1, -1)
#' # `2` = c(1, 0)
#' ordinalBanzhafScores(pr)
#'
#' @export
ordinalBanzhafScores <- function(powerRelation) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #
  # TODO check for normalization

  result <- list()
  for(e in powerRelation$elements) {
    result[[paste(e)]] <- c(0, 0)
  }

  for(coalition in powerRelation$rankingCoalitions) {
    for(e in coalition) {
      withoutIndex <- equivalenceClassIndex(powerRelation, coalition - sets::set(e), stopIfNotExists = FALSE)
      if(withoutIndex == -1)
        next

      diff <- withoutIndex - equivalenceClassIndex(powerRelation, coalition)
      if(diff < 0)
        result[[paste(e)]][2] <- result[[paste(e)]][2] - 1
      else if(diff > 0)
        result[[paste(e)]][1] <- result[[paste(e)]][1] + 1
    }
  }

  structure(result, class = 'OrdinalBanzhafScores')
}

#' Ordinal Banzhaf Ranking
#'
#' [`ordinalBanzhafRanking()`] returns the corresponding ranking.
#'
#' @template param/powerRelation
#'
#' @rdname ordinalBanzhafScores
#'
#' @template return/ranking
#'
#' @examples
#' # 1 > 2
#' ordinalBanzhafRanking(pr)
#'
#' @export
ordinalBanzhafRanking <- function(powerRelation) {
  doRanking(
    powerRelation,
    ordinalBanzhafScores(powerRelation)
  )
}

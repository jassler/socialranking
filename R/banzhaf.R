#' @export
`[.OrdinalBanzhafScores` <- function(x, i, ...) structure(unclass(x)[i], class = 'OrdinalBanzhafScores')

#' @export
`==.OrdinalBanzhafScores` <- function(a, b) {
  sum(a[[1]][-3]) == sum(b[[1]][-3])
}

#' @export
`>.OrdinalBanzhafScores` <- function(a, b) {
  sum(a[[1]][-3]) > sum(b[[1]][-3])
}

#' @export
is.na.OrdinalBanzhafScores <- function(x) FALSE

#' Ordinal Banzhaf
#'
#' Calculate the Ordinal Banzhaf scores, the number of positive and negative marginal contributions.
#'
#' Inspired by the Banzhaf index \insertCite{1964Banzhaf}{socialranking}, the Ordinal Banzhaf
#' determines the score of element \eqn{i}{i} by adding the amount of coalitions
#' \eqn{S \subseteq N \setminus \lbrace i \rbrace}{S \\subseteq N \\ \{i\}}
#' its contribution impacts positively (\eqn{S \cup \lbrace i \rbrace \succ S}{S u \{i\} > S })
#' and subtracting the amount of coalitions where its contribution
#' had a negative impact (\eqn{S \succ S \cup \lbrace i \rbrace}{S > S u \{i\}})\insertCite{2019OrdinalBanzhaf}{socialranking}.
#'
#' @template param/powerRelation
#' @template param/elements
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
#' pr <- as.PowerRelation("12 > (2 ~ {}) > 1")
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
ordinalBanzhafScores <- function(powerRelation, elements = NULL) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  if(is.null(elements)) elements <- powerRelation$elements
  # --- end checks --- #

  result <- list()
  for(i in seq_along(elements)) {
    score <- c(0,0,0)
    for(coalition in createPowerset(powerRelation$elements[-i])) {
      e1 <- powerRelation$coalitionLookup(coalition)
      e2 <- powerRelation$coalitionLookup(c(coalition, elements[i]))
      if(e1 == -1 || e2 == -1) {
        score[3] <- score[3] + 1
      } else if(e1 < e2) {
        score[2] <- score[2] - 1
      } else if(e2 < e1) {
        score[1] <- score[1] + 1
      }
    }

    result[[paste(elements[i])]] <- score
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
  doRanking(ordinalBanzhafScores(powerRelation))
}

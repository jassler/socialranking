#' socialranking: A package for constructing ordinal power relations and evaluating social ranking solutions
#'
#' The package `socialranking` offers functions to represent ordinal
#' information of coalitions and calculate the power relation between elements or players.
#'
#' [`PowerRelation()`] creates a [`PowerRelation`] object. [`createPowerset()`]
#' is a convenient function to generate a [`PowerRelation()`] or [`as.PowerRelation()`] function call
#' for all possible coalitions.
#'
#' The functions used to analyze power relations can be grouped into comparison functions,
#' score functions and ranking solutions. Ranking solutions produce a `SocialRanking` object.
#'
#' | Comparison Functions         | Score Functions            | Ranking Solutions           |
#' |----------------------------  |--------------------------  |-----------------------------|
#' | [`dominates()`]              |                            |                             |
#' | [`cumulativelyDominates()`]  | [`cumulativeScores()`]     |                             |
#' | [`cpMajorityComparison()`]^1 | [`copelandScores()`]       | [`copelandRanking()`]       |
#' |                              | [`kramerSimpsonScores()`]  | [`kramerSimpsonRanking()`]  |
#' |                              | [`lexcelScores()`]         | [`lexcelRanking()`]         |
#' |                              |                            | [`dualLexcelRanking()`]     |
#' |                              | [`L1Scores()`]             | [`L1Ranking()`]             |
#' |                              | [`ordinalBanzhafScores()`] | [`ordinalBanzhafRanking()`] |
#'
#' ^1 [`cpMajorityComparisonScore()`] is a faster alternative to [`cpMajorityComparison()`], but it produces less data.
#'
#' [`powerRelationMatrix()`] uses [`relations::relation()`] to create
#' an incidence matrix between all competing coalitions. The incidence
#' matrix can be displayed with [`relations::relation_incidence()`].
#'
#' Use `browseVignettes("socialranking")` for more information.
#'
#' @importFrom Rdpack reprompt
#' @importFrom relations as.relation
#'
#' @docType package
#' @name socialrankingpackage
#' @aliases socialranking-package
NULL
#> NULL

release_bullets <- function() {
  c(
    "Updated vignettes/prebuild.pdf? (check ignored/vignetter.R)",
    "Updated auto-generated function checks? (check ignored/checkGenerator.R)"
  )
}

#' socialranking: A package for constructing ordinal power relations and evaluating social ranking solutions
#'
#' The package `socialranking` offers functions to represent ordinal
#' information of coalitions and calculate the power relation between elements or players.
#'
#' [`newPowerRelation()`] creates a `PowerRelation` object. [`createPowerset()`]
#' is a convenient function to generate a [`newPowerRelation()`] function call
#' for all possible coalitions.
#'
#' For every function that calculates the score of a given element in
#' a power relation, there is a `ranking`-equivalent function that
#' displays the ranking between elements. These include:
#'
#' |      *Score Function*      |      *Ranking Function*      |
#' | -------------------------- | ---------------------------- |
#' | [`ordinalBanzhafScores()`] | [`ordinalBanzhafRanking()`]  |
#' | [`copelandScores()`]       | [`copelandRanking()`]        |
#' | [`kramerSimpsonScores()`]  | [`kramerSimpsonRanking()`]   |
#' | [`lexcelScores()`]         | [`lexcelRanking()`]          |
#' |                            | [`dualLexcelRanking()`]      |
#' | [`cumulativeScores()`]     | [`cumulativelyDominates()`]* |
#' |                            | [`dominates()`]*             |
#' |                            | [`cpMajorityComparison()`]*  |
#'
#' *These functions only compare two elements and doen't rank them.
#'
#' [`powerRelationMatrix()`] uses [`relations::relation()`] to create
#' an incidence matrix between all competing coalitions. The incidence
#' matrix can be displayed with [`relations::relation_incidence()`].
#'
#' @importFrom Rdpack reprompt
#' @importFrom relations as.relation
#' @importFrom mathjaxr preview_rd
#'
#' @docType package
#' @name socialranking
NULL
#> NULL

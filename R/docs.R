#' socialranking: A package for constructing ordinal power relations and evaluating social ranking solutions
#'
#' The package `socialranking` offers functions to represent ordinal
#' information of coalitions and calculate the power relation between elements or players.
#'
#' [`newPowerRelation()`] creates a `PowerRelation` object. [`createPowerset()`]
#' is a convenient function to generate a [`newPowerRelation()`] function call
#' for all possible coalitions.
#'
#' The functions used to analyze power relations can be grouped into comparison functions,
#' score functions and ranking solutions. Ranking solutions produce a `SocialRankingSolution` object.
#'
#' | Comparison Functions                                      | Score Functions                               | Ranking Solutions                               |
#' |-----------------------------------------------------------|-----------------------------------------------|-------------------------------------------------|
#' | `dominates()`                                             |                                               |                                                 |
#' | `cumulativelyDominates()`                                 | `cumulativeScores()`                          |                                                 |
#' | `cpMajorityComparison()`<br>`cpMajorityComparisonScore()` | `copelandScores()`<br>`kramerSimpsonScores()` | `copelandRanking()`<br>`kramerSimpsonRanking()` |
#' |                                                           | `lexcelScores()`                              | `lexcelRanking()`<br>`dualLexcelRanking()`      |
#' |                                                           | `ordinalBanzhafScores()`                      | `ordinalBanzhafRanking()`                       |
#'
#' [`powerRelationMatrix()`] uses [`relations::relation()`] to create
#' an incidence matrix between all competing coalitions. The incidence
#' matrix can be displayed with [`relations::relation_incidence()`].
#'
#' Use `browseVignettes("socialranking")` for more information.
#' 
#' @importFrom Rdpack reprompt
#' @importFrom relations as.relation
#' @importFrom mathjaxr preview_rd
#'
#' @docType package
#' @name socialranking
NULL
#> NULL


<!-- README.md is generated from README.Rmd. Please edit that file -->

# `socialranking`

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/jassler/socialranking/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jassler/socialranking?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/socialranking)](https://CRAN.R-project.org/package=socialranking)
<!-- badges: end -->

The package `socialranking` offers functions to represent ordinal
information of coalitions and calculate the power relation between
elements or players.

## Installation

Install the package directly from
[CRAN](https://cran.r-project.org/package=socialranking) with:

``` r
install.packages("socialranking")
```

You can also install the development version of socialranking from
[GitHub](https://github.com/jassler/socialranking) with:

``` r
# install.packages("devtools")
devtools::install_github("jassler/socialranking")
```

## Usage

The package `socialranking` offers functions to represent ordinal
information of coalitions and calculate the power relation between
elements or players.

Once installed, call `library(socialranking)` to load the package into
your current environment.

`PowerRelation()` and `as.PowerRelation()` creates a `PowerRelation`
object. `createPowerset()` is a convenient function to generate a
`PowerRelation()` or `as.PowerRelation()` function call for all possible
coalitions.

``` r
library(socialranking)
if(interactive()) {
  createPowerset(1:3, copyToClipboard = TRUE)
}

# pasted, rearranged, adjusted comparators
as.PowerRelation("
  123
  > 12
  ~ 13
  > 2
  ~ 23
  > 1
  > 3
")
#> 123 > (12 ~ 13) > (2 ~ 23) > 1 > 3

# equivalent
pr <- as.PowerRelation(
  list(c(1,2,3), c(1,2), c(1,3), c(2), c(2,3), c(1), c(3)),
  comparators = c(">", "~", ">", "~", ">", ">")
)

# equivalent
pr <- as.PowerRelation("123 > 12 ~ 13 > 2 ~ 23 > 1 > 3")
pr
#> 123 > (12 ~ 13) > (2 ~ 23) > 1 > 3

pr$elements
#> [1] 1 2 3
pr$eqs[[2]]
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 1 3
```

The functions used to analyze power relations can be grouped into
comparison functions, score functions and ranking solutions. Ranking
solutions produce a `SocialRankingSolution` object.

| Comparison Functions | Score Functions | Ranking Solutions |
|----|----|----|
| `dominates()` |  |  |
| `cumulativelyDominates()` | `cumulativeScores()` |  |
| `cpMajorityComparison()`^1 | `copelandScores()` | `copelandRanking()` |
|  | `kramerSimpsonScores()` | `kramerSimpsonRanking()` |
|  | `ordinalBanzhafScores()` | `ordinalBanzhafRanking()` |
|  | `lexcelScores()` | `lexcelRanking()` |
|  |  | `dualLexcelRanking()` |
|  | `L1Scores()` | `L1Ranking()` |
|  | `LPScores()` | `LPRanking()` |
|  | `LPSScores()` | `LPSRanking()` |

^1 `cpMajorityComparisonScore()` is a faster alternative to
`cpMajorityComparison()`, but it produces less data.

``` r
dominates(pr, 1, 2)
#> [1] FALSE

copelandRanking(pr)
#> 1 ~ 2 > 3

lexcelScores(pr, 1)
#> $`1`
#> [1] 1 2 0 1 0
#> 
#> attr(,"class")
#> [1] "LexcelScores"
```

`PowerRelation` objects can be turned into `relations` objects from the
[relations](https://CRAN.R-project.org/package=relations) package using
`powerRelationMatrix()` or `as.relation()`.

Use `browseVignettes("socialranking")` for further information.

## License

This package is licensed under
[GPL-3](https://choosealicense.com/licenses/gpl-3.0/#).

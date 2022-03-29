## ----echo=FALSE---------------------------------------------------------------
library(kableExtra)
functionTable <- data.frame(
  "Comparison functions" = c("`dominates()`", "`cumulativelyDominates()`", "`cpMajorityComparison()`\n`cpMajorityComparisonScore()`", "", ""),
  "Score functions" = c("", "`cumulativeScores()`", "`copelandScores()`\n`kramerSimpsonScores()`", "`lexcelScores()`", "`ordinalBanzhafScores()`"),
  "Ranking functions" = c("", "", "`copelandRanking()`\n`kramerSimpsonRanking()`", "`lexcelRanking()`\n`dualLexcelRanking()`", "`ordinalBanzhafRanking()`")
)

if(knitr::is_latex_output()) {
  for(i in 1:nrow(functionTable)) {
    for(j in 1:ncol(functionTable)) {
      if(grepl("\n", functionTable[i,j]))
        functionTable[i,j] <- kableExtra::linebreak(functionTable[i,j])
    }
  }
} else {
  for(i in 1:nrow(functionTable)) {
    for(j in 1:ncol(functionTable)) {
      if(grepl("\n", functionTable[i,j]))
        functionTable[i,j] <- gsub("\n", "<br>", functionTable[i,j])
    }
  }
}

# setup for tex documents
# remove ## in front of output
# adjust spacing before and after code chunks
if(knitr::is_latex_output()) {
  hook_output <- knitr::knit_hooks$get("output")
  hook_warning <- knitr::knit_hooks$get("warning")
  
  # remove ##, wrap long output lines
  knitr::knit_hooks$set(output = function(x, options) {
    x <- knitr:::split_lines(x)
    # x <- gsub("^## ", "#| ", x)
    if (any(nchar(x) > 70)) x <- strwrap(x, width = 70)
    x <- paste(x, collapse = "\n")
    hook_output(x, options)
    
  }, warning = function(x, options) {
    x <- knitr:::split_lines(x)
    x <- gsub("^## ", "#! ", x)
    if (any(nchar(x) > 70)) x <- strwrap(x, width = 70)
    x <- paste(x, collapse = "\n")
    hook_warning(x, options)
  })
  
  # adjust spacing around code chunks
  oldSource <- knitr::knit_hooks$get("source")
  knitr::knit_hooks$set(source = function(x, options) {
    x <- oldSource(x, options)
  
    if(options$echo == FALSE)
      return(x)
  
    paste0(
      '\\vspace{10pt}',
      x,
      '\n\\vspace{-10pt}'
    )
  })
}

examples <- list()
exampleCounter <- function(id) {
  if(missing(id)) {
    examples[[length(examples) + 1]] <- -1
  
  } else if(id %in% names(examples)) {
    stop(paste("id", id, "is already in examples"))
  } else {
    examples[[id]] <<- length(examples) + 1
  }
  return(length(examples))
}

definitions <- list()
definitionCounter <- function(id) {
  if(missing(id)) {
    definitions[[length(definitions)+1]] <<- length(definitions) + 1
    
  } else if(id %in% names(examples)) {
    stop(paste("id", id, "is already in examples"))
  } else {
    definitions[[id]] <<- length(definitions) + 1
  }
  length(definitions)
}

refId <- function(id, htmlTemplate = "[ID](ID)") {
  if(knitr::is_latex_output())
    paste0("\\ref{", id, "}")
  else
    gsub("ID", paste0("#", id), htmlTemplate)
}

refDef <- function(def) {
  if(!(def %in% names(definitions)))
    stop(paste0("Definition ", def, " is not defined"))
  
  if(knitr::is_latex_output())
    paste0("\\ref{", def, "}")
  else
    paste0("[", definitions[[def]], "](#", def, ")")
}

## -----------------------------------------------------------------------------
library(socialranking)
newPowerRelation(c(1,2), ">", 1, "~", c(), ">", 2)

newPowerRelationFromString("ab > a ~ {} > b")

newPowerRelationFromString("12 > 1 ~ {} > 2", asWhat = as.numeric)

## ----echo=FALSE---------------------------------------------------------------
kableExtra::kbl(
  if(knitr::is_latex_output()) apply(functionTable, 1:2, function(x) gsub("`([^`]*)`", "\\\\small{\\\\texttt{\\1}}", x))
  else functionTable,
  col.names = c("Comparison Functions", "Score Functions", "Ranking Solutions"),
  escape = FALSE, booktabs = TRUE
) %>% kableExtra::kable_styling(bootstrap_options = c("hover")) %>% kableExtra::row_spec(1:(nrow(functionTable)-1), hline_after = TRUE)

## -----------------------------------------------------------------------------
pr <- newPowerRelationFromString("ab > ac ~ bc > a ~ c > {} > b")

# a dominates b -> TRUE
dominates(pr, "a", "b")

# b does not dominate a -> FALSE
dominates(pr, "b", "a")

# calculate cumulative scores
scores <- cumulativeScores(pr)
# show score of element a
scores$a

# performing a bunch of rankings
lexcelRanking(pr)
dualLexcelRanking(pr)
copelandRanking(pr)
kramerSimpsonRanking(pr)
ordinalBanzhafRanking(pr)

## -----------------------------------------------------------------------------
rel <- relations::as.relation(pr)
rel

relations::relation_incidence(rel)

## -----------------------------------------------------------------------------
library(socialranking)
pr <- newPowerRelation(c(1,2), ">", 2, "~", c(), ">", 1)
pr

class(pr)

## -----------------------------------------------------------------------------
newPowerRelationFromString("12 > 2~{} > 1", asWhat = as.numeric)

## -----------------------------------------------------------------------------
prLong <- newPowerRelation(
  c("Alice", "Bob"), ">", "Bob", "~", c(), ">", "Alice"
)
prLong

class(prLong)

## -----------------------------------------------------------------------------
class(pr) <- class(pr)[-which(class(pr) == "SingleCharElements")]
pr

## ----echo=FALSE---------------------------------------------------------------
df <- data.frame(
    c("`elements`", "`rankingCoalitions`", "`equivalenceClasses`"),
    c("Sorted vector of elements", "Coalitions in power relation", "List containing lists, each\ncontaining coalitions in the\nsame equivalence class"),
    c("`c(1,2)`", "`list(set(1,2),set(2),set(),set(1))`", "`list(list(set(1,2)),`\n\\phantom{list(}`list(set(2), set()),`\n\\phantom{list(}`list(set(1)))`")
)

if(knitr::is_latex_output()) {
  df[3,2] <- kableExtra::linebreak(df[3,2])
  df[3,3] <- kableExtra::linebreak(df[3,3])
} else {
  df[3,2] <- gsub("\n", "<br>", df[3,2])
  df[3,3] <- gsub("\n", "<br>", df[3,3])
}
kableExtra::kbl(
  if(knitr::is_latex_output()) apply(df, 1:2, function(x) gsub("`([^`]*)`", "\\\\small{\\\\texttt{\\1}}", x))
  else df,
  col.names = c("Attribute", "Description", "Value in `pr`"), escape = FALSE, booktabs = TRUE
) %>% kableExtra::kable_styling(bootstrap_options = "hover", latex_options = c("scale_down")) %>% kableExtra::row_spec(1:(nrow(df)-1), hline_after = TRUE)

## -----------------------------------------------------------------------------
prAtts <- newPowerRelation(c(2,2,1,1,2), ">", c(1,1,1), "~", c())
prAtts

prAtts$elements

prAtts$rankingCoalitions

prAtts$rankingComparators

prAtts$equivalenceClasses

## -----------------------------------------------------------------------------
equivalenceClassIndex(prAtts, c(2,1))

equivalenceClassIndex(prAtts, 1)

equivalenceClassIndex(prAtts, c())

# are the given coalitions in the same equivalence class?
coalitionsAreIndifferent(prAtts, 1, c())

coalitionsAreIndifferent(prAtts, 1, c(1,2))

## -----------------------------------------------------------------------------
pr

# reverse power ranking
newPowerRelation(
  rankingCoalitions = rev(pr$rankingCoalitions),
  rankingComparators = pr$rankingComparators
)

## -----------------------------------------------------------------------------
newPowerRelation(rankingCoalitions = rev(pr$rankingCoalitions))

## -----------------------------------------------------------------------------
# if too short -> comparator values are repeated
newPowerRelation(
  rankingCoalitions = as.list(1:9),
  rankingComparators = "~"
)

newPowerRelation(
    rankingCoalitions = as.list(letters[1:9]),
    rankingComparators = c(">", "~", "~")
)

# if too long -> ignore excessive comparators
newPowerRelation(
  rankingCoalitions = pr$rankingCoalitions,
  rankingComparators = c("~", ">", "~", ">", ">", "~")
)

## -----------------------------------------------------------------------------
createPowerset(
  c("a", "b", "c"),
  writeLines = TRUE,
  copyToClipboard = FALSE
)

## -----------------------------------------------------------------------------
ps <- createPowerset(1:2, includeEmptySet = FALSE)
ps

newPowerRelation(rankingCoalitions = ps)

newPowerRelation(rankingCoalitions = createPowerset(letters[1:4]))

## -----------------------------------------------------------------------------
pr <- newPowerRelationFromString("abc > ab ~ ac > bc")

# pr$elements == c("a", "b", "c")
# we define some arbitrary score vector where "a" scores highest
# "b" and "c" both score 1, thus they are indifferent
scores <- c(100, 1, 1)
doRanking(pr, scores)

# we can also tell doRanking to punish higher scores
doRanking(pr, scores, decreasing = FALSE)

## -----------------------------------------------------------------------------
scores <- c(0, 20, 21)
# b and c are considered to be indifferent,
# because their score difference is less than 2
doRanking(pr, scores, isIndifferent = function(a,b) abs(a-b) < 2)

## -----------------------------------------------------------------------------
pr <- newPowerRelationFromString(
  "3 > 1 > 2 > 12 > 13 > 23",
  asWhat = as.numeric
)

# 1 clearly dominates 2
dominates(pr, 1, 2)
dominates(pr, 2, 1)

# 3 does not dominate 1, nor does 1 dominate 3, because
# {}u3 > {}u1, but 2u1 > 2u3
dominates(pr, 1, 3)
dominates(pr, 3, 1)

# an element i dominates itself, but it does not strictly dominate itself
# because there is no Sui > Sui
dominates(pr, 1, 1)
dominates(pr, 1, 1, strictly = TRUE)

## -----------------------------------------------------------------------------
pr <- newPowerRelationFromString("ac > bc ~ b > a ~ abc > ab")

# FALSE because ac > bc, whereas b > a
dominates(pr, "a", "b")

# TRUE because ac > bc, ignoring b > a comparison
dominates(pr, "a", "b", includeEmptySet = FALSE)

## -----------------------------------------------------------------------------
pr <- newPowerRelationFromString("ab > (ac ~ bc) > (a ~ c) > {} > b")
cumulativeScores(pr)

# for each index k, $a[k] >= $b[k]
cumulativelyDominates(pr, "a", "b")

# $a[3] > $b[3], therefore a also strictly dominates b
cumulativelyDominates(pr, "a", "b", strictly = TRUE)

# $b[1] > $c[1], but $c[3] > $b[3]
# therefore neither b nor c dominate each other
cumulativelyDominates(pr, "b", "c")
cumulativelyDominates(pr, "c", "b")

## -----------------------------------------------------------------------------
pr <- newPowerRelationFromString("ab > (ac ~ bc) > (a ~ c) > {} > b")
cpMajorityComparisonScore(pr, "a", "b")

cpMajorityComparisonScore(pr, "b", "a")

if(sum(cpMajorityComparisonScore(pr, "a", "b")) >= 0) {
  print("a >= b")
} else {
  print("b > a")
}

## -----------------------------------------------------------------------------
# Now (ac ~ bc) is not counted
cpMajorityComparisonScore(pr, "a", "b", strictly = TRUE)

# Notice that the sum is still the same
sum(cpMajorityComparisonScore(pr, "a", "b", strictly = FALSE)) ==
  sum(cpMajorityComparisonScore(pr, "a", "b", strictly = TRUE))

## -----------------------------------------------------------------------------
# extract more information in cpMajorityComparison
cpMajorityComparison(pr, "a", "b")

# with strictly set to TRUE, coalition c does
# neither appear in D_ab nor in D_ba
cpMajorityComparison(pr, "a", "b", strictly = TRUE)

## -----------------------------------------------------------------------------
pr <- newPowerRelation(
  c(1,2),
  ">", c(1),
  ">", c(2)
)

# both players 1 and 2 have an Ordinal Banzhaf Score of 1
# therefore they are indifferent to one another
ordinalBanzhafScores(pr)

ordinalBanzhafRanking(pr)

pr <- newPowerRelationFromString("ab > a > {} > b")

# player b has a negative impact on the empty set
# -> player b's score is 1 - 1 = 0
# -> player a's score is 2 - 0 = 2
sapply(ordinalBanzhafScores(pr), function(score) sum(score))

ordinalBanzhafRanking(pr)

## -----------------------------------------------------------------------------
pr <- newPowerRelationFromString("(abc ~ ab ~ c ~ a) > (b ~ bc) > ac")
scores <- copelandScores(pr)

# Based on CP-Majority, a>=b and a>=c (+2), but b>=a (-1)
scores$a

sapply(copelandScores(pr), sum)

copelandRanking(pr)

## -----------------------------------------------------------------------------
pr <- newPowerRelationFromString("(abc ~ ab ~ c ~ a) > (b ~ bc) > ac")
unlist(kramerSimpsonScores(pr))

kramerSimpsonRanking(pr)

## -----------------------------------------------------------------------------
pr <- newPowerRelationFromString(
  "b > (a ~ c) > ab > (ac ~ bc) > {} > abc"
)
kramerSimpsonRanking(pr)

# notice how b's score is negative
unlist(kramerSimpsonScores(pr))

kramerSimpsonScores(pr, elements = "b", compIvsI = TRUE)

## -----------------------------------------------------------------------------
pr <- newPowerRelationFromString(
  "12 > (123 ~ 23 ~ 3) > (1 ~ 2) > 13",
  asWhat = as.numeric
)

# show the number of times an element appears in each equivalence class
# e.g. 3 appears 3 times in [[2]] and 1 time in [[4]]
lapply(pr$equivalenceClasses, unlist)

lexScores <- lexcelScores(pr)
for(i in names(lexScores))
  paste0("Lexcel score of element ", i, ": ", lexScores[i])

# at index 1, element 2 ranks higher than 3
lexScores['2'] > lexScores['3']

# at index 2, element 2 ranks higher than 1
lexScores['2'] > lexScores['1']

lexcelRanking(pr)

## -----------------------------------------------------------------------------
lexcelCumulated <- lapply(lexScores, cumsum)
cumulScores <- cumulativeScores(pr)

paste0(names(lexcelCumulated), ": ", lexcelCumulated, collapse = ', ')
paste0(names(cumulScores), ": ", cumulScores, collapse = ', ')

## -----------------------------------------------------------------------------
pr <- newPowerRelationFromString(
  "12 > (123 ~ 23 ~ 3) > (1 ~ 2) > 13",
  asWhat = as.numeric
)

lexScores <- lexcelScores(pr)

# in regular Lexcel, 1 scores higher than 3
lexScores['1'] > lexScores['3']

# turn Lexcel score into Dual Lexcel score
dualLexScores <- structure(
  lapply(lexcelScores(pr), function(r) -rev(r)),
  class = 'LexcelScores'
)

# now 1 scores lower than 3
dualLexScores['1'] > dualLexScores['3']

# element 2 comes out at the top in both Lexcel and Dual Lexcel
lexcelRanking(pr)

dualLexcelRanking(pr)

## -----------------------------------------------------------------------------
pr <- newPowerRelationFromString("ab > a > {} > b")
rel <- relations::as.relation(pr)

relations::relation_incidence(rel)

c(
  relations::relation_is_acyclic(rel),
  relations::relation_is_antisymmetric(rel),
  relations::relation_is_linear_order(rel),
  relations::relation_is_complete(rel),
  relations::relation_is_reflexive(rel),
  relations::relation_is_transitive(rel)
)

## -----------------------------------------------------------------------------
# a power relation where coalitions {1} and {2} are indifferent
pr <- newPowerRelationFromString("12 > (1 ~ 2)", asWhat = as.numeric)
rel <- relations::as.relation(pr)

# we have both binary relations {1}R{2} as well as {2}R{1}
relations::relation_incidence(rel)

# FALSE
c(
  relations::relation_is_acyclic(rel),
  relations::relation_is_antisymmetric(rel),
  relations::relation_is_linear_order(rel),
  relations::relation_is_complete(rel),
  relations::relation_is_reflexive(rel),
  relations::relation_is_transitive(rel)
)

## -----------------------------------------------------------------------------
newPowerRelation(c(1,2), ">", 2, ">", 1, "~", 2, ">", c(1,2))

## -----------------------------------------------------------------------------
pr <- suppressWarnings(newPowerRelation(1, '>', 2, '>', 1))
pr

transitiveClosure(pr)

# two cycles, (1>3>1) and (2>23>2)
pr <- suppressWarnings(
  newPowerRelationFromString(
    "1 > 3 > 1 > 2 > 23 > 2",
    asWhat = as.numeric
  )
)

transitiveClosure(pr)

# overlapping cycles
pr <- suppressWarnings(
  newPowerRelationFromString("c > ac > b > ac > (a ~ b) > abc")
)

transitiveClosure(pr)


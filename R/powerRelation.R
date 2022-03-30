
#' PowerRelation object
#'
#' Use [`newPowerRelation()`] or [`newPowerRelationFromString()`] to create a PowerRelation object.
#'
#' @param x An object
#' @param ... Arguments passed to or from other methods
#'
#' @template return/noreturn
#'
#' @export
PowerRelation <- function(x, ...) {
  UseMethod('PowerRelation', x)
}

#' PowerRelation object
#'
#' Use [`newPowerRelation()`] or [`newPowerRelationFromString()`] to create a PowerRelation object.
#'
#' @param x An object
#' @param ... Arguments passed to or from other methods
#'
#' @template return/noreturn
#'
#' @export
PowerRelation.default <- function(x, ...) {
  stop('Use newPowerRelation() or newPowerRelationFromString() to create a PowerRelation object.')
}


#' New Power Relation
#'
#' Create a `PowerRelation` object based on coalition parameters separated by `">"` or `"~"`.
#'
#' \loadmathjax
#' A power relation describes the ordinal information between coalitions.
#' [`createPowerset()`] offers a convenient way of creating a powerset over a set of elements that can be used to call
#' the `newPowerRelation()` function. Each coalition in that case is put
#' on a separate line (see example). In RStudio this allows us to easily rearrange the coalitions
#' using the Alt+Up or Alt+Down shortcut (Option+Up or Option+Down on MacOS).
#'
#' A coalition is a [vector][base::c()] or a [`sets::set()`]. Every vector is turned into a [`sets::set()`].
#'
#' @section Mathematical background:
#'
#' Let \mjeqn{N = \lbrace 1, ..., n \rbrace}{N = \{1, ..., n\}} be a finite set of
#' *elements* (sometimes also called players). \mjseqn{2^N}
#' describes the powerset of \mjseqn{N}, or the set of all subsets, also *coalitions*.
#'
#' Let \mjeqn{\mathcal{P} \subseteq 2^N}{P \\subseteq 2^N} be a collection of coalitions. A
#' *power relation* on \mjeqn{\mathcal{P}}{P} is a total preorder
#' \mjeqn{\succeq \subseteq \mathcal{P} \times \mathcal{P}}{>= \\subseteq P x P}.
#'
#' With that, \mjeqn{\mathcal{T}(\mathcal{P})}{T(P)} denotes the family of all power relations on every
#' collection \mjeqn{\mathcal{P} \subseteq 2^N}{P \\subseteq 2^N}. Given a *power relation*
#' \mjeqn{\succeq \in \mathcal{T}(\mathcal{P})}{>= in T(P)}, \mjeqn{\sim}{~} denotes its symmetric
#' part whereas \mjeqn{\succ}{>} its asymmetric part. For example, let \mjeqn{S, T \in \mathcal{P}}{S, T in P}. Then:
#'
#' \mjdeqn{S \sim T \textrm{ if } S \succeq T \textrm{ and } T \succeq S}{S ~ T if S >= T and T >= S}
#'
#' \mjdeqn{S \succ T \textrm{ if } S \succeq T \textrm{ and not } T \succeq S}{S > T if S >= T and not T >= S}
#'
#' @param ... Coalition vector, comparison character (`">"` or `"~"`), coalition vector, comparison character, coalition vector, ...
#' @param rankingCoalitions List of ordered coalition vectors. If empty, it is ignored. Corresponds to
#' `$rankingCoalitions` list from a `PowerRelation` object.
#' @param rankingComparators Vector of `">"` or `"~"` characters. If `rankingCoalitions` list is empty, it is ignored. If
#' vector is empty, it uses the `">"` relation by default.
#'
#' @template return/PowerRelation
#'
#' @references
#' \insertRef{2017axiomaticAndAlgorithmicPerspectives}{socialranking}
#'
#' \insertRef{2019Lexcel}{socialranking}
#'
#' @family newPowerRelation functions
#'
#' @examples
#' if(interactive())
#'   createPowerset(1:3, copyToClipboard = TRUE)
#'
#' # pasted clipboard and rearranged lines using
#' # Alt + Up, and
#' # Alt + Down shortcut in RStudio
#' pr <- newPowerRelation(
#'   c(1,2),
#'   ">", c(1,2,3),
#'   ">", c(1,3),
#'   "~", c(2),
#'   ">", c(1),
#'   "~", c(2,3),
#'   "~", c(3),
#' )
#'
#' # Elements: 1 2 3
#' # 12 > 123 > (13 ~ 2) > (1 ~ 23 ~ 3)
#' print(pr)
#'
#' # {1, 2, 3}
#' pr$elements
#'
#' # {1, 2}, {1, 2, 3}, {1, 3}, {2}, {1}, {2, 3}, {3}
#' pr$rankingCoalitions
#'
#' # ">" ">" "~" ">" ">" ">"
#' pr$rankingComparators
#'
#' # {{1, 2}}, {{1, 2, 3}}, {{1, 3}, {2}}, {{1}, {2, 3}, {3}}
#' pr$equivalenceClasses
#'
#' # not all coalitions of a powerset have to be present
#' newPowerRelation(c(1,2), ">", c(1))
#'
#' # cycles produce a warning (but no errors)
#' newPowerRelation(c(1,2), ">", c(1), ">", c(1,2))
#'
#' # use createPowerset directly
#' # 123 > 12 > 13 > 23 > 1 > 2 > 3 > {}
#' newPowerRelation(rankingCoalitions = createPowerset(1:3))
#'
#' # 123 > (12 ~ 13) > (23 ~ 1) > (2 ~ 3) > {}
#' newPowerRelation(rankingCoalitions = createPowerset(1:3), rankingComparators = c(">", "~"))
#'
#'
#' # It's discouraged to directly change the ordering of a power relation inside a
#' # PowerRelation object. Instead extract rankingCoalitions, rearrange the list
#' # and pass it to newPowerRelation
#' newOrdering <- rev(pr$rankingCoalitions)
#'
#' # 3 > 23 > (1 ~ 2) > (13 ~ 123 ~ 12)
#' newPowerRelation(rankingCoalitions = newOrdering, rankingComparators = pr$rankingComparators)
#'
#' # 3 > 23 > 1 > 2 > 13 > 123 > 12
#' newPowerRelation(rankingCoalitions = newOrdering)
#'
#' @export
newPowerRelation <- function(..., rankingCoalitions = list(), rankingComparators = c()) {
  ranking <- if(length(rankingCoalitions) > 1) {
    if(length(rankingComparators) == 0)
      rankingComparators <- '>'
    rankingComparators <- rep(rankingComparators, length.out = length(rankingCoalitions) - 1)

    l <- list(rankingCoalitions[[1]])
    for(i in 1:length(rankingComparators)) {
      l[[i*2]] <- rankingComparators[i]
      l[i*2+1] <- list(rankingCoalitions[[i+1]])
    }
    l
  } else if(...length() == 1 && is.list(..1)) {
    ranking <- ..1
  } else {
    rlang::list2(...)
  }

  if(length(ranking) < 3) {
    stop(paste("ranking parameter must have at least", 3, "items, got", length(ranking)))
  }

  if(length(ranking) %% 2 != 1) {
    stop("ranking parameter must be a list where the length is uneven. Index 1 contains a vector representing the highest rated coalition. The next coalitions in the ranking are separated with '>' (strictly better) or '~' (indifferent). Use createPowerset(elements, writeLines=T) to get a template list you can work from.")
  }

  # value <- list(ranking = lapply(ranking, function(x) sets::as.tuple(x)))
  value <- list()

  value$rankingCoalitions <- lapply(ranking[seq(1, length(ranking), by = 2)], function(x) sets::as.set(x))
  value$rankingComparators <- unlist(ranking[seq(2, length(ranking), by = 2)])

  if(any(value$rankingComparators != ">" & value$rankingComparators != "~")) {
    stop(paste("Each coalition in ranking list must be separated by a '>' or '~' character, got:", paste(unlist(ranking[seq(2, length(ranking), by = 2)]), collapse = ", ")))
  }

  uniques <- unique(value$rankingCoalitions)
  duplicates <- duplicated(value$rankingCoalitions)
  if(any(duplicates)) {
    duplicates <- unique(value$rankingCoalitions[duplicates])
    duplicates <- sapply(
      duplicates,
      function(x) paste0('{', paste(x, collapse = ', '), '}')
    )
    warning(paste0(
      'Found the following ',
      if(length(duplicates) == 1) 'duplicate' else 'duplicates',
      '. Did you mean to introduce cycles?\n  ',
      paste(duplicates, collapse = '\n  ')
    ))
  }

  value$elements <- unique(sort(unlist(value$rankingCoalitions)))
  value$equivalenceClasses <- generateEquivalenceClasses(value$rankingCoalitions, value$rankingComparators)

  if(all(nchar(value$elements) == 1))
    structure(value, class = c('PowerRelation', 'SingleCharElements'))
  else
    structure(value, class = 'PowerRelation')
}


generateEquivalenceClasses <- function(coalitions, rankingComparators) {
  greaterThans <- c(0, which(rankingComparators == '>'), length(rankingComparators)+1)
  lapply(
    seq_along(greaterThans)[-1],
    function(x) {
      coalitions[seq(greaterThans[x-1]+1, greaterThans[x])]
    }
  )
}

#' Create `PowerRelation` object from string
#'
#' \loadmathjax
#' Given a pure string representation of a power relation, create a `PowerRelation` object.
#'
#' Elements in this power relation are assumed to be one character long.
#' E.g., the coalitions `"{1,2,3}"` and `123` are equivalent, given that the `elementNames`
#' parameter tells the function to only interpret the characters `1`, `2` and `3` as valid element names.
#'
#' @param string String representation of a power relation. Special characters such as \mjeqn{\succ}{\\succ} and
#' \mjeqn{\sim}{\\sim} are replaced with their ASCII equivalents `>` and `~` respectively.
#' @param elementNames Regular expression to match single characters in string input that should
#' be interpreted as a name of an element. If character does not match, it is simply ignored.
#' @param asWhat Elements are interpreted as string characters by default. [`base::as.numeric`]
#' or [`base::as.integer`] can be passed to convert those string characters into numeric values.
#'
#' @family newPowerRelation functions
#'
#' @template return/PowerRelation
#'
#' @examples
#' # Elements: 1 2 3
#' # 123 > 12 > 23 > 1 > (13 ~ 2)
#' newPowerRelationFromString("123 > 12 > 23 > 1 > 13 ~ 2", asWhat = as.numeric)
#'
#' # commas, braces and spaces are ignored by default
#' # notice that since an empty set is not a valid name of an element,
#' # it is simply ignored. Since there are no valid elements at the
#' # end, it is interpreted as an empty set.
#' newPowerRelationFromString("{1,2,3} > {1,3} > {1,2 } ~ \u2205", asWhat = as.numeric)
#'
#' # use unvoncentional names
#' pr <- newPowerRelationFromString(".,; > .;~.,~,; > .~,~;", elementNames = "[.,;]")
#' stopifnot(pr$elements == sort(c(".", ",", ";")))
#'
#' @export
newPowerRelationFromString <- function(string, elementNames = '[0-9a-zA-Z]', asWhat = identity) {
  coalition <- c()
  isAllNumbers <- TRUE

  coals <- list()
  comps <- c()

  for(s in strsplit(string, '')[[1]]) {
    if(s == '>' || s == '~' || s == '\u227B' || s == '\u223C') {
      if(s == '\u227B') s <- '>'
      else if(s == '\u223C') s <- '~'
      coals[length(coals)+1] <- list(asWhat(coalition))
      comps <- c(comps, s)
      coalition <- c()

    } else if(grepl(elementNames, s)) {
      coalition <- c(coalition, s)
    }
  }

  coals[length(coals)+1] <- list(asWhat(coalition))

  if(identical(asWhat, identity) && class(unlist(coals)) == 'character' && all(grepl("^[0-9]+$", unlist(coals)))) {
    coals <- lapply(coals, as.numeric)
    message('Note: Called as.numeric on all elements.\nIf you wanted the elements to be represented as characters instead, call newPowerRelationFromString again and set asWhat = as.character')
  }

  newPowerRelation(rankingCoalitions = coals, rankingComparators = comps)
}


#' Are coalitions indifferent
#'
#' Check if coalitions are indifferent from one another, or, if they appear in the same
#' equivalence class.
#'
#' [`equivalenceClassIndex()`] is called to determine, which equivalence class `c1` and `c2`
#' belong to. It returns `TRUE` if both are in the same equivalence class.
#'
#' If either coalition `c1` or `c2` is not part of the power relation, an error is thrown.
#'
#' @template param/powerRelation
#' @param c1 Coalition [vector][base::c()] or [`sets::set()`]
#' @param c2 Coalition [vector][base::c()] or [`sets::set()`]
#'
#' @return Logical value `TRUE` if `c1` and `c2` are in the same equivalence class, else `FALSE`.
#'
#' @examples
#' pr <- newPowerRelation(c(1,2), ">", c(1), "~", c(2))
#'
#' # FALSE
#' coalitionsAreIndifferent(pr, c(1,2), c(1))
#'
#' # TRUE
#' coalitionsAreIndifferent(pr, 2, 1)
#'
#' # Error: The coalition {} does not appear in the power relation
#' tryCatch(
#'   equivalenceClassIndex(pr, c()),
#'   error = function(e) { e }
#' )
#'
#' @export
coalitionsAreIndifferent <- function(powerRelation, c1, c2) {
  equivalenceClassIndex(powerRelation, c1) == equivalenceClassIndex(powerRelation, c2)
}

#' Get index of equivalence class containing a coalition
#'
#' Given a `coalition` [vector][base::c()] or [sets::set()],
#' return the index of the equivalence class it is located in.
#'
#' @template param/powerRelation
#' @param coalition a coalition vector or [`sets::set`] that is part of `powerRelation`
#' @template param/stopIfNotExists
#'
#' @return Numeric value, equivalence class index where `coalition` appears in.
#'
#' @examples
#' pr <- newPowerRelation(c(1,2), ">", c(1), "~", c(2))
#'
#' # 1
#' equivalenceClassIndex(pr, c(1, 2))
#'
#' # 2
#' equivalenceClassIndex(pr, c(1))
#'
#' # 2
#' equivalenceClassIndex(pr, c(2))
#'
#' # Error: The coalition {} does not appear in the power relation
#' tryCatch(
#'   equivalenceClassIndex(pr, c()),
#'   error = function(e) { e }
#' )
#'
#' # Error: This time only return a -1
#' stopifnot(-1 == equivalenceClassIndex(pr, c(), stopIfNotExists = FALSE))
#'
#' @export
equivalenceClassIndex <- function(powerRelation, coalition, stopIfNotExists = TRUE) {
  # --- checks (generated) --- #
  stopifnot(is.PowerRelation(powerRelation))
  # --- end checks --- #

  coalition <- sets::as.set(coalition)
  for(i in 1:length(powerRelation$equivalenceClasses)) {
    if(any(sapply(powerRelation$equivalenceClasses[[i]], '==', x = coalition)))
      return(i)
  }
  if(stopIfNotExists)
    stop(paste0('The coalition {', paste(coalition, collapse = ', '), '} does not appear in the power relation'))
  else
    return(-1)
}

#' @rdname PowerRelation
#' @export
is.PowerRelation <- function(x, ...) {
  'PowerRelation' %in% class(x)
}

#' @rdname PowerRelation
#' @export
print.PowerRelation <- function(x, ...) {
  cat('Elements: ', paste(x$elements, collapse = ' '), '\n', sep = '')

  p <- if('SingleCharElements' %in% class(x)) {
    function(pl) if(length(pl) > 0) paste(pl, collapse = '') else '{}'
  } else {
    function(pl) paste0('{', paste(pl, collapse = ', '), '}')
  }

  eClasses <- unlist(lapply(
    x$equivalenceClasses,
    function(e) {
      el <- unlist(lapply(
        e,
        function(r) p(r)
      ))
      if(length(el) == 1)
        el
      else
        paste0('(', paste(el, collapse = ' ~ '), ')')
    }
  ))

  cat(eClasses, sep = ' > ')
}

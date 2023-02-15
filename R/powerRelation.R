#' PowerRelation object
#'
#' Create a `PowerRelation` object.
#'
#' A power relation describes the ordinal information between elements.
#' Here specifically, we are interested in the power relation of coalitions, or groups of elements.
#' Each coalition is assumed to be a [vector][base::c()] containing zero (empty coalition), one (singleton) or more elements.
#'
#' [`createPowerset()`] offers a convenient way of creating a power set over a set of elements that can be used to call `PowerRelation()` or [`as.PowerRelation()`].
#'
#' Trying to figure out what equivalence class certain coalitions or elements belong to is quite common.
#' For these sets of problems, the functions `$coalitionLookup(v)` and `$elementLookup(e)` should be utilized.
#'
#' @section Mathematical background:
#'
#' Let \eqn{N = \lbrace 1, ..., n \rbrace}{N = \{1, ..., n\}} be a finite set of *elements* (also called players).
#' Any subset \eqn{S \subseteq N}{S \\subseteq N} is considered to be a group or coalition of elements,
#' where \eqn{\{\}}{\{\}} is referred to as the empty coalition, \eqn{\{i\}}{\{i\}} as a singleton (a coalition of size 1), and \eqn{N}{N} as the grand coalition.
#' The power set \eqn{2^N}{2^N} denotes the set of all subsets over \eqn{N}{N}.
#'
#' Let \eqn{\mathcal{P} \subseteq 2^N}{P \\subseteq 2^N} be a collection of coalitions.
#' A *power relation* on \eqn{\mathcal{P}}{P} is a total preorder \eqn{\succeq \subseteq \mathcal{P} \times \mathcal{P}}{>= \\subseteq P x P}.
#' That is, for any two coalitions \eqn{S, T \in \mathcal{P}}{S, T in P}, either \eqn{(S,T) \in \succeq}{(S,T) in >=}, or \eqn{(T,S) \in \succeq}{(T,S) in >=}, or both.
#' In other words, we can compare any two groups of elements in \eqn{\mathcal{P}}{P} and determine, if one group is better, worse, or equivalent to the other.
#'
#' More commonly, the relation \eqn{(S,T) \in \succeq}{(S,T) in >=} is notated directly as \eqn{S \succeq T}{S >= T}.
#'
#' \eqn{\mathcal{T}(\mathcal{P})}{T(P)} denotes the family of all power relations on every collection \eqn{\mathcal{P} \subseteq 2^N}{P \\subseteq 2^N}.
#' Given a power relation \eqn{\succeq \in \mathcal{T}(\mathcal{P})}{>= in T(P)}, \eqn{\sim}{~} denotes its symmetric part whereas \eqn{\succ}{>} its asymmetric part.
#' Let \eqn{S, T \in \mathcal{P}}{S, T in P}.
#' Then,
#'
#' \deqn{S \sim T \textrm{ if } S \succeq T \textrm{ and } T \succeq S,}{S ~ T if S >= T and T >= S,}
#'
#' \deqn{S \succ T \textrm{ if } S \succeq T \textrm{ and not } T \succeq S.}{S > T if S >= T and not T >= S.}
#'
#' Coalitions who are deemed equivalent (\eqn{S \sim T}{S ~ T}) can be collected into an equivalence class \eqn{\Sigma_i}{E_i}.
#' The list of equivalence classes forms a linear order, \eqn{\Sigma_1 \succ \Sigma_2 \succ \dots \succ \Sigma_m}{E_1 > E_2 > ... > E_m}.
#'
#' @section Mathematical example:
#'
#' As an example, consider the elements \eqn{N = \{\textrm{apple}, \textrm{banana}, \textrm{chocolate}\}}{N = \{apple, banana, chocolate\}}.
#' Each of them individually may go well with pancakes, but we are also interested in the combination of condiments.
#' If we consider all possibilities, we will have to compare the sets
#'
#' \deqn{\mathcal{P} = 2^N = \{\{a,b,c\}, \{a,b\}, \{a,c\}, \{b,c\}, \{a\}, \{b\}, \{c\}, \{\}\}.}{P = 2^N = \{\{a,b,c\}, \{a,b\}, \{a,c\}, \{b,c\}, \{a\}, \{b\}, \{c\}, \{\}\}.}
#'
#' Looking for a way to rank this group of objects, one may arrive at the following total preorder \eqn{\succeq \in \mathcal{T}(\mathcal{P})}{>= in T(P)}:
#'
#' \deqn{\{b,c\} \succ (\{a\} \sim \{c\}) \succ \{b\} \succ \{\} \succ (\{a,b,c\} \sim \{a,b\} \sim \{a, c\}).}{>=: \{b,c\} > (\{a\} ~ \{c\}) > \{b\} > \{\} > (\{a,b,c\} ~ \{a,b\} ~ \{a, c\}).}
#'
#' In this particular case, we get five equivalence classes.
#'
#' \deqn{\Sigma_1 = \{\{b,c\}\}\\
#' \Sigma_2 = \{\{a\}, \{c\}\}\\
#' \Sigma_3 = \{\{b\}\}\\
#' \Sigma_4 = \{\{\}\}\\
#' \Sigma_5 = \{\{a,b,c\},\{a,b\},\{a,c\}\}
#' }{}
#'
#' @references
#' \insertRef{2017axiomaticAndAlgorithmicPerspectives}{socialranking}
#'
#' \insertRef{2019Lexcel}{socialranking}
#'
#' \insertRef{2021Lexcel}{socialranking}
#'
#' @param equivalenceClasses A nested list of lists, each containing coalitions or groups represented as vectors that are in the same equivalence class.
#' @param elements Vector of elements in power relation. Only set this value if you know what you are doing. See Details for more.
#' @param coalitionLookup A function taking a vector parameter and returning an index. See return value for more details. Only set this value if you know what you are doing.
#' @param elementLookup A function taking an element and returning a list of 2-sized tuples. See return value for more details. Only set this value if you know what you are doing.
#'
#' @template return/PowerRelation
#'
#' @examples
#' pr <- PowerRelation(list(
#'   list(c(1,2,3)),
#'   list(c(1, 2), 2, 3),
#'   list(c(2, 3), c()),
#'   list(c(1, 3)),
#'   list(1)
#' ))
#'
#' pr
#' # 123 > (12 ~ 2 ~ 3) > (23 ~ {}) > 13 > 1
#'
#' stopifnot(pr$elements == 1:3)
#' stopifnot(pr$coalitionLookup(1) == 5)
#' stopifnot(pr$coalitionLookup(c()) == 3)
#' stopifnot(pr$coalitionLookup(c(1,2)) == 2)
#'
#' # find coalitions an element appears in
#' for(t in pr$elementLookup(2)) {
#'   stopifnot(2 %in% pr$eqs[[t[1]]][[t[2]]])
#' }
#'
#' # use createPowerset to help generate a valid function call
#' if(interactive())
#'   createPowerset(letters[1:3], result = "copy")
#'
#' # pasted, rearranged using alt+up / alt+down in RStudio
#  as.PowerRelation("
#    a
#    ~ ab
#    < abc
#    < bc
#    < {}
#    ~ b
#    < ac
#    < c
#  ")
#'
#' # note that the function call looks different if elements are only one character long
#' if(interactive())
#'   createPowerset(c("apple", "banana", "chocolate"), result = "copy")
#'
#' # pasted clipboard
#' PowerRelation(rlang::list2(
#'   list(c("banana", "chocolate")),
#'   list(c("apple"),
#'        c("chocolate")),
#'   list(c("banana")),
#'   list(c()),
#'   list(c("apple", "banana", "chocolate"),
#'        c("apple", "banana"),
#'        c("apple", "chocolate")),
#' ))
#' # {banana, chocolate} > ({apple} ~ {chocolate}) > {banana} > {} > ...
#'
#' @export
PowerRelation <- function(equivalenceClasses, elements = NULL, coalitionLookup = NULL, elementLookup = NULL) {

  if(is.null(elements) || is.null(coalitionLookup) || is.null(elementLookup)) {
    equivalenceClasses <- lapply(equivalenceClasses, lapply, sort)
    lookupTables <- createLookupTables(equivalenceClasses)
    elements        <- lookupTables$elements
    coalitionLookup <- lookupTables$coalitionLookup
    elementLookup   <- lookupTables$elementLookup
  }

  classes <- c('PowerRelation', if(all(nchar(elements) == 1)) 'SingleCharElements')

  structure(list(
    elements = elements,
    eqs = equivalenceClasses,
    coalitionLookup = coalitionLookup,
    elementLookup = elementLookup
  ), class = classes)
}

createLookupTables <- function(equivalenceClasses) {
  elements <- unique(sort(unlist(equivalenceClasses)))

  # if(noDuplicates) {
  #   coalitionLookup <- hash::hash(
  #     keys = unlist(equivalenceClasses, recursive = FALSE),
  #     values = unlist(sapply(
  #       seq_along(equivalenceClasses),
  #       function(i) rep(i, length(equivalenceClasses[[i]]))
  #     ))
  #   )
  #   elementLookup <- hash::hash(
  #     keys = elements,
  #     values = rep(list(c()), length(elements))
  #   )
  #   # todo
  # }

  keyList <- tryCatch(
    { lapply(equivalenceClasses, hash::make.keys) },
    error = function(e) { stop('Power relation must contain at least two coalitions and cannot have empty equivalence classes.') }
  )
  keys <- unlist(keyList)

  if(length(keys) <= 1) {
    stop('Power relation must contain at least two coalitions.')
  }

  coalitionLookup <- hash::hash(keys = keys    , values = rep(list(c()), length(keys)))
  elementLookup   <- hash::hash(keys = elements, values = rep(list(c()), length(elements)))

  duplicates <- sets::set()
  for(i in seq_along(keyList)) {
    for(j in seq_along(keyList[[i]])) {
      k <- keyList[[i]][j]
      v <- c(coalitionLookup[[k]], i)
      coalitionLookup[[k]] <- v

      if(length(v) > 1)
        duplicates <- sets::set_union(duplicates, k)

      els <- equivalenceClasses[[i]][[j]]
      if(length(els) == 0)
        next

      for(el in hash::make.keys(equivalenceClasses[[i]][[j]])) {
        elementLookup[[el]] <- append(elementLookup[[el]], list(c(i,j)))
      }
    }
  }
  if(length(duplicates) > 0) {
    warning(paste0('Found ', length(duplicates), ' duplicate coalition', if(length(duplicates) > 1) 's', ', listed below. This violates transitivity and can cause issues with certain ranking solutions. You may want to take a look at socialranking::transitiveClosure().\n    - ', paste(duplicates, collapse = '\n    - ')))
  }

  return(list(
    elements = elements,
    coalitionLookup = function(v, default = -1) { r <- coalitionLookup[[hash::make.keys(list(sort(v)))]]; if(!is.null(r)) r else default},
    elementLookup = function(e) elementLookup[[paste(e)]]
  ))
}

#' @rdname PowerRelation
#' @export
`==.PowerRelation` <- function(a, b) {
  if(length(a$eqs) != length(b$eqs))
    return(FALSE)

  for(i in seq_along(a$eqs)) {
    if(length(a$eqs[[i]]) != length(b$eqs[[i]]))
      return(FALSE)

    for(cl in a$eqs[[i]]) {
      if(any(sapply(b$eqs[[i]], function(x) all(cl == x))))
        next
      return(FALSE)
    }
  }
  return(TRUE)
}

#' @rdname PowerRelation
#' @export
is.PowerRelation <- function(x, ...) {
  'PowerRelation' %in% class(x)
}

#' Are coalitions indifferent
#'
#' Check if coalitions are indifferent from one another, or, if they appear in the same
#' equivalence class.
#'
#' @template param/powerRelation
#' @param c1 Coalition [vector][base::c()] or [`sets::set()`]
#' @param c2 Coalition [vector][base::c()] or [`sets::set()`]
#'
#' @return Logical value `TRUE` if `c1` and `c2` are in the same equivalence class, else `FALSE`.
#'
#' @family equivalence class lookup functions
#'
#' @examples
#' pr <- newPowerRelation(c(1,2), ">", c(1), "~", c(2))
#'
#' stopIfNot(F == coalitionsAreIndifferent(pr, c(1,2), c(1)))
#' stopIfNot(T == coalitionsAreIndifferent(pr, 2, 1))
#'
#' # Note that it doesn't fail with non-existing power relations
#' stopIfNot(F == coalitionsAreIndifferent(pr, 1, c()))
#' stopIfNot(T == coalitionsAreIndifferent(pr, 3, c(1,2,3)))
#'
#' @export
coalitionsAreIndifferent <- function(powerRelation, c1, c2) {
  powerRelation$coalitionLookup(c1) == powerRelation$coalitionLookup(c2)
}

#' @rdname PowerRelation
#' @export
print.PowerRelation <- function(x, ...) {
  p <- if('SingleCharElements' %in% class(x)) {
    function(pl) if(length(pl) > 0) paste(pl, collapse = '') else '{}'
  } else {
    function(pl) paste0('{', paste(pl, collapse = ', '), '}')
  }

  eClasses <- unlist(lapply(
    x$eqs,
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
  cat('\n')
}

#' Get index of equivalence class containing a coalition
#'
#' Deprecated. Try to use powerRelation$coalitionLookup() instead.
#'
#' Given a `coalition` [vector][base::c()] or [sets::set()], return a singular index number of the equivalence class it is located in.
#'
#' @template param/powerRelation
#' @param coalition a coalition vector or [`sets::set`] that is part of `powerRelation`
#' @template param/stopIfNotExists
#'
#' @return Numeric value, equivalence class index where `coalition` appears in.
#'
#' @family equivalence class lookup functions
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

  warning(paste0('Deprecated. Use powerRelation$coalitionLookup(c(', paste(coalition, collapse = ', '), ')) instead.'))

  coalition <- sets::as.set(coalition)
  i <- which(sapply(powerRelation$eqs, function(eq) {
    any(coalition == eq)
  }))
  if(length(i) == 0) {
    if(stopIfNotExists)
      stop(paste0('The coalition {', paste(coalition, collapse = ', '), '} does not appear in the power relation'))
    return(-1)
  } else {
    return(i)
  }
}

#' New Power Relation
#'
#' Deprecated. Use [`PowerRelation()`] instead.
#'
#' @param ... Any parameter.
#' @template return/noreturn
#'
#' @export
newPowerRelation <- function(...) {
  stop("This function has been deprecated. Use PowerRelation() instead.")
}

#' Create [`PowerRelation`] object from string
#'
#' Deprecated. Use [`as.PowerRelation()`] instead.
#'
#' @param ... Any parameter.
#' @template return/noreturn
#'
#' @export
newPowerRelationFromString <- function(...) {
  stop("This function has been deprecated. Use as.PowerRelation() instead.")
}


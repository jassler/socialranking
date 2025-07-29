#' PowerRelation object
#'
#' Create a `PowerRelation` object.
#'
#' A power relation describes the ordinal information between elements.
#' Here specifically, we are interested in the power relation between coalitions, or groups of elements.
#' Each coalition is assumed to be a [vector][base::c()] containing zero (empty coalition), one (singleton) or more elements.
#'
#' [`createPowerset()`] offers a convenient way of creating a power set over a set of elements that can be used to call `PowerRelation()` or [`as.PowerRelation()`].
#'
#' Trying to figure out what equivalence class certain coalitions or elements belong to is quite common.
#' For these sets of problems, the functions `$coalitionLookup(v)` and `$elementLookup(e)` should be utilized.
#' We use some redundancy to speed up the lookup methods.
#' As such, it is highly discouraged to edit a `PowerRelation` object directly, as the different power relation representations will fall out of sync.
#' For more information, see the vignette: `vignette(package = 'socialranking')`
#'
#' The `PowerRelation()` function expects a nested list of coalitions as input. For alternatives, see [`as.PowerRelation()`].
#'
#' @section Mathematical background:
#'
#' Let \eqn{N = \lbrace 1, ..., n \rbrace}{N = \{1, ..., n\}} be a finite set of *elements* (also called players).
#' Any subset \eqn{S \subseteq N}{S \\subseteq N} is considered to be a group or coalition of elements,
#' where \eqn{\{\}}{\{\}} is referred to as the empty coalition, \eqn{\{i\}}{\{i\}} as a singleton (a coalition of size 1), and \eqn{N}{N} as the grand coalition.
#' The power set \eqn{2^N}{2^N} denotes the set of all subsets over \eqn{N}{N}.
#'
#' Let \eqn{\mathcal{P} \subseteq 2^N}{P \\subseteq 2^N} be a collection of coalitions.
#' A *power relation* on \eqn{\mathcal{P}}{P} is a total preorder \eqn{\succsim \subseteq \mathcal{P} \times \mathcal{P}}{>= \\subseteq P x P}.
#' That is, for any two coalitions \eqn{S, T \in \mathcal{P}}{S, T in P}, either \eqn{(S,T) \in \succsim}{(S,T) in >=}, or \eqn{(T,S) \in \succsim}{(T,S) in >=}, or both.
#' In other words, we can compare any two groups of elements in \eqn{\mathcal{P}}{P} and determine, if one group is better than, worse than, or equivalent to the other.
#'
#' More commonly, the relation \eqn{(S,T) \in \succsim}{(S,T) in >=} is notated as \eqn{S \succsim T}{S >= T}.
#'
#' \eqn{\mathcal{T}(\mathcal{P})}{T(P)} denotes the family of all power relations on every collection \eqn{\mathcal{P} \subseteq 2^N}{P \\subseteq 2^N}.
#' Given a power relation \eqn{\succsim \in \mathcal{T}(\mathcal{P})}{>= in T(P)}, \eqn{\sim}{~} denotes its symmetric part whereas \eqn{\succ}{>} its asymmetric part.
#' Let \eqn{S, T \in \mathcal{P}}{S, T in P}.
#' Then,
#'
#' \deqn{
#' S \sim T \textrm{ if } S \succsim T \textrm{ and } T \succsim S,\\
#' S \succ T \textrm{ if } S \succsim T \textrm{ and not } T \succsim S.
#' }{
#' S ~ T if S >= T and T >= S,\\
#' S > T if S >= T and not T >= S.
#' }
#'
#' Coalitions which are deemed equivalent (\eqn{S \sim T}{S ~ T}) can be collected into an equivalence class \eqn{\Sigma_i}{E_i}.
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
#' Looking for a way to rank this group of objects, one may arrive at the following total preorder \eqn{\succsim \in \mathcal{T}(\mathcal{P})}{>= in T(P)}:
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
#' The power relation \eqn{\succsim}{>=} can be copy-pasted as a character string to the [`as.PowerRelation()`] function (it should accept the special characters \eqn{\succsim}{>=} and \eqn{\sim}{~}).
#'
#' `as.PowerRelation("{b,c} > ({a} ~ {c}) > {b} > {} > ({a,b,c} ~ {a,b} ~ {a,c})")`
#'
#' @references
#' \insertRef{2017axiomaticAndAlgorithmicPerspectives}{socialranking}
#'
#' \insertRef{2019Lexcel}{socialranking}
#'
#' \insertRef{2021Lexcel}{socialranking}
#'
#' @param eqs A nested list of lists, each containing coalitions or groups represented as vectors that are in the same equivalence class.
#' @param elements Vector of elements in power relation. Only set this value if you know what you are doing. See Details for more.
#' @param x An \R object.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @template return/PowerRelation
#'
#' @seealso Other ways to create a `PowerRelation()` object using [`as.PowerRelation()`].
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
#' # note that the function call looks different if elements are multiple characters long
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
PowerRelation <- function(eqs, elements = NULL, asBits = FALSE) {
  if(asBits) {
    if(is.null(elements)) {
      elements <- seq((eqs |> unlist() |> max() |> log2() |> floor()) + 1)
      isSeq <- TRUE
    } else {
      isSeq <- all(elements == seq_along(elements))
    }
  } else {
    if(is.null(elements)) {
      elements <- eqs |> unlist() |> unique() |> sort()
    }
    isSeq <- all(elements == seq.int(1, length(elements)))
    eqs <- (
      if(isSeq) lapply(eqs, function(eq) sapply(eq, encodeCoalition))
      else lapply(eqs, function(eq) sapply(eq, encodeCoalition, elements))
    )
  }
  class(eqs) <- 'eqClasses'

  stopifnot(
    'Power relations with 32 or more elements is currently not supported. Please file an issue at https://github.com/jassler/socialranking/issues if this is required.'
    = length(elements) < 32
  )

  if(any(sapply(eqs, length) == 0)) {
    idx <- which(sapply(eqs, length) == 0)
    stop(paste0('Each equivalence class must contain at least one coalition. The following list ', if(length(idx) == 1) 'index was' else 'indexes were', ' empty: ', paste0('eqs[[', idx, ']]', collapse = ', ')))
  }

  classes <- c(
    'PowerRelation',
    if(isSeq) 'Seq',
    if(all(nchar(elements) == 1)) 'SingleCharElements'
  )

  remove(asBits)

  coalTable <- NULL
  elemTable <- NULL
  structure(list(
    elements = elements,
    eqs = eqs,
    coalitionLookup = function(v, asBits=FALSE) {
      if(is.null(coalTable)) { coalTable <<- createCoalitionLookupTable(elements, eqs) }
      coalTable[1 + (
        if(asBits) v
        else if(isSeq) encodeCoalition(v)
        else encodeCoalition(v, elements)
      )]
    },
    elementLookup = function(e) {
      if(is.null(elemTable)) { elemTable <<- createElemLookupTable(elements, eqs) }
      elemTable[[
        if(isSeq) e
        else which(elements == e)
      ]]
    }
  ), class = classes)
}

# old creation times
# coalStuff <- lapply(2:10, function(x) createPowerset(1:x))
# microbenchmark::microbenchmark(
#   PowerRelation(coalStuff[1]),
#   PowerRelation(coalStuff[2]),
#   PowerRelation(coalStuff[3]),
#   PowerRelation(coalStuff[4]),
#   PowerRelation(coalStuff[5]),
#   PowerRelation(coalStuff[6]),
#   PowerRelation(coalStuff[7]),
#   PowerRelation(coalStuff[8]),
#   PowerRelation(coalStuff[9]),
#   times=1000L
# )
#
# OLD
# expr       min         lq       mean     median         uq       max neval
#  n=2    89.093    94.9970   144.4726    99.5480   116.8295 24432.597  1000
#  n=3   138.662   146.6775   196.4533   153.2580   172.5895 21680.308  1000
#  n=4   242.638   255.3275   311.0265   266.0695   286.1800  7255.852  1000
#  n=5   465.268   485.0915   570.0666   502.0245   532.5695 19157.865  1000
#  n=6   946.403   983.5900  1132.3996  1017.1485  1053.6180 19380.946  1000
#  n=7  2028.475  2110.8030  2369.6984  2184.5620  2296.7790  6483.658  1000
#  n=8  4630.663  4795.8315  5377.4642  4990.0075  5614.3965 25045.219  1000
#  n=9 11480.123 12141.4530 12985.8866 12874.2460 13510.2790 29382.404  1000
# n=10 32119.236 33781.3555 34972.6116 34458.2860 35291.2215 53779.249  1000

# COLD
# expr     min      lq      mean   median       uq       max neval
#  n=3  16.154  16.933  17.94808  17.3020  17.8555    94.136  1000
#  n=4  21.279  22.304  23.55401  22.8370  23.6980    54.489  1000
#  n=9 343.621 356.331 398.31775 363.4240 374.5555  4316.029  1000
# n=10 681.215 707.332 791.59327 718.4020 743.0020  4854.113  1000

# COALITION LOOKUP
# expr      min        lq       mean   median        uq      max neval
#  n=3   25.502   26.8755   29.36486   27.552   29.0690   87.822  1000
#  n=4   35.506   37.1870   39.78833   38.007   39.9955  145.714  1000
#  n=9  647.800  670.8420  734.35904  679.206  697.5945 9016.474  1000
# n=10 1282.644 1328.6460 1408.61732 1343.078 1369.6870 9279.694  1000

# ELEMENT LOOKUP
# expr      min        lq       mean    median        uq        max neval
#  n=3   37.761   39.9750   50.08605   43.9520   60.0035    130.667  1000
#  n=4   58.507   61.5205   74.26883   66.1945   82.0000   2373.572  1000
#  n=9 2316.951 2443.2720 2790.11384 2481.4430 2534.1690  19231.829  1000
# n=10 6585.461 7077.6250 8451.62512 7173.2165 7309.6235 106699.466  1000

# BOTH
# expr       min         lq        mean     median         uq       max neval
#  n=3    66.092    70.9300   103.46379    75.9320    89.9130  5164.606  1000
#  n=4   119.310   127.4690   159.53764   136.3455   155.2670  5541.888  1000
#  n=9  6592.267  6923.6700  8049.83016  7049.5195  7539.5515 83310.483  1000
# n=10 17986.782 18967.9940 21411.49343 19406.8785 24375.3815 95770.260  1000



createElemLookupTable <- function(elements, eqs) {
  elemTable <- structure(
    lapply(elements, function(e) rep(0, 2 ^ length(elements))),
    # vector(mode = 'list', length = length(elements)),
    names = elements
  )
  idx <- rep(1, length(elements))

  eqs <- unclass(eqs)
  for(eqI in seq_along(eqs)) {
    for(j in seq_along(eqs[[eqI]])) {
      for(i in which(intToBits(eqs[[eqI]][[j]])[1:length(elements)] == 1)) {
        elemTable[[i]][idx[i]:(idx[i]+1)] <- c(eqI, j)
        idx[i] <- idx[i] + 2
      }
    }
  }
  names <- list(c('E', 'i'), NULL)
  lapply(elemTable, function(tab) matrix(tab[tab > 0], nrow = 2, dimnames = names))
}

#' Internal coalition representation
#'
#' Transform a vector of coalition members into its internal integer representation used by the `SocialRanking` package.
#'
#' Coalitions are internally stored as 32-bit integers, where each bit signalizes if a player (or element) partakes in the given coalition or not.
#' For instance, a `14 = (1110)_2` represents a coalition containing the elements `2`, `3`, and `4`.
#'
#' By default, it is assumed that elements are a sequence of numbers starting from `1`.
#' If this is not the case, pass a supplementary `elements` parameter to map elements to their corresponding sequence indexes.
#'
#' Since all coalitions passed to a `SocialRanking` functions are converted to integers, this processing step can be circumvented if the `asBit` flag in those functions are set to `TRUE` and the corresponding integer representation of the coalition is supplied.

#' @param v A vector of coalition members to be encoded into an integer value.
#' @param x An encoded integer value to be transformed into a vector of elements.
#' @param elements A vector of elements for the encoded bits to be mapped to.
#'
#' @examples
#' stopifnot(11 == encodeCoalition(c(1, 2, 4)))
#' stopifnot(11 == encodeCoalition(c('a', 'b', 'd'), elements = letters))
#'
#' stopifnot(identical(decodeCoalition(11), c(1, 2, 4)))
#' stopifnot(identical(decodeCoalition(11, elements = letters), c('a', 'b', 'd')))
#'
#' # potential usage
#' PowerRelation(list(list(7, 3, 1), list(6, 4), list(5, 2, 1, 0)), asBits = TRUE)
#'
#' PowerRelation(list(list(7, 3, 1), list(6, 4), list(5, 2, 1, 0)), elements = letters[1:3], asBits = TRUE)
#' @export
encodeCoalition <- function(v, elements) {
  if(!(missing(elements) || is.null(elements))) {
    v <- v |> match(elements)
  }
  (2L ^ (v - 1)) |> as.integer() |> Reduce(f = bitwOr, init = 0)
}

#' @rdname encodeCoalition
#' @export
decodeCoalition <- function(x, elements) {
  if(missing(elements)) {
    which(intToBits(x) == 1)
  } else {
    elements[which(intToBits(x) == 1)]
  }
}



createCoalitionLookupTable <- function(elements, eqs) {
  table <- NA
  for(k in seq_along(eqs)) {
    for(coal in eqs[[k, asBits=TRUE]]) {
      if(is.na(table[coal+1])) {
        table[coal+1] <- k
      } else {
        warning(paste0('Found duplicate coalition {', paste(decodeCoalition(coal, elements), collapse=', '), '} in equivalence class ', k, '. It will be ignored in coalitionLookup(), but not in elementLookup().\nIf this was not a mistake, please file an issue at: https://github.com/jassler/socialranking/issues/'))
      }
    }
  }
  table
}

toKey <- function(coalition) {
  paste('\u200b', coalition |> sort() |> paste(collapse = '\u200b'), sep = '')
}
# createLookupTables <- function(equivalenceClasses) {
#   if(length(equivalenceClasses) == 0) {
#     stop('Must supply at least one equivalence class.')
#   }
#
#   empties <- which(sapply(equivalenceClasses, function(eq) length(eq) == 0))
#   if(length(empties) > 0) {
#     stop(paste(
#       'Equivalence classes must not be empty. In the given list of equivalence classes, the following ',
#       if(length(empties) == 1) 'index was' else 'indexes were', ' empty: ', paste(empties, collapse = ', '))
#     )
#   }
#
#   elements <- equivalenceClasses |> unlist() |> sort() |> unique()
#
#   stopifnot('The character "\\u200b" is specially reserved and must not be used in coalition names.' = (!is.character(elements) || length(grep('\u200b', elements)) == 0))
#   keyList <- lapply(equivalenceClasses, lapply, toKey)
#
#   uniqueKeys <- keyList |> unlist() |> unique()
#   #structure(as.list()) hash::hash(keys = keys    , values = NULL)
#   coalitionLookup <- vector(mode = 'list', length = length(uniqueKeys)) |> structure(names = uniqueKeys)
#   elementLookup   <- vector(mode = 'list', length = length(elements)) |> structure(names = elements)
#
#   duplicates <- list()
#   duplicateEls <- list()
#   for(i in seq_along(keyList)) {
#     for(j in seq_along(keyList[[i]])) {
#       k <- keyList[[i]][[j]]
#       v <- c(coalitionLookup[[k]], i)
#       coalitionLookup[[k]] <- v
#
#       coal <- equivalenceClasses[[i]][[j]]
#       if(length(v) > 1) {
#         duplicates <- append(duplicates, paste0('{', paste(coal, collapse = ', '), '}'))
#       }
#
#       if(any((dups <- duplicated(coal)))) {
#         els <- coal[dups] |> sort() |> unique()
#         duplicateEls <- append(duplicateEls, paste0(paste(els, collapse = ', '), ' in the coalition {', paste0(coal, collapse = ', '), '}'))
#       }
#
#       for(el in paste(equivalenceClasses[[i]][[j]])) {
#         elementLookup[[el]] <- append(elementLookup[[el]], list(c(i,j)))
#       }
#     }
#   }
#   if(length(duplicates) > 0) {
#     duplicates <- unique(duplicates)
#     warning(paste0('Found ', length(duplicates), ' duplicate coalition', if(length(duplicates) > 1) 's', ', listed below. This violates transitivity and can cause issues with certain ranking solutions. You may want to take a look at socialranking::transitiveClosure().\n    - ', paste(duplicates, collapse = '\n    - ')))
#   }
#   if(length(duplicateEls) > 0) {
#     warning(paste0('Found ', length(duplicateEls), ' coalition', if(length(duplicateEls) > 1) 's', ' that contain elements more than once.\n    - ', paste0(duplicateEls, collapse = '\n    - ')))
#   }
#
#   return(list(
#     elements = elements,
#     coalitionLookup = function(v) coalitionLookup[[toKey(v)]],
#     elementLookup = function(e) elementLookup[[paste(e)]]
#   ))
# }

#' @export
`==.PowerRelation` <- function(a, b) {
  if(length(a$eqs) != length(b$eqs))
    return(FALSE)

  for(i in seq_along(a$eqs)) {
    if(length(a$eqs[[i]]) != length(b$eqs[[i]]))
      return(FALSE)

    for(cl in a$eqs[[i]]) {
      if(any(sapply(b$eqs[[i]], function(x) identical(cl, x))))
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
#' Check if coalitions are indifferent to one another, or, in other words, if they appear in the same equivalence class.
#'
#' @template param/pr
#' @param c1 Coalition [vector][base::c()]
#' @param c2 Coalition [vector][base::c()]
#'
#' @return Logical value `TRUE` if `c1` and `c2` are in the same equivalence class, else `FALSE`.
#'
#' @family lookup functions
#'
#' @examples
#' pr <- PowerRelation(list(list(c(1,2)), list(1, 2)))
#'
#' stopifnot(coalitionsAreIndifferent(pr, c(1,2), c(1)) == FALSE)
#' stopifnot(coalitionsAreIndifferent(pr, 2, 1) == TRUE)
#'
#' # Note that it doesn't fail with non-existing power relations
#' stopifnot(coalitionsAreIndifferent(pr, 1, c()) == FALSE)
#' stopifnot(coalitionsAreIndifferent(pr, 3, c(1,2,3)) == TRUE)
#'
#' @export
coalitionsAreIndifferent <- function(pr, c1, c2, asBits = FALSE) {
  pr$coalitionLookup(c1, asBits = asBits) == pr$coalitionLookup(c2, asBits = asBits)
}

#' @rdname PowerRelation
#' @export
print.PowerRelation <- function(x, ...) {
  p <- if('SingleCharElements' %in% class(x)) {
    function(pl) if(length(pl) > 0) paste(pl, collapse = '') else '{}'
  } else {
    function(pl) paste0('{', paste(pl, collapse = ', '), '}')
  }
  q <- if('Seq' %in% class(x)) {
    identity
  } else {
    function(co) x$elements[co]
  }

  eClasses <- unlist(lapply(
    x$eqs,
    function(e) {
      el <- unlist(lapply(e, function(r) p(q(r))))
      if(length(el) == 1)
        el
      else
        paste0('(', paste(el, collapse = ' ~ '), ')')
    }
  ))

  cat(eClasses, sep = ' > ')
  cat('\n')
}

#' @rdname PowerRelation
#' @export
sort.PowerRelation <- function(x, decreasing = FALSE, ...) {
  PowerRelation(sort(x$eqs, decreasing = decreasing, ...), elements = x$elements, asBits = TRUE)
}

#' Get index of equivalence class containing a coalition
#'
#' Given a `coalition` [vector][base::c()], return the equivalence class index it appears in.
#'
#' This function calls `pr$coalitionLookup(coalition)`.
#'
#' `equivalenceClassIndex()` serves as an alias to `coalitionLookup()`.
#'
#' @template param/pr
#' @template param/asBits
#' @param coalition a coalition [vector][base::c()] or that is part of `pr`
#'
#' @return Numeric value, equivalence class index containing `coalition`.
#' `NULL` if the coalition does not exist.
#' If the `pr` contains cycles, it is possible that multiple values are returned.
#'
#' @family lookup functions
#'
#' @examples
#' pr <- as.PowerRelation("12 > 2 ~ 1")
#'
#' (e1 <- equivalenceClassIndex(pr, c(1, 2)))
#' # 1
#'
#' (e2 <- equivalenceClassIndex(pr, c(1)))
#' # 2
#'
#' (e3 <- equivalenceClassIndex(pr, c(2)))
#' # 2
#'
#' (e4 <- equivalenceClassIndex(pr, c()))
#' # NULL <- empty set does not exist
#'
#' stopifnot(all(c(e1,e2,e3,e4) == c(1,2,2)))
#'
#' @export
equivalenceClassIndex <- function(pr, coalition, asBits = FALSE) {
  pr$coalitionLookup(coalition, asBits = asBits)
}

#' @rdname equivalenceClassIndex
#' @export
coalitionLookup <- equivalenceClassIndex

#' Element lookup
#'
#' List coalitions that an element appears in.
#'
#' This function calls `pr$elementLookup(element)`.
#' The returned list contains tuples containing the index to find the corresponding coalitions in `pr$eqs`.
#'
#' If  `elementLookup(pr, 2)` returns `list(c(1,1), c(1,2), c(3,1))`, we can determine that the element `2`
#' appears twice in equivalence class `1` and once in equivalence class `3`.
#' The specific coalition then can be accessed with `pr$eqs[[i]][[j]]`, where `i` is the equivalence class index
#' and `j` is the coalition in that equivalence class containing the element.
#'
#' @template param/pr
#' @param element an element in `pr$elements`
#'
#' @return List of tuples, each of size 2.
#' First value of a tuple indicates the equivalence class index,
#' the second value the index inside that equivalence class with the coalition containing the element.
#' Returns `NULL` if the element does not exist.
#'
#' @family lookup functions
#'
#' @examples
#' pr <- as.PowerRelation("12 > 2 ~ 1")
#'
#' l <- elementLookup(pr, 1)
#' l
#' # (1,1), (2,2)
#'
#' sapply(l, function(tuple) 1 %in% pr$eqs[[tuple[1]]][[tuple[2]]]) |> all() |> stopifnot()
#'
#' # if element does not exist, it returns NULL
#' elementLookup(pr, 3) |> is.null() |> stopifnot()
#'
#' @export
elementLookup <- function(pr, element) {
  pr$elementLookup(element)
}


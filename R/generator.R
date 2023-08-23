
#' Generate power relations
#'
#' Based on a list of coalitions, create a generator function that returns a new [`PowerRelation`] object with every call.
#' `NULL` is returned once every possible power relation has been generated.
#'
#' Using the `partitions` library, [`partitions::compositions()`] is used to create all possible partitions over the set of coalitions.
#' For every partition, [`partitions::multinomial()`] is used to create all permutations over the order of the coalitions.
#'
#' Note that the number of power relations (or total preorders) grows incredibly fast.
#'
#' The Stirling number of second kind \eqn{S(n,k)}{S(n,k)} gives us the number of \eqn{k}{k} partitions over \eqn{n}{n} elements.
#'
#' \deqn{S(n,k) = \frac{1}{k!}\sum_{j=0}^{k} (-1)^j \binom{k}{j}(k-j)^n}{S(n,k) = 1/k! * sum_(j=0)^(k) -1^j binom(k,j) (k-j)^n}
#'
#' For example, with 4 coalitions (n = 4) there are 6 ways to split it into k = 3 partitions.
#' The sum of all partitions of any size is also known as the Bell number (\eqn{B_n = \sum_{k=0}^n S(n,k)}{B_n = S(n,0) + S(n,1) + ... + S(n,n)}, see also [`numbers::bell()`]).
#'
#' Regarding total preorders \eqn{\mathcal{T}(X)}{T(X)} over a set \eqn{X}{X}, the Stirling number of second kind can be used to determine the number of all possible total preorders \eqn{|\mathcal{T}(X)|}{|T(X)|}.
#'
#' \deqn{|\mathcal{T}(X)| = \sum_{k=0}^{|X|} k! * S(|X|, k)}{|T(X)| = Sum_{k=0}^{|X|} k! * S(|X|, k)}
#'
#' In literature, it is referred to as the ordered Bell number or Fubini number.
#'
#' In the context of social rankings we may consider total preorders over the set of coalitions \eqn{2^N}{2^N} for a given set of elements or players \eqn{N}{N}.
#' Here, the number of coalitions doubles with every new element.
#' The number of preorders then are:
#'
#' | # of elements | # of coalitions | # of total preorders  | 1ms / computation |
#' | ------------- | --------------- | --------------------  | ----------------- |
#' | 0             | 1               | 1                     | 1ms               |
#' | 1             | 2               | 3                     | 3ms               |
#' | 2             | 4               | 75                    | 75ms              |
#' | 3             | 7 (w/o empty set) | 47,293              | 47 seconds        |
#' | 3             | 8               | 545,835               | 9 minutes         |
#' | 4             | 15 (w/o empty set) | 230,283,190,977,853 | 7,302 years      |
#' | 4             | 16              | 5,315,654,681,981,355 | 168,558 years     |
#'
#' @param coalitions List of coalition vectors. An empty coalition can be set with `c()`.
#' @param startWithLinearOrder If set to `TRUE`, the first [`PowerRelation`] object generated will be a linear order in the order of the list of `coalitions` they are given.
#' If set to `FALSE`, the first [`PowerRelation`] object generated will have a single equivalence class containing all coalitions, as in, every coalition is equally powerful.
#'
#' @return A generator function.
#' Every time this generator function is called, a different [`PowerRelation`] object is returned.
#' Once all possible power relations have been generated, the generator function returns `NULL`.
#'
#' @family generator functions
#'
#' @examples
#' coalitions <- createPowerset(c('a','b'), includeEmptySet = FALSE)
#' # list(c('a','b'), 'a', 'b')
#'
#' gen <- powerRelationGenerator(coalitions)
#'
#' while(!is.null(pr <- gen())) {
#'   print(pr)
#' }
#' # (ab ~ a ~ b)
#' # (ab ~ a) > b
#' # (ab ~ b) > a
#' # (a ~ b) > ab
#' # ab > (a ~ b)
#' # a > (ab ~ b)
#' # b > (ab ~ a)
#' # ab > a > b
#' # ab > b > a
#' # a > ab > b
#' # b > ab > a
#' # a > b > ab
#' # b > a > ab
#'
#' # from now on, gen() always returns NULL
#' gen()
#' # NULL
#'
#' # Use generateNextPartition() to skip certain partitions
#' gen <- powerRelationGenerator(coalitions)
#'
#' gen <- generateNextPartition(gen)
#' gen <- generateNextPartition(gen)
#' gen()
#'
#' @export
powerRelationGenerator <- function(coalitions, startWithLinearOrder = FALSE) {
  rlang::check_installed('partitions')

  if(length(coalitions) < 2) {
    stop('At least two coalitions must be given.')

  } else if(length(coalitions) > 20) {
    warning('More than 20 coalitions were given. partitions::compositions() is called to generate all partitions beforehand. This may dampen or exceed system resources.')
  }

  # created once, used every time upon generating a new PowerRelation object
  elements <- unique(sort(unlist(coalitions)))

  if(is.vector(coalitions)) {
    coalitions <- lapply(coalitions, identity)
  }
  coalitions <- lapply(coalitions, sort)
  # create warning about duplicate coalitions
  PowerRelation(list(coalitions))

  compositions <- partitions::compositions(length(coalitions))
  compositions <- compositions[,apply(compositions, 2, function(x) {
    zeros <- which(x == 0)
    l <- length(zeros)
    l == 0 || l == (zeros[l] - zeros[1] + 1)
  })]
  r <- nrow(compositions)
  compositions <- compositions[,order(apply(compositions, 2, function(x)
    sum(sapply(seq_along(x), function(i) x[i] * (r + 1 - i)))
  ), decreasing = !startWithLinearOrder)]

  compI <- 1
  part <- Filter(function(x) x != 0, compositions[,1])
  perms <- partitions::multinomial(part)
  partCum <- c(0, cumsum(part))
  permsI <- 0

  done <- FALSE

  nextPartition <- function() {
    if(compI >= ncol(compositions)) {
      done <<- TRUE
      return()
    }

    compI <<- compI + 1
    part <<- Filter(function(x) x != 0, compositions[,compI])
    perms <<- partitions::multinomial(part)
    partCum <<- c(0, cumsum(part))

    permsI <<- 0
  }

  function() {
    if(permsI >= ncol(perms)) {
      nextPartition()
    }
    if(done) {
      return(NULL)
    }

    permsI <<- permsI + 1

    eqs <- lapply(seq.int(length(partCum)-1), function(x) {
      coalitions[perms[(partCum[x]+1):partCum[x+1],permsI]]
    })

    # Calling PowerRelation like this is 5x slower
    #PowerRelation(eqs)

    coalitionLookup <- eqs |> seq_along() |> lapply(function(i)
      rep(i, length(eqs[[i]]))
    ) |> unlist() |> as.list() |> structure(names = sapply(unlist(eqs, recursive = FALSE), toKey))

    elementLookup <- structure(vector('list', length(elements)), names = paste(elements))
    for(i in seq_along(eqs)) {
      for(j in seq_along(eqs[[i]])) {
        for(el in paste(eqs[[i]][[j]])) {
          elementLookup[[el]] <- append(elementLookup[[el]], list(c(i,j)))
        }
      }
    }

    PowerRelation(
      eqs,
      elements = elements,
      coalitionLookup = function(v) coalitionLookup[[toKey(v)]],
      elementLookup = function(e) elementLookup[[paste(e)]]
    )
  }
}

#' Next partition
#'
#' Skip to the next partition of the generator.
#'
#' @param gen A generator object.
#' @return A generator function.
#' If the generator is already down to its last partition, it will throw an error.
#'
#' @family generator functions
#'
#' @examples
#' coalitions <- createPowerset(c('a','b'), includeEmptySet = FALSE)
#' # list(c('a','b'), 'a', 'b')
#'
#' gen <- powerRelationGenerator(coalitions)
#' gen()
#' # (ab ~ a ~ b)
#'
#' gen()
#' # (ab ~ b) > a
#'
#' # skipping partition of size two, where the first partition has
#' # 2 coalitions and the second partition has 1 coalition
#' gen <- generateNextPartition(gen)
#' gen()
#' # ab > (a ~ b)
#'
#' # only remaining partition is one of size 3, wherein each
#' # equivalence class is of size 1
#' gen <- generateNextPartition(gen)
#' gen()
#' # ab > a > b
#'
#' # went through all partitions, it will only generate NULL now
#' gen <- generateNextPartition(gen)
#' stopifnot(is.null(gen()))
#'
#' @export
generateNextPartition <- function(gen) {
  environment(gen)$nextPartition()
  gen
}



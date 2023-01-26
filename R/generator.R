
#' Generate power relations
#'
#' Based on a list of coalitions, create a generator function that returns a new [`PowerRelation`] object with every call.
#' `NULL` is returned once every possible power relation has been generated.
#'
#' @param coalitions List of coalition vectors. An empty coalition can be set with `c()`.
#'
#' @return A generator function.
#' Every time this generator function is called, a different [`PowerRelation`] object is returned.
#' Once all possible power relations have been generated, the generator function returns `NULL`.
#'
#' @examples
#' coalitions <- createPowerset(c('a','b'), includeEmptySet = FALSE)
#' # list(c('a','b'), 'a', 'b')
#'
#' gen <- powerRelationGenerator(coalitions)
#'
#' pr <- gen()
#' while(!is.null) {
#'   print(pr)
#'   pr <- gen()
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
#' # NULL
#'
#' @export
powerRelationGenerator <- function(coalitions) {
  rlang::check_installed('partitions')

  compositions <- partitions::compositions(length(coalitions))
  compositions <- compositions[,apply(compositions, 2, function(x) {
    zeros <- which(x == 0)
    l <- length(zeros)
    l == 0 || l == (zeros[l] - zeros[1] + 1)
  })]
  r <- nrow(compositions)
  compositions <- compositions[,order(apply(compositions, 2, function(x)
    sum(sapply(seq_along(x), function(i) x[i] * (r + 1 - i)))
  ), decreasing = TRUE)]

  compI <- 1
  part <- Filter(function(x) x != 0, compositions[,1])
  perms <- partitions::multinomial(part)
  partCum <- c(0, cumsum(part))
  permsI <- 0

  # created once, used every time upon generating a new PowerRelation object
  elements <- unique(sort(unlist(coalitions)))
  classes <- c('PowerRelation', if(all(nchar(elements) == 1)) 'SingleCharElements')

  function() {
    if(permsI >= ncol(perms)) {
      if(compI >= ncol(compositions))
        return(NULL)

      compI <<- compI + 1
      part <<- Filter(function(x) x != 0, compositions[,compI])
      perms <<- partitions::multinomial(part)
      partCum <<- c(0, cumsum(part))

      permsI <<- 1
    } else {
      permsI <<- permsI + 1
    }
    comps <- unlist(sapply(part, function(x) c(rep('~', x-1), '>')))

    # This is 15x (!) faster than calling newPowerRelation
    # If structure of PowerRelation changes, it has to change here, too
    structure(list(
      elements = elements,
      rankingCoalitions = coalitions[perms[,permsI]],
      rankingComparators = comps[-length(comps)],
      equivalenceClasses = lapply(seq.int(length(partCum)-1), function(x) {
        coalitions[perms[(partCum[x]+1):partCum[x+1],permsI]]
      })
    ), class = classes)
  }
}



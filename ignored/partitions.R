library(partitions)

permutate <- function(v) {
  X <- NULL
  for (i in seq_along(v)) X <- rbind(X, cbind(v[i], permutate(v[-i])))
  X
}

prGenerator <- function(P) {
  parts <- listParts(length(P))
  perms <- permutate(seq_along(parts[[1]]))
  function() {
    if(length(perms) == 0) {
      if(length(parts) <= 1)
        return(NULL)
      parts <<- parts[-1]
      perms <<- permutate(seq_along(parts[[1]]))
    }

    eqs <- rapply(parts[[1]], function(i) P[i], how = "replace")
    perm <- perms[1,]
    perms <<- perms[-1,,drop=FALSE]
    newPowerRelation(equivalenceClasses = eqs[perm])
  }
}

# for(part in parts) {
#   pr <- newPowerRelation(equivalenceClasses = rapply(unclass(part), function(i) P[i], how="replace"))
#   print(pr)
# }
# x <- c(2,4,6)       # Substitute the vector for which you want partitions
# parts <- listParts(length(x))
# out <- rapply(parts, function(ii) x[ii], how="replace")
#
# # This step is for cosmetic purposes only. It allows you to take advantage of
# # the `print.equivalence` print method when printing the object to a console
# for(i in seq_along(out)) class(out[[i]]) <- c("list", "equivalence")
# out
# newPowerRelation(equivalenceClasses = unlist(listParts(c(3))[[5]], recursive = F))
# listParts(3)

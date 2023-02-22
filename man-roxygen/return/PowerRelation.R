#' @return [`PowerRelation`] object containing the following values:
#' * `$elements`: vector of elements
#' * `$eqs`: equivalence classes. Nested list of lists, each containing vectors representing groups of elements in the same equivalence class
#' * `$coalitionLookup`: `function(v)` taking a coalition vector `v` and returning the equivalence class it belongs to. See [`coalitionLookup()`] for more.
#' * `$elementLookup`: `function(e)` taking an element `e` and returning a list of 2-sized tuples. See [`elementLookup()`] for more.
#'

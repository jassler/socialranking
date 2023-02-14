#' @return [`PowerRelation`] object containing the following values:
#' * `$elements`: vector of elements
#' * `$eqs`: equivalence classes. Nested list of lists, each containing vectors representing groups of elements in the same equivalence class
#' * `$coalitionLookup`: `function(v)` taking a coalition vector `v` as input and returning the equivalence class it belongs to (or a vector of equivalence classes if the coalition appears multiple times).
#' * `$elementLookup`: `function(e)` taking an element `e` as input and returning a list of 2-sized tuples, each representing which equivalence class and which index inside that equivalence class that element appears in.
#'
#' If `$elements`, `$coalitionLookup` and `$elementLookup` are known beforehand, they can be passed to the function to bypass its own possibly resource-intensive initialization process.
#' Since these values are so tightly coupled to the `$eqs` list, however, this is only ever encouraged if the time gains are undoubtedly justified.

test_that("with empty set", {
  l <- createPowerset(1:3)
  expect_equal(l, list(
    c(1,2,3),
    c(1,2),
    c(1,3),
    c(2,3),
    c(1),
    c(2),
    c(3),
    c()
  ))

  result <- evaluate_promise(createPowerset(c("a", "b", "c"), result = 'print'), print = TRUE)
  expect_equal(result$output,
'as.PowerRelation("
  abc
  > ab
  > ac
  > bc
  > a
  > b
  > c
  > {}
")')
})

test_that("without empty set", {
  l <- createPowerset(1:3, includeEmptySet = FALSE)
  expect_equal(l, list(
    c(1,2,3),
    c(1,2),
    c(1,3),
    c(2,3),
    c(1),
    c(2),
    c(3)
  ))

  result <- evaluate_promise(createPowerset(c("a", "b", "c"), includeEmptySet = FALSE, result = 'print'), print = TRUE)
  expect_equal(result$output,
'as.PowerRelation("
  abc
  > ab
  > ac
  > bc
  > a
  > b
  > c
")')
})

test_that("longer names", {
  result <- evaluate_promise(createPowerset(c('ab', 'cd'), result = 'print'), print = TRUE)
  expect_equal(result$output,
'PowerRelation(rlang::list2(
  list(c("ab", "cd")),
  list(c("ab")),
  list(c("cd")),
  list(c()),
))')

  result <- evaluate_promise(createPowerset(c(12, 56), result = 'print'), print = TRUE)
  expect_equal(result$output,
               'PowerRelation(rlang::list2(
  list(c(12, 56)),
  list(c(12)),
  list(c(56)),
  list(c()),
))')
})

test_that("compact", {
  result <- evaluate_promise(createPowerset(c("a", "b", "c"), includeEmptySet = FALSE, result = 'printCompact'), print = TRUE)
  expect_equal(result$output, 'as.PowerRelation("abc > ab > ac > bc > a > b > c")')

  result <- evaluate_promise(createPowerset(c('ab', 'cd'), result = 'printCompact'), print = TRUE)
  expect_equal(result$output, 'PowerRelation(rlang::list2(list(c("ab", "cd")), list(c("ab")), list(c("cd")), list(c())))')
})

test_that("invalid option", {
  expect_error(createPowerset(letters[1:3], result = "invalid"))
})

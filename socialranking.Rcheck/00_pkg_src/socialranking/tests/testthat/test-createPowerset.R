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

  result <- evaluate_promise(createPowerset(c("a", "b", "c"), writeLines = TRUE), print = TRUE)
  expect_equal(result$output,
'newPowerRelation(
  c("a", "b", "c"),
  ">", c("a", "b"),
  ">", c("a", "c"),
  ">", c("b", "c"),
  ">", c("a"),
  ">", c("b"),
  ">", c("c"),
  ">", c(),
)')
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

  result <- evaluate_promise(createPowerset(c("a", "b", "c"), writeLines = TRUE, includeEmptySet = FALSE), print = TRUE)
  expect_equal(result$output,
               'newPowerRelation(
  c("a", "b", "c"),
  ">", c("a", "b"),
  ">", c("a", "c"),
  ">", c("b", "c"),
  ">", c("a"),
  ">", c("b"),
  ">", c("c"),
)')
})

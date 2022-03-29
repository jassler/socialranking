test_that("manip paper example with empty set", {
  pr <- newPowerRelation(
    c(1,2,3),
    "~", c(1,2),
    "~", c(3),
    "~", c(1),
    ">", c(2),
    "~", c(2,3),
    ">", c(1,3)
  )

  result12 <- cpMajorityComparison(pr, 1, 2)
  result13 <- cpMajorityComparison(pr, 1, 3)
  result23 <- cpMajorityComparison(pr, 2, 3)

  expect_null(result12$winner)
  expect_null(result12$loser)
  expect_equal(result13$winner, 1)
  expect_equal(result13$loser, 3)
  expect_null(result23$winner)
  expect_null(result23$loser)
  expect_true(pr %:% 1 %>=cp% 2)
  expect_true(pr %:% 2 %>=cp% 1)
  expect_true(pr %:% 1 %>=cp% 3)
  expect_false(pr %:% 3 %>=cp% 1)
  expect_true(pr %:% 2 %>=cp% 3)
  expect_true(pr %:% 3 %>=cp% 2)

  expect_false(pr %:% 1 %>cp% 2)
  expect_false(pr %:% 2 %>cp% 1)
  expect_true(pr %:% 1 %>cp% 3)
  expect_false(pr %:% 3 %>cp% 1)
  expect_false(pr %:% 2 %>cp% 3)
  expect_false(pr %:% 3 %>cp% 2)

  expect_equal(result12$e1$name, 1)
  expect_equal(result12$e2$name, 2)
  expect_equal(result13$e1$name, 1)
  expect_equal(result13$e2$name, 3)
  expect_equal(result23$e1$name, 2)
  expect_equal(result23$e2$name, 3)

  expect_length(result12$e1$winningCoalitions, 1)
  expect_length(result12$e2$winningCoalitions, 1)
  expect_length(result13$e1$winningCoalitions, 2)
  expect_length(result13$e2$winningCoalitions, 1)
  expect_length(result23$e1$winningCoalitions, 1)
  expect_length(result23$e2$winningCoalitions, 1)

  expect_equal(result12$e1$winningCoalitions[[1]], sets::set())
  expect_equal(result12$e2$winningCoalitions[[1]], sets::set(3))
  expect_equal(result13$e1$winningCoalitions[[1]], sets::set(2))
  expect_equal(result23$e1$winningCoalitions[[1]], sets::set(1))
  expect_equal(result23$e2$winningCoalitions[[1]], sets::set())

  expect_equal(result12$e1$score, 1)
  expect_equal(result12$e2$score, 1)
  expect_equal(result13$e1$score, 2)
  expect_equal(result13$e2$score, 1)
  expect_equal(result23$e1$score, 1)
  expect_equal(result23$e2$score, 1)

  # check score-only function
  expect_equal(cpMajorityComparisonScore(pr, 1, 2), c(result12$e1$score, -result12$e2$score))
  expect_equal(cpMajorityComparisonScore(pr, 2, 1), c(result12$e2$score, -result12$e1$score))
  expect_equal(cpMajorityComparisonScore(pr, 1, 3), c(result13$e1$score, -result13$e2$score))
  expect_equal(cpMajorityComparisonScore(pr, 3, 1), c(result13$e2$score, -result13$e1$score))
  expect_equal(cpMajorityComparisonScore(pr, 2, 3), c(result23$e1$score, -result23$e2$score))
  expect_equal(cpMajorityComparisonScore(pr, 3, 2), c(result23$e2$score, -result23$e1$score))
})


test_that("manip paper example strictly", {
  pr <- newPowerRelation(
    c(1,2,3),
    "~", c(1,2),
    "~", c(3),
    "~", c(1),
    ">", c(2),
    "~", c(2,3),
    ">", c(1,3)
  )

  result12 <- cpMajorityComparison(pr, 1, 2, strictly = TRUE)
  result13 <- cpMajorityComparison(pr, 1, 3, strictly = TRUE)
  result23 <- cpMajorityComparison(pr, 2, 3, strictly = TRUE)

  expect_null(result12$winner)
  expect_null(result12$loser)
  expect_equal(result13$winner, 1)
  expect_equal(result13$loser, 3)
  expect_null(result23$winner)
  expect_null(result23$loser)

  expect_equal(result12$e1$name, 1)
  expect_equal(result12$e2$name, 2)
  expect_equal(result13$e1$name, 1)
  expect_equal(result13$e2$name, 3)
  expect_equal(result23$e1$name, 2)
  expect_equal(result23$e2$name, 3)

  expect_length(result12$e1$winningCoalitions, 1)
  expect_length(result12$e2$winningCoalitions, 1)
  expect_length(result13$e1$winningCoalitions, 1)
  expect_length(result13$e2$winningCoalitions, 0)
  expect_length(result23$e1$winningCoalitions, 1)
  expect_length(result23$e2$winningCoalitions, 1)

  expect_equal(result12$e1$winningCoalitions[[1]], sets::set())
  expect_equal(result12$e2$winningCoalitions[[1]], sets::set(3))
  expect_equal(result13$e1$winningCoalitions[[1]], sets::set(2))
  expect_equal(result23$e1$winningCoalitions[[1]], sets::set(1))
  expect_equal(result23$e2$winningCoalitions[[1]], sets::set())

  expect_equal(result12$e1$score, 1)
  expect_equal(result12$e2$score, 1)
  expect_equal(result13$e1$score, 1)
  expect_equal(result13$e2$score, 0)
  expect_equal(result23$e1$score, 1)
  expect_equal(result23$e2$score, 1)

  # check score-only function
  expect_equal(cpMajorityComparisonScore(pr, 1, 2, strictly = TRUE), c(result12$e1$score, -result12$e2$score))
  expect_equal(cpMajorityComparisonScore(pr, 2, 1, strictly = TRUE), c(result12$e2$score, -result12$e1$score))
  expect_equal(cpMajorityComparisonScore(pr, 1, 3, strictly = TRUE), c(result13$e1$score, -result13$e2$score))
  expect_equal(cpMajorityComparisonScore(pr, 3, 1, strictly = TRUE), c(result13$e2$score, -result13$e1$score))
  expect_equal(cpMajorityComparisonScore(pr, 2, 3, strictly = TRUE), c(result23$e1$score, -result23$e2$score))
  expect_equal(cpMajorityComparisonScore(pr, 3, 2, strictly = TRUE), c(result23$e2$score, -result23$e1$score))
})

test_that("manip paper example without empty set", {
  pr <- newPowerRelation(
    c(1,2,3),
    "~", c(1,2),
    "~", c(3),
    "~", c(1),
    ">", c(2),
    "~", c(2,3),
    ">", c(1,3)
  )

  result12 <- cpMajorityComparison(pr, 1, 2, includeEmptySet = FALSE)
  result13 <- cpMajorityComparison(pr, 1, 3, includeEmptySet = FALSE)
  result23 <- cpMajorityComparison(pr, 2, 3, includeEmptySet = FALSE)

  expect_equal(result12$winner, 2)
  expect_equal(result12$loser, 1)
  expect_equal(result13$winner, 1)
  expect_equal(result13$loser, 3)
  expect_equal(result23$winner, 2)
  expect_equal(result23$loser, 3)

  expect_equal(result12$e1$name, 1)
  expect_equal(result12$e2$name, 2)
  expect_equal(result13$e1$name, 1)
  expect_equal(result13$e2$name, 3)
  expect_equal(result23$e1$name, 2)
  expect_equal(result23$e2$name, 3)

  expect_length(result12$e1$winningCoalitions, 0)
  expect_length(result12$e2$winningCoalitions, 1)
  expect_length(result13$e1$winningCoalitions, 1)
  expect_length(result13$e2$winningCoalitions, 0)
  expect_length(result23$e1$winningCoalitions, 1)
  expect_length(result23$e2$winningCoalitions, 0)

  expect_equal(result12$e2$winningCoalitions[[1]], sets::set(3))
  expect_equal(result13$e1$winningCoalitions[[1]], sets::set(2))
  expect_equal(result23$e1$winningCoalitions[[1]], sets::set(1))

  expect_equal(result12$e1$score, 0)
  expect_equal(result12$e2$score, 1)
  expect_equal(result13$e1$score, 1)
  expect_equal(result13$e2$score, 0)
  expect_equal(result23$e1$score, 1)
  expect_equal(result23$e2$score, 0)

  # check score-only function
  expect_equal(cpMajorityComparisonScore(pr, 1, 2, includeEmptySet = FALSE), c(result12$e1$score, -result12$e2$score))
  expect_equal(cpMajorityComparisonScore(pr, 2, 1, includeEmptySet = FALSE), c(result12$e2$score, -result12$e1$score))
  expect_equal(cpMajorityComparisonScore(pr, 1, 3, includeEmptySet = FALSE), c(result13$e1$score, -result13$e2$score))
  expect_equal(cpMajorityComparisonScore(pr, 3, 1, includeEmptySet = FALSE), c(result13$e2$score, -result13$e1$score))
  expect_equal(cpMajorityComparisonScore(pr, 2, 3, includeEmptySet = FALSE), c(result23$e1$score, -result23$e2$score))
  expect_equal(cpMajorityComparisonScore(pr, 3, 2, includeEmptySet = FALSE), c(result23$e2$score, -result23$e1$score))
})

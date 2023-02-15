test_that("basic", {
  pr <- PowerRelation(list(list(c(2,1)),list(1),list(2)))
  expect_equal(pr$elements, c(1, 2))
  expect_equal(pr$eqs, list(list(c(1,2)), list(c(1)), list(c(2))))

  expect_equal(pr$coalitionLookup(c(1,2)), 1)
  expect_equal(pr$coalitionLookup(1), 2)
  expect_equal(pr$coalitionLookup(2), 3)

  expect_equal(pr$elementLookup(1), list(c(1,1), c(2,1)))
  expect_equal(pr$elementLookup(2), list(c(1,1), c(3,1)))
})

test_that("element types", {
  pr <- PowerRelation(list(list(c("a",2)), list("a", 2)))
  expect_equal(pr$elements, c('2','a'))
  expect_equal(pr$eqs, list(list(c('2','a')), list(c('a'), c(2))))
})

test_that("too few coalitions error", {
  expect_error(PowerRelation(list(list())), "Power relation must contain at least two coalitions and cannot have empty equivalence classes.")
  expect_error(PowerRelation(list(list(1))), "Power relation must contain at least two coalitions.")
  expect_error(PowerRelation(list(list(1), list(), list(2))), "Power relation must contain at least two coalitions and cannot have empty equivalence classes.")
})

test_that("from string", {
  pr <- as.PowerRelation("12 > 1 ~ 2")
  expect_equal(pr, PowerRelation(list(list(c(1,2)), list(1, 2))))

  pr <- as.PowerRelation("ab > a ~ b")
  expect_equal(pr, PowerRelation(list(list(c("a","b")), list("a", "b"))))


  pr <- as.PowerRelation("12 > (1 ~ 2).:;-*/\\")
  expect_equal(pr, PowerRelation(list(list(c(1,2)), list(1, 2))))

  pr <- as.PowerRelation("a ~ {} > b")
  expect_equal(pr, PowerRelation(list(list("a", c()), list("b"))))

  pr <- as.PowerRelation("{} > a > b")
  expect_equal(pr, PowerRelation(list(list(c()), list("a"), list("b"))))
})


test_that("output", {
  result <- evaluate_promise(as.PowerRelation("1 > 2 ~ 12"), print = TRUE)
  expect_equal(result$output, "1 > (2 ~ 12)")


  result <- evaluate_promise(PowerRelation(list(list(c('de','at')),list('b',c('de','b')))), print = TRUE)
  expect_equal(result$output, "{at, de} > ({b} ~ {b, de})")
})


test_that("cycle warning", {
  result <- evaluate_promise(as.PowerRelation("1 > 1"))
  expect_length(result$warnings, 1)
  expect_equal(result$warnings[1], "Found 1 duplicate coalition, listed below. This violates transitivity and can cause issues with certain ranking solutions. You may want to take a look at socialranking::transitiveClosure().
    - 1")

  result <- evaluate_promise(as.PowerRelation("1 > 1 ~ 2 > 2"))
  expect_length(result$warnings, 1)
  expect_equal(result$warnings[1], "Found 2 duplicate coalitions, listed below. This violates transitivity and can cause issues with certain ranking solutions. You may want to take a look at socialranking::transitiveClosure().
    - 1
    - 2")

  result <- evaluate_promise(as.PowerRelation("12 > 12 ~ 123 > 12"))
  expect_length(result$warnings, 1)
  expect_equal(result$warnings[1], "Found 1 duplicate coalition, listed below. This violates transitivity and can cause issues with certain ranking solutions. You may want to take a look at socialranking::transitiveClosure().
    - c(1, 2)")
})

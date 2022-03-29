test_that("works", {
  pr <- newPowerRelation(c(1,2), ">", 1, ">", 2)
  expect_equal(equivalenceClassIndex(pr, c(1,2)), 1)
  expect_equal(equivalenceClassIndex(pr, 1), 2)
  expect_equal(equivalenceClassIndex(pr, 2), 3)

  pr <- newPowerRelation(c(1,2), "~", 1, "~", 2, ">", c())
  expect_equal(equivalenceClassIndex(pr, c(1,2)), 1)
  expect_equal(equivalenceClassIndex(pr, 1), 1)
  expect_equal(equivalenceClassIndex(pr, 2), 1)
  expect_equal(equivalenceClassIndex(pr, c()), 2)
})

test_that("stops", {
  pr <- newPowerRelation(c(1,2), ">", 1, ">", 2)
  expect_error(equivalenceClassIndex(pr, 3), "The coalition {3} does not appear in the power relation", fixed = TRUE)
  expect_error(equivalenceClassIndex(pr, c()), "The coalition {} does not appear in the power relation", fixed = TRUE)

  expect_equal(equivalenceClassIndex(pr, 3, stopIfNotExists = FALSE), -1)
  expect_equal(equivalenceClassIndex(pr, c(), stopIfNotExists = FALSE), -1)
})

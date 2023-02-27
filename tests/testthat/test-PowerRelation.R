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

test_that("empty eqs exception", {
  expect_error(PowerRelation(list()))
  expect_error(PowerRelation(list(list())))
  expect_error(PowerRelation(list(list(1), list(), list(2))))
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

test_that("from list", {
  pr <- as.PowerRelation(list(c(1,2), 1, 2))
  expect_equal(pr, PowerRelation(list(list(c(1,2)), list(1), list(2))))

  pr <- as.PowerRelation(list(c(1,2), 1, 2), comparators = '>')
  expect_equal(pr, PowerRelation(list(list(c(1,2)), list(1), list(2))))

  pr <- as.PowerRelation(list(c(1,2), 1, 2), comparators = '~')
  expect_equal(pr, PowerRelation(list(list(c(1,2), 1, 2))))

  pr <- as.PowerRelation(list(c(1,2), 1, 2), comparators = c('>', '~'))
  expect_equal(pr, PowerRelation(list(list(c(1,2)), list(1, 2))))
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
    - {1}")

  result <- evaluate_promise(as.PowerRelation("1 > 1 ~ 2 > 2"))
  expect_length(result$warnings, 1)
  expect_equal(result$warnings[1], "Found 2 duplicate coalitions, listed below. This violates transitivity and can cause issues with certain ranking solutions. You may want to take a look at socialranking::transitiveClosure().
    - {1}
    - {2}")

  result <- evaluate_promise(as.PowerRelation("12 > 12 ~ 123 > 12"))
  expect_length(result$warnings, 1)
  expect_equal(result$warnings[1], "Found 1 duplicate coalition, listed below. This violates transitivity and can cause issues with certain ranking solutions. You may want to take a look at socialranking::transitiveClosure().
    - {1, 2}")

  result <- evaluate_promise(PowerRelation(list(list(c("ab", "cd", "ef")), list(c("ab", "cd", "ef")))))
  expect_length(result$warnings, 1)
  expect_equal(result$warnings[1], "Found 1 duplicate coalition, listed below. This violates transitivity and can cause issues with certain ranking solutions. You may want to take a look at socialranking::transitiveClosure().
    - {ab, cd, ef}")
})

test_that("duplicate elements", {
  result <- evaluate_promise(as.PowerRelation("11 > 12"))
  expect_length(result$warnings, 1)
  expect_equal(result$warnings[1], "Found 1 coalition that contain elements more than once.\n    - 1 in the coalition {1, 1}")

  result <- evaluate_promise(as.PowerRelation("11 > 121 > 313133"))
  expect_length(result$warnings, 1)
  expect_equal(result$warnings[1], "Found 3 coalitions that contain elements more than once.\n    - 1 in the coalition {1, 1}\n    - 1 in the coalition {1, 1, 2}\n    - 1, 3 in the coalition {1, 1, 3, 3, 3, 3}")

  result <- evaluate_promise(as.PowerRelation("121 > 123 > 121"))
  expect_length(result$warnings, 2)
})

test_that("equality", {
  expect_true(
    as.PowerRelation("1 ~ 12 ~ 3") ==
    as.PowerRelation("12 ~ 3 ~ 1")
  )
  expect_true(
    as.PowerRelation("12 > 1 ~ 2") ==
    as.PowerRelation("12 > 2 ~ 1")
  )

  expect_false(
    as.PowerRelation("12 > 1 ~ 2") ==
    as.PowerRelation("1 > 12 ~ 2")
  )
})

test_that("coalitionsAreIndifferent", {
  pr <- as.PowerRelation("123 ~ 2 > 12 ~ 1 ~ 3 > {}")
  expect_true(coalitionsAreIndifferent(pr, c(1,2,3), 2))
  expect_true(coalitionsAreIndifferent(pr, 2, c(1,2,3)))
  expect_false(coalitionsAreIndifferent(pr, 2, c()))
  expect_false(coalitionsAreIndifferent(pr, 3, c(1,2,3)))
})

test_that("equivalenceClassIndex", {
  pr <- as.PowerRelation("123 ~ 2 > 12 ~ 1 ~ 3 > {}")
  expect_equal(equivalenceClassIndex(pr, c(1,2)), 2)
  expect_equal(equivalenceClassIndex(pr, c(2,1)), 2)
  expect_null(equivalenceClassIndex(pr, c(1,3)))
  expect_equal(equivalenceClassIndex(pr, c()), 3)
  expect_equal(equivalenceClassIndex, coalitionLookup)
})

test_that("elementLookup", {
  pr <- as.PowerRelation("123 ~ 2 > 12 ~ 1 ~ 3 > {}")
  expect_equal(elementLookup(pr, 1), list(c(1,1), c(2,1), c(2,2)))
  expect_equal(elementLookup(pr, 2), list(c(1,1), c(1,2), c(2,1)))
  expect_equal(elementLookup(pr, 3), list(c(1,1), c(2,3)))
  expect_null(elementLookup(pr, 4))

})

test_that("deprecated", {
  pr <- as.PowerRelation("123 ~ 2 > 12 ~ 1 ~ 3 > {}")
  expect_error(newPowerRelation(c(1,2), '>', 2))
  expect_error(newPowerRelationFromString("123 ~ 2 > 12 ~ 1 ~ 3 > {}"))
})

test_that("Inequality from differently sized lists", {
  expect_false({
    as.PowerRelation("12 > (13 ~ 1) >( 23 ~ 123 ~ 2 ~ 3)") ==
    as.PowerRelation("12 > (13 ~ 1 ~ 23 ~ 123 ~ 2 ~ 3)")
  })
  expect_false({
    as.PowerRelation("12 > (13 ~ 1) > (23 ~ 123 ~ 2 ~ 3)") ==
    as.PowerRelation("(12 ~ 13) > 1 > (23 ~ 123 ~ 2 ~ 3)")
  })
})

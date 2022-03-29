test_that("ellipsis", {
  pr <- newPowerRelation(c(1,2), ">", 1, ">", 2)
  expect_equal(pr$rankingCoalitions, list(sets::set(1,2), sets::set(1), sets::set(2)))
  expect_equal(pr$rankingComparators, c(">", ">"))
  expect_equal(pr$elements, c(1, 2))
  expect_equal(pr$equivalenceClasses, list(list(sets::set(1,2)), list(sets::set(1)), list(sets::set(2))))
})

test_that("list", {
  pr <- newPowerRelation(list(c(1,2), ">", 1, ">", 2))
  expect_equal(pr$rankingCoalitions, list(sets::set(1,2), sets::set(1), sets::set(2)))
  expect_equal(pr$rankingComparators, c(">", ">"))
  expect_equal(pr$elements, c(1, 2))
  expect_equal(pr$equivalenceClasses, list(list(sets::set(1,2)), list(sets::set(1)), list(sets::set(2))))
})

test_that("element types", {
  pr <- newPowerRelation(list(sets::set("a",2), ">", "a", "~", 2))
  expect_equal(pr$rankingCoalitions, list(sets::set("a", 2), sets::set("a"), sets::set(2)))
  expect_equal(pr$rankingComparators, c(">", "~"))
  expect_equal(pr$elements, sort(c("a", "2")))
  expect_equal(pr$equivalenceClasses, list(list(sets::set("a",2)), list(sets::set("a"), sets::set(2))))
})

test_that("too few coalitions error", {
  expect_error(newPowerRelation(), "ranking parameter must have at least 3 items, got 0")
  expect_error(newPowerRelation(1), "ranking parameter must have at least 3 items, got 1")
  expect_error(newPowerRelation(1,">"), "ranking parameter must have at least 3 items, got 2")
})

test_that("invalid arguments error", {
  expect_error(newPowerRelation(1, ">", ">", 2), "ranking parameter must be a list where the length is uneven")
  expect_error(newPowerRelation(1, ">", 2, "=", 3), "Each coalition in ranking list must be separated by a '>' or '~' character, got: >, =")
})

test_that("other arguments", {
  pr <- newPowerRelation(rankingCoalitions = list(c(1,2), c(1), c(2)))
  expect_equal(pr$rankingCoalitions, list(sets::set(1,2), sets::set(1), sets::set(2)))
  expect_equal(pr$rankingComparators, c(">", ">"))

  pr <- newPowerRelation(rankingCoalitions = list(c(1,2), c(1), c(2)), rankingComparators = c(">", "~"))
  expect_equal(pr$rankingCoalitions, list(sets::set(1,2), sets::set(1), sets::set(2)))
  expect_equal(pr$rankingComparators, c(">", "~"))

  pr <- newPowerRelation(rankingCoalitions = list(c(1,2), c(1), c(2)), rankingComparators = "~")
  expect_equal(pr$rankingCoalitions, list(sets::set(1,2), sets::set(1), sets::set(2)))
  expect_equal(pr$rankingComparators, c("~", "~"))

  pr <- newPowerRelation(rankingCoalitions = list(c(1,2), c(1), c(2)), rankingComparators = c("~", ">", ">"))
  expect_equal(pr$rankingCoalitions, list(sets::set(1,2), sets::set(1), sets::set(2)))
  expect_equal(pr$rankingComparators, c("~", ">"))
})


test_that("from string", {
  pr <- newPowerRelationFromString("12 > 1 ~ 2", asWhat = as.numeric)
  expect_equal(pr, newPowerRelation(c(1,2), ">", 1, "~", 2))

  pr <- newPowerRelationFromString("ab > a ~ b")
  expect_equal(pr, newPowerRelation(c("a","b"), ">", "a", "~", "b"))


  pr <- newPowerRelationFromString("12 > (1 ~ 2).:;-*/\\", asWhat = as.numeric)
  expect_equal(pr, newPowerRelation(c(1,2), ">", 1, "~", 2))

  pr <- newPowerRelationFromString("a ~ {} > b")
  expect_equal(pr, newPowerRelation(c("a"), "~", c(), ">", "b"))
})


test_that("output", {
  result <- evaluate_promise(newPowerRelationFromString("1 > 2 ~ 12"), print = TRUE)
  expect_equal(result$output, "Elements: 1 2
1 > (2 ~ 12)")


  result <- evaluate_promise(newPowerRelation(c("de", "at"), ">", "b", "~", c("de", "b")), print = TRUE)
  expect_equal(result$output, "Elements: at b de
{at, de} > ({b} ~ {b, de})")
})


test_that("cycle warning", {
  result <- evaluate_promise(newPowerRelationFromString("1 > 1"))
  expect_length(result$warnings, 1)
  expect_equal(result$warnings[1], "Found the following duplicate. Did you mean to introduce cycles?
  {1}")

  result <- evaluate_promise(newPowerRelationFromString("1 > 1 ~ 2 > 2"))
  expect_length(result$warnings, 1)
  expect_equal(result$warnings[1], "Found the following duplicates. Did you mean to introduce cycles?
  {1}
  {2}")

  result <- evaluate_promise(newPowerRelationFromString("12 > 12 ~ 123 > 12"))
  expect_length(result$warnings, 1)
  expect_equal(result$warnings[1], "Found the following duplicate. Did you mean to introduce cycles?
  {1, 2}")
})

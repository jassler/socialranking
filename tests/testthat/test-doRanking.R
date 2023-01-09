test_that("doRanking works", {
  pr <- newPowerRelationFromString("12 > 1 > 3 > 23 > 123 ~ 13", asWhat = as.numeric)

  result <- evaluate_promise(doRanking(c(1, 2, 3)), print = TRUE)
  expect_equal(result$output, "3 > 2 > 1")

  result <- evaluate_promise(doRanking(c(1, 2, 3), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "1 > 2 > 3")

  result <- evaluate_promise(doRanking(c(1, 1, 1), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "1 ~ 2 ~ 3")

  result <- evaluate_promise(doRanking(c(3, 1, 3), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "2 > 1 ~ 3")

  result <- evaluate_promise(doRanking(c(3, 3, 1), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "3 > 1 ~ 2")

  result <- evaluate_promise(doRanking(c(3, 1, 1), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "2 ~ 3 > 1")
})

test_that("doRanking named", {
  result <- evaluate_promise(doRanking(structure(c(1, 2, 3), names = c("Bacon", "Egg", "Salt"))), print = TRUE)
  expect_equal(result$output, "Salt > Egg > Bacon")

  result <- evaluate_promise(doRanking(structure(c(1, 2, 3), names = c("Bacon", "Egg", "Salt")), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "Bacon > Egg > Salt")

  result <- evaluate_promise(doRanking(structure(c(1, 1, 1), names = c("Bacon", "Egg", "Salt")), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "Bacon ~ Egg ~ Salt")

  result <- evaluate_promise(doRanking(structure(c(3, 1, 3), names = c("Bacon", "Egg", "Salt")), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "Egg > Bacon ~ Salt")

  result <- evaluate_promise(doRanking(structure(c(3, 3, 1), names = c("Bacon", "Egg", "Salt")), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "Salt > Bacon ~ Egg")

  result <- evaluate_promise(doRanking(structure(c(3, 1, 1), names = c("Bacon", "Egg", "Salt")), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "Egg ~ Salt > Bacon")
})

test_that("doRanking compare", {
  result <- evaluate_promise(doRanking(c(1,2,3), compare = function(a,b) a - b), print = TRUE)
  expect_equal(result$output, "3 > 2 > 1")

  result <- evaluate_promise(doRanking(c(1,2,3), compare = function(a,b) b - a), print = TRUE)
  expect_equal(result$output, "1 > 2 > 3")

  result <- evaluate_promise(doRanking(c(1,2,3), compare = function(a,b) a - a), print = TRUE)
  expect_equal(result$output, "1 ~ 2 ~ 3")
})

test_that("doRanking works", {
  pr <- newPowerRelationFromString("12 > 1 > 3 > 23 > 123 ~ 13", asWhat = as.numeric)

  result <- evaluate_promise(doRanking(pr, c(1, 2, 3)), print = TRUE)
  expect_equal(result$output, "3 > 2 > 1")

  result <- evaluate_promise(doRanking(pr, c(1, 2, 3), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "1 > 2 > 3")

  result <- evaluate_promise(doRanking(pr, c(1, 1, 1), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "1 ~ 2 ~ 3")

  result <- evaluate_promise(doRanking(pr, c(3, 1, 3), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "2 > 1 ~ 3")

  result <- evaluate_promise(doRanking(pr, c(3, 3, 1), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "3 > 1 ~ 2")

  result <- evaluate_promise(doRanking(pr, c(3, 1, 1), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "2 ~ 3 > 1")

  result <- evaluate_promise(doRanking(pr, c(1, -1, -2), isIndifferent = function(a, b) abs(a) == abs(b)), print = TRUE)
  expect_equal(result$output, "1 ~ 2 > 3")
})

test_that("doRanking named", {
  pr <- newPowerRelation(
    c("Salt"),
    ">", c("Bacon"),
    ">", c("Egg"),
    ">", c("Bacon", "Egg"),
    ">", c("Egg", "Salt"),
    ">", c("Bacon", "Egg", "Salt"),
    ">", c("Bacon", "Salt"),
    ">", c(),
  )

  result <- evaluate_promise(doRanking(pr, c(1, 2, 3)), print = TRUE)
  expect_equal(result$output, "Salt > Egg > Bacon")

  result <- evaluate_promise(doRanking(pr, c(1, 2, 3), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "Bacon > Egg > Salt")

  result <- evaluate_promise(doRanking(pr, c(1, 1, 1), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "Bacon ~ Egg ~ Salt")

  result <- evaluate_promise(doRanking(pr, c(3, 1, 3), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "Egg > Bacon ~ Salt")

  result <- evaluate_promise(doRanking(pr, c(3, 3, 1), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "Salt > Bacon ~ Egg")

  result <- evaluate_promise(doRanking(pr, c(3, 1, 1), decreasing = FALSE), print = TRUE)
  expect_equal(result$output, "Egg ~ Salt > Bacon")

  result <- evaluate_promise(doRanking(pr, c(1, -1, -2), isIndifferent = function(a, b) abs(a) == abs(b)), print = TRUE)
  expect_equal(result$output, "Bacon ~ Egg > Salt")
})

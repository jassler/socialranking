test_that("doRanking works", {
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

test_that("Rank lists", {
  result <- evaluate_promise(doRanking(list(
    a = c(1, 2, 3),
    b = c(1, 3, 3),
    c = c(2, 2, 3)
  ), compare = function(a, b) {
    i <- rev(which(a != b))
    if(length(i) == 0) 0
    else a[i[1]] - b[i[1]]
  }), print = TRUE)
  expect_equal(result$output, "b > c > a")
})

test_that("Equality", {
  expect_true(
    doRanking(c(a = 3, b = 1, c = 1)) ==
    doRanking(c(b = 1, a = 3, c = 1))
  )

  expect_false(
    doRanking(c(a = 3, b = 1, c = 1)) ==
    doRanking(c(b = 3, a = 1, c = 1))
  )
})

test_that("doRanking compare", {
  result <- evaluate_promise(doRanking(c(1,2,3), compare = function(a,b) a - b), print = TRUE)
  expect_equal(result$output, "3 > 2 > 1")

  result <- evaluate_promise(doRanking(c(1,2,3), compare = function(a,b) b - a), print = TRUE)
  expect_equal(result$output, "1 > 2 > 3")

  result <- evaluate_promise(doRanking(c(1,2,3), compare = function(a,b) a - a), print = TRUE)
  expect_equal(result$output, "1 ~ 2 ~ 3")

  l <- list(a = c(3, 1), b = c(9, 4, 2), c = c(4))
  result <- evaluate_promise(doRanking(l, compare = function(a, b) sum(a) - sum(b)), print = TRUE)
  expect_equal(result$output, "b > a ~ c")

  result <- evaluate_promise(doRanking(l, compare = function(a, b) sum(b) - sum(a)), print = TRUE)
  expect_equal(result$output, "a ~ c > b")
})

test_that("Inequality from differently sized objects", {
  expect_false(
    doRanking(c(a = 3, b = 2, c = 1)) ==
    doRanking(c(a = 3, b = 2))
  )
  expect_false(
    doRanking(c(a = 2, b = 2, c = 1)) ==
      doRanking(c(a = 2, b = 1, c = 1))
  )
})

test_that("SocialRanking()", {
  result <- evaluate_promise(SocialRanking(list(c("a", "b"), "e", c("c", "d"))), print = TRUE)
  expect_equal(result$output, "a ~ b > e > c ~ d")

  result <- evaluate_promise(SocialRanking(list(12, 34)), print = TRUE)
  expect_equal(result$output, "12 > 34")
})

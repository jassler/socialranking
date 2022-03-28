test_that("ranking", {
  pr <- newPowerRelation(c(1,2), ">", 1, ">", 2)
  ranking <- evaluate_promise(lexcelRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 > 2")
})

test_that("score vectors", {
  pr <- newPowerRelation(c(1,2), ">", 1, ">", 2)
  expect_equal(
    lexcelScores(pr),
    structure(list(
      `1` = c(1,1,0),
      `2` = c(1,0,1)
    ), class = "LexcelScores")
  )
})

test_that("manip paper works", {
  pr <- newPowerRelation(
    c(2,3),
    ">", c(1,2,3),
    "~", c(1,2),
    ">", c(1,3),
  )

  expect_equal(
    lexcelScores(pr),
    structure(list(
      `1` = c(0,2,1),
      `2` = c(1,2,0),
      `3` = c(1,1,1)
    ), class = "LexcelScores")
  )
})

test_that("dualLexcel", {
  pr <- newPowerRelation(c(1,2), ">", 1, ">", 2)
  ranking <- evaluate_promise(dualLexcelRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 > 2")

  pr <- newPowerRelation(c(1,2), "~", 1, ">", 2, ">", c(1,3))
  ranking <- evaluate_promise(dualLexcelRanking(pr), print = TRUE)
  expect_equal(ranking$output, "2 > 3 > 1")

  ranking <- evaluate_promise(lexcelRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 > 2 > 3")
})

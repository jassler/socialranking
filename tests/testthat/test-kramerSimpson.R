test_that("manip paper example", {
  pr <- newPowerRelation(
    2,
    ">", 1,
    "~", 3,
    ">", c(1,2),
    ">", c(1,3),
    "~", c(2,3),
    ">", c(),
    ">", c(1,2,3)
  )

  ranking <- evaluate_promise(kramerSimpsonRanking(pr), print = TRUE)
  expect_equal(ranking$output, "2 > 1 > 3")
  expect_equal(kramerSimpsonScores(pr), structure(list(
    `1` = 1,
    `2` = -1,
    `3` = 2
  ), class = "KramerSimpsonScores"))
})

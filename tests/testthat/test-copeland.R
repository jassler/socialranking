test_that("scores", {
  pr <- newPowerRelation(c(1,2), ">", 1, "~", 2)
  expect_equal(copelandScores(pr), structure(list(`1` = c(1,-1), `2` = c(1,-1)), class = "CopelandScores"))
  expect_equal(copelandScores(pr, 1), structure(list(`1` = c(1,-1)), class = "CopelandScores"))
  expect_equal(copelandScores(pr, 2), structure(list(`2` = c(1,-1)), class = "CopelandScores"))

  pr <- newPowerRelation(c(1,2), ">", 1, ">", 2)
  expect_equal(copelandScores(pr), structure(list(`1` = c(1,0), `2` = c(0,-1)), class = "CopelandScores"))
  expect_equal(copelandScores(pr, 1), structure(list(`1` = c(1,0)), class = "CopelandScores"))
  expect_equal(copelandScores(pr, 2), structure(list(`2` = c(0,-1)), class = "CopelandScores"))

  pr <- newPowerRelation(c(1,2), ">", 2, ">", 1)
  expect_equal(copelandScores(pr), structure(list(`1` = c(0,-1), `2` = c(1,0)), class = "CopelandScores"))
  expect_equal(copelandScores(pr, 1), structure(list(`1` = c(0,-1)), class = "CopelandScores"))
  expect_equal(copelandScores(pr, 2), structure(list(`2` = c(1,0)), class = "CopelandScores"))
})

test_that("ranking", {
  pr <- newPowerRelation(c(1,2), ">", 1, "~", 2)
  ranking <- evaluate_promise(copelandRanking(pr), print = TRUE)
  expect_equal(ranking$result, structure(list(c(1, 2)), class = "SocialRankingSolution"))
  expect_equal(ranking$output, "1 ~ 2")
  expect_true(pr %:% 1 %>=cop% 2)
  expect_true(pr %:% 2 %>=cop% 1)
  expect_false(pr %:% 1 %>cop% 2)
  expect_false(pr %:% 2 %>cop% 1)

  pr <- newPowerRelation(c(1,2), ">", 1, ">", 2)
  ranking <- evaluate_promise(copelandRanking(pr), print = TRUE)
  expect_equal(ranking$result, structure(list(1, 2), class = "SocialRankingSolution"))
  expect_equal(ranking$output, "1 > 2")
  expect_true(pr %:% 1 %>=cop% 2)
  expect_false(pr %:% 2 %>=cop% 1)
})

test_that("manip paper example", {
  pr <- newPowerRelation(
    c(1,2,3),
    "~", c(1,2),
    "~", c(3),
    "~", c(1),
    ">", c(2),
    "~", c(2,3),
    ">", c(1,3)
  )

  ranking <- evaluate_promise(copelandRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 > 2 > 3")
  expect_true(pr %:% 1 %>=cop% 2)
  expect_true(pr %:% 1 %>=cop% 3)
  expect_true(pr %:% 2 %>=cop% 3)
  expect_false(pr %:% 2 %>=cop% 1)
  expect_false(pr %:% 3 %>=cop% 1)
  expect_false(pr %:% 3 %>=cop% 2)
  expect_true(pr %:% 3 %>=cop% 3)
  expect_equal(ranking$result, structure(list(1, 2, 3), class = "SocialRankingSolution"))

  expect_equal(copelandScores(pr), structure(list(`1` = c(2,-1), `2` = c(2,-2), `3` = c(1,-2)), class = "CopelandScores"))
})

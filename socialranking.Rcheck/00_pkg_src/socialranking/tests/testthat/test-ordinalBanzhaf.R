test_that("ranking", {
  pr <- newPowerRelation(c(1,2), ">", 1, ">", 2)
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 ~ 2")
  expect_true(pr %:% 1 %>=banz% 2)
  expect_true(pr %:% 2 %>=banz% 1)
  expect_false(pr %:% 1 %>banz% 2)
  expect_false(pr %:% 2 %>banz% 1)

  pr <- newPowerRelation(1, ">", 2, ">", c(1,2))
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 ~ 2")

  pr <- newPowerRelation(c(1,2), ">", 1, ">", c(), ">", 2)
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 > 2")
  expect_true(pr %:% 1 %>=banz% 2)
  expect_false(pr %:% 2 %>=banz% 1)
  expect_true(pr %:% 1 %>banz% 2)
  expect_false(pr %:% 2 %>banz% 1)
  expect_false(pr %:% 1 %>banz% 1)

  pr <- newPowerRelation(c(1,2), ">", 1, "~", c(), ">", 2)
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 > 2")

  pr <- newPowerRelation(c(1,2), ">", 1, ">", c(), "~", 2)
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 > 2")

  pr <- newPowerRelation(c(1,2), ">", 1, "~", c(), "~", 2)
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 ~ 2")



  pr <- newPowerRelation(c(1,2), ">", 2, ">", 1)
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 ~ 2")

  pr <- newPowerRelation(2, ">", 1, ">", c(1,2))
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 ~ 2")

  pr <- newPowerRelation(c(1,2), ">", 2, ">", c(), ">", 1)
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "2 > 1")

  pr <- newPowerRelation(c(1,2), ">", 2, "~", c(), ">", 1)
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "2 > 1")

  pr <- newPowerRelation(c(1,2), ">", 2, ">", c(), "~", 1)
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "2 > 1")

  pr <- newPowerRelation(c(1,2), ">", 2, "~", c(), "~", 1)
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 ~ 2")
})

test_that("scores", {
  pr <- newPowerRelation(c(1,2), ">", 1, ">", 2)
  expect_equal(ordinalBanzhafScores(pr), structure(list(
    `1` = c(1,0),
    `2` = c(1,0)
  ), class = "OrdinalBanzhafScores"))

  pr <- newPowerRelation(c(1,2), ">", 2, ">", c(), ">", 1)
  expect_equal(ordinalBanzhafScores(pr), structure(list(
    `1` = c(1,-1),
    `2` = c(2,0)
  ), class = "OrdinalBanzhafScores"))

  pr <- newPowerRelation(c(1,2), ">", 2, "~", c(), ">", 1)
  expect_equal(ordinalBanzhafScores(pr), structure(list(
    `1` = c(1,-1),
    `2` = c(1,0)
  ), class = "OrdinalBanzhafScores"))
})

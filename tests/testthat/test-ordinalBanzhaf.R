test_that("banz ranking", {
  pr <- as.PowerRelation('12 > 1 > 2')
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 ~ 2")
  expect_true(pr %:% 1 %>=banz% 2)
  expect_true(pr %:% 2 %>=banz% 1)
  expect_false(pr %:% 1 %>banz% 2)
  expect_false(pr %:% 2 %>banz% 1)

  pr <- as.PowerRelation('1 > 2 > 12')
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 ~ 2")

  pr <- as.PowerRelation('12 > 1 > {} > 2')
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 > 2")
  expect_true(pr %:% 1 %>=banz% 2)
  expect_false(pr %:% 2 %>=banz% 1)
  expect_true(pr %:% 1 %>banz% 2)
  expect_false(pr %:% 2 %>banz% 1)
  expect_false(pr %:% 1 %>banz% 1)

  pr <- as.PowerRelation('12 > 1 ~ {} > 2')
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 > 2")

  pr <- as.PowerRelation('12 > 1 > {} ~ 2')
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 > 2")

  pr <- as.PowerRelation('12 > 1 ~ {} ~ 2')
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 ~ 2")



  pr <- as.PowerRelation('12 > 2 > 1')
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 ~ 2")

  pr <- as.PowerRelation('2 > 1 > 12')
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 ~ 2")

  pr <- as.PowerRelation('12 > 2 > {} > 1')
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "2 > 1")

  pr <- as.PowerRelation('12 > 2 ~ {} > 1')
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "2 > 1")

  pr <- as.PowerRelation('12 > 2 > {} ~ 1')
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "2 > 1")

  pr <- as.PowerRelation('12 > 2 ~ {} ~ 1')
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 ~ 2")
})

test_that("banz scores", {
  pr <- as.PowerRelation('12 > 1 > 2')
  expect_equal(ordinalBanzhafScores(pr), structure(list(
    `1` = c(1,0,1),
    `2` = c(1,0,1)
  ), class = "OrdinalBanzhafScores"))

  pr <- as.PowerRelation('12 > 2 > {} > 1')
  expect_equal(ordinalBanzhafScores(pr), structure(list(
    `1` = c(1,-1,0),
    `2` = c(2,0,0)
  ), class = "OrdinalBanzhafScores"))

  pr <- as.PowerRelation('12 > 2 ~ {} > 1')
  expect_equal(ordinalBanzhafScores(pr), structure(list(
    `1` = c(1,-1,0),
    `2` = c(1,0,0)
  ), class = "OrdinalBanzhafScores"))
})

test_that('banz named', {
  pr <- as.PowerRelation('abc > bc > ac > a > b > c > ab > {}')
  ranking <- evaluate_promise(ordinalBanzhafRanking(pr), print = TRUE)
  expect_equal(ranking$output, 'c > a ~ b')

  # abc~ab~a~c > b~bc > ac
  pr <- PowerRelation(list(
    list(c("Apple","Banana","Citrus"), c("Apple","Banana"), c("Citrus"), c("Apple")),
    list(c("Banana"), c("Banana","Citrus")),
    list(c("Apple","Citrus"))
  ))
  ranking <- evaluate_promise(lexcelRanking(pr), print = TRUE)
  expect_equal(ranking$output, 'Apple > Banana > Citrus')
})

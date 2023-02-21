test_that("works", {
  pr <- as.PowerRelation("(1234 ~ 12 ~ 2 ~ 134) > (23 ~ 14) > 234 > 4 > (3 ~ 1) > 123 > {} > (124 ~ 13 ~ 24 ~ 34)")
  scores <- L1Scores(pr)

  expect_s3_class(scores, 'L1Scores')
  expect_equal(scores$`1`, matrix(c(
    0, 0, 0, 0, 1, 0, 0, 0,
    1, 1, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 1, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0
  ), nrow = 4, byrow = TRUE))
  expect_equal(scores$`2`, matrix(c(
    1, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 0, 0, 0, 0, 0, 1,
    0, 0, 1, 0, 0, 1, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0
  ), nrow = 4, byrow = TRUE))
  expect_equal(scores$`3`, matrix(c(
    0, 0, 0, 0, 1, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 2,
    1, 0, 1, 0, 0, 1, 0, 0,
    1, 0, 0, 0, 0, 0, 0, 0
  ), nrow = 4, byrow = TRUE))
  expect_equal(scores$`4`, matrix(c(
    0, 0, 0, 1, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 2,
    1, 0, 1, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0
  ), nrow = 4, byrow = TRUE))

  # 2 > 1 > 4 > 3
  expect_equal(L1Ranking(pr), doRanking(c(`1` = 3, `2` = 4, `3` = 1, `4` = 2)))

  # 1 > 2 ~ 3
  expect_equal(
    L1Ranking(as.PowerRelation("123 > 12 ~ 13 > 23 > 1 ~ 2 ~ 3")),
    doRanking(c(`1` = 2, `2` = 1, `3` = 1))
  )
})

test_that("Aliases", {
  expect_equal(lexcel1Scores, L1Scores)
  expect_equal(lexcel1Ranking, L1Ranking)
})

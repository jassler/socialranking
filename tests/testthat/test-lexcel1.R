test_that("works", {
  pr <- as.PowerRelation("(1234 ~ 12 ~ 2 ~ 134) > (23 ~ 14) > 234 > 4 > (3 ~ 1) > 123 > {} > (124 ~ 13 ~ 24 ~ 34)")
  scores <- L1Scores(pr)

  # L1
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

  # L2
  scores <- L2Scores(pr)
  expect_s3_class(scores, 'L2Scores')
  expect_equal(scores$`1`, matrix(c(
    3, 1, 0, 0, 1, 1, 0, 2,
    0, 0, 0, 0, 1, 0, 0, 0,
    1, 1, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 1, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0
  ), nrow = 5, byrow = TRUE))
  expect_equal(scores$`2`, matrix(c(
    3, 1, 1, 0, 0, 1, 0, 2,
    1, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 0, 0, 0, 0, 0, 1,
    0, 0, 1, 0, 0, 1, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0
  ), nrow = 5, byrow = TRUE))
  expect_equal(scores$`3`, matrix(c(
    2, 1, 1, 0, 1, 1, 0, 2,
    0, 0, 0, 0, 1, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 2,
    1, 0, 1, 0, 0, 1, 0, 0,
    1, 0, 0, 0, 0, 0, 0, 0
  ), nrow = 5, byrow = TRUE))
  expect_equal(scores$`4`, matrix(c(
    2, 1, 1, 1, 0, 0, 0, 3,
    0, 0, 0, 1, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 2,
    1, 0, 1, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0
  ), nrow = 5, byrow = TRUE))

  # LP
  scores <- LPScores(pr)
  expect_s3_class(scores, 'LPScores')
  expect_equal(scores$`1`, c(5, 2, 1, 1))
  expect_equal(scores$`2`, c(1, 0, 0, 0))
  expect_equal(scores$`3`, c(5, 1, 2, 1))
  expect_equal(scores$`4`, c(4, 1, 2, 1))

  # LP*
  scores <- LPSScores(pr)
  expect_s3_class(scores, 'LP*Scores')
  expect_equal(scores$`1`, matrix(c(
    1, 1, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0
  ), nrow = 3, byrow = TRUE))
  expect_equal(scores$`2`, matrix(integer(0), nrow = 3, byrow = TRUE))
  expect_equal(scores$`3`, matrix(c(
    0, 1, 0, 0,
    1, 0, 1, 0,
    1, 0, 0, 0
  ), nrow = 3, byrow = TRUE))
  expect_equal(scores$`4`, matrix(c(
    0, 1, 0,
    1, 0, 1,
    1, 0, 0
  ), nrow = 3, byrow = TRUE))

  # 2 > 1 > 4 > 3
  expect_equal(L1Ranking(pr), doRanking(c(`1` = 3, `2` = 4, `3` = 1, `4` = 2)))
  expect_equal(L2Ranking(pr), doRanking(c(`1` = 3, `2` = 4, `3` = 1, `4` = 2)))
  expect_equal(LPRanking(pr), doRanking(c(`1` = 2, `2` = 4, `3` = 1, `4` = 3)))
  expect_equal(LPSRanking(pr), doRanking(c(`1` = 2, `2` = 4, `3` = 1, `4` = 3)))

  # 1 > 2 ~ 3
  pr <- as.PowerRelation("123 > 12 ~ 13 > 23 > 1 ~ 2 ~ 3")
  expect_equal(L1Ranking(pr), doRanking(c(2, 1, 1)))
  expect_equal(L2Ranking(pr), doRanking(c(2, 1, 1)))
  expect_equal(LPRanking(pr), doRanking(c(1, 1, 1)))
  expect_equal(LPSRanking(pr), doRanking(c(2, 1, 1)))

  # empty matrices
  pr <- as.PowerRelation('a ~ b ~ ac ~ abc > c ~ bc ~ ab > {}')
  scores <- LPSScores(pr)
  expect_equal(scores$a, matrix(numeric(), nrow = 2))
  expect_equal(scores$b, matrix(numeric(), nrow = 2))
  expect_equal(scores$c, matrix(c(1, 1), nrow = 2))
})

test_that("Aliases", {
  expect_equal(lexcel1Scores, L1Scores)
  expect_equal(lexcel1Ranking, L1Ranking)

  expect_equal(lexcel2Scores, L2Scores)
  expect_equal(lexcel2Ranking, L2Ranking)

  expect_equal(lexcelPScores, LPScores)
  expect_equal(lexcelPRanking, LPRanking)

  expect_equal(lexcelPSScores, LPSScores)
  expect_equal(lexcelPSRanking, LPSRanking)
})

test_that("Different solutions", {
  testLFcts <- function(pr, l1, l2, lp, lps) {
    expect_output(print(L1Ranking(pr)), l1)
    expect_output(print(L2Ranking(pr)), l2)
    expect_output(print(LPRanking(pr)), lp)
    expect_output(print(LPSRanking(pr)), lps)
  }

  as.PowerRelation('1 ~ 23 ~ 24 ~ 234 > 2 ~ 34 > 1234 ~ 3 ~ 4 > {} > (123 ~ 124 ~ 134 ~ 12 ~ 13 ~ 14)') |> testLFcts(
    '1 > 2 > 3 ~ 4',
    '2 > 3 ~ 4 > 1',
    '1 > 2 > 3 ~ 4',
    '1 > 2 > 3 ~ 4'
  )

  as.PowerRelation('12 > 13 ~ 1 > 2 ~ 3 > 123 ~ {} ~ 23') |> testLFcts(
    '1 > 2 > 3',
    '1 > 2 > 3',
    '1 > 2 ~ 3',
    '1 > 2 > 3'
  )
})


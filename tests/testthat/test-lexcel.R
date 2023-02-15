test_that("ranking", {
  pr <- as.PowerRelation('12 > 1 > 2')
  ranking <- evaluate_promise(lexcelRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 > 2")
  expect_true(pr %:% 1 %>=lex% 2)
  expect_false(pr %:% 2 %>=lex% 1)
  expect_true(pr %:% 1 %>=duallex% 2)
  expect_false(pr %:% 2 %>=duallex% 1)
})

test_that("score vectors", {
  pr <- as.PowerRelation('12 > 1 > 2')
  expect_equal(
    lexcelScores(pr),
    structure(list(
      `1` = c(1,1,0),
      `2` = c(1,0,1)
    ), class = "LexcelScores")
  )
})

test_that("manip paper works", {
  pr <- as.PowerRelation('23 > 123 ~ 12 > 13')

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
  pr <- as.PowerRelation('12 > 1 > 2')
  ranking <- evaluate_promise(dualLexcelRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 > 2")

  pr <- as.PowerRelation('12 ~ 1 > 2 > 13')
  ranking <- evaluate_promise(dualLexcelRanking(pr), print = TRUE)
  expect_equal(ranking$output, "2 > 3 > 1")

  ranking <- evaluate_promise(lexcelRanking(pr), print = TRUE)
  expect_equal(ranking$output, "1 > 2 > 3")

  expect_false(pr %:% 2 %>=lex% 1)
  expect_true(pr %:% 2 %>=lex% 2)
  expect_true(pr %:% 2 %>=lex% 3)
  expect_true(pr %:% 1 %>=lex% 1)
  expect_true(pr %:% 1 %>=lex% 2)
  expect_true(pr %:% 1 %>=lex% 3)
  expect_false(pr %:% 3 %>=lex% 1)
  expect_false(pr %:% 3 %>=lex% 2)
  expect_true(pr %:% 3 %>=lex% 3)

  expect_true(pr %:% 2 %>=duallex% 1)
  expect_true(pr %:% 2 %>=duallex% 2)
  expect_true(pr %:% 2 %>=duallex% 3)
  expect_true(pr %:% 3 %>=duallex% 1)
  expect_false(pr %:% 3 %>=duallex% 2)
  expect_true(pr %:% 3 %>=duallex% 3)
  expect_true(pr %:% 1 %>=duallex% 1)
  expect_false(pr %:% 1 %>=duallex% 2)
  expect_false(pr %:% 1 %>=duallex% 3)

  expect_true(pr %:% 2 %>duallex% 1)
  expect_false(pr %:% 3 %>lex% 1)
  expect_false(pr %:% 3 %>lex% 2)
  expect_false(pr %:% 3 %>lex% 3)
  expect_false(pr %:% 1 %>duallex% 1)
  expect_false(pr %:% 1 %>duallex% 2)
  expect_false(pr %:% 1 %>duallex% 3)
})

test_that('lexcel named', {
  pr <- as.PowerRelation('abc > bc > ac > a > b > c > ab > {}')
  ranking <- evaluate_promise(lexcelRanking(pr), print = TRUE)
  expect_equal(ranking$output, 'c > b > a')

  ranking <- evaluate_promise(dualLexcelRanking(pr), print = TRUE)
  expect_equal(ranking$output, 'c > a > b')

  # abc~ab~a~c > b~bc > ac
  pr <- PowerRelation(list(
    list(c("Apple","Banana","Citrus"), c("Apple","Banana"), c("Citrus"), c("Apple")),
    list(c("Banana"), c("Banana","Citrus")),
    list(c("Apple","Citrus"))
  ))
  ranking <- evaluate_promise(lexcelRanking(pr), print = TRUE)
  expect_equal(ranking$output, 'Apple > Banana > Citrus')

  ranking <- evaluate_promise(dualLexcelRanking(pr), print = TRUE)
  expect_equal(ranking$output, 'Banana > Apple > Citrus')
})

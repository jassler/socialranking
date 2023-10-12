test_that("manip paper example", {
  pr <- as.PowerRelation('2 > 1 ~ 3 > 12 > 13 ~ 23 > {} > 123')

  ranking <- evaluate_promise(kramerSimpsonRanking(pr), print = TRUE)
  expect_equal(ranking$output, "2 > 1 > 3")
  expect_equal(kramerSimpsonScores(pr), structure(c(
    `1` = -1,
    `2` = 0,
    `3` = -2
  ), class = "KramerSimpsonScores"))

  expect_true(pr %:% 2 %>=ks% 1)
  expect_true(pr %:% 2 %>=ks% 2)
  expect_true(pr %:% 2 %>=ks% 3)
  expect_true(pr %:% 1 %>=ks% 1)
  expect_false(pr %:% 1 %>=ks% 2)
  expect_true(pr %:% 1 %>=ks% 3)
  expect_false(pr %:% 3 %>=ks% 1)
  expect_false(pr %:% 3 %>=ks% 2)
  expect_true(pr %:% 3 %>=ks% 3)

  expect_true(pr %:% 2 %>ks% 1)
  expect_false(pr %:% 2 %>ks% 2)
  expect_true(pr %:% 2 %>ks% 3)
  expect_false(pr %:% 1 %>ks% 1)
  expect_false(pr %:% 1 %>ks% 2)
  expect_true(pr %:% 1 %>ks% 3)
  expect_false(pr %:% 3 %>ks% 1)
  expect_false(pr %:% 3 %>ks% 2)
  expect_false(pr %:% 3 %>ks% 3)
})

test_that('kramer named', {
  pr <- as.PowerRelation('b > (a ~ c) > ab > (ac ~ bc) > {} > abc')
  ranking <- evaluate_promise(kramerSimpsonRanking(pr), print = TRUE)
  expect_equal(ranking$output, 'b > a > c')

  # abc~ab~a~c > b~bc > ac
  # '2 > 1 ~ 3 > 12 > 13 ~ 23 > {} > 123'
  pr <- PowerRelation(list(
    list(c("Banana")),
    list(c("Apple"), c("Citrus")),
    list(c("Apple", "Banana")),
    list(c("Apple", "Citrus"), c("Banana", "Citrus")),
    list(c()),
    list(c("Apple", "Banana", "Citrus"))
  ))
  ranking <- evaluate_promise(kramerSimpsonRanking(pr), print = TRUE)
  expect_equal(ranking$output, 'Banana > Apple > Citrus')
})

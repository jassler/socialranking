test_that("cumulativeScores", {
  pr <- newPowerRelation(c(1,2), ">", 1, ">", 2)
  expect_equal(cumulativeScores(pr, 1), structure(list(`1` = c(1, 2, 2)), class = "CumulativeScores"))
  expect_equal(cumulativeScores(pr, 2), structure(list(`2` = c(1, 1, 2)), class = "CumulativeScores"))
  expect_equal(cumulativeScores(pr), structure(
    list(
      `1` = c(1, 2, 2),
      `2` = c(1, 1, 2)
    ), class = "CumulativeScores")
  )
})

test_that("cumulativelyDominates", {
  pr <- newPowerRelation(c(1,2), ">", 1, ">", 2)
  expect_true(cumulativelyDominates(pr, 1, 2))
  expect_false(cumulativelyDominates(pr, 2, 1))
  expect_true(cumulativelyDominates(pr, 1, 1))
  expect_false(cumulativelyDominates(pr, 1, 1, strictly = TRUE))
  expect_true(cumulativelyDominates(pr, 2, 2))
  expect_false(cumulativelyDominates(pr, 2, 2, strictly = TRUE))
  expect_true(pr %:% 1 %>=cumuldom% 2)
  expect_false(pr %:% 2 %>=cumuldom% 1)
  expect_true(pr %:% 1 %>=cumuldom% 1)
  expect_true(pr %:% 2 %>=cumuldom% 2)

  expect_true(pr %:% 1 %>cumuldom% 2)
  expect_false(pr %:% 2 %>cumuldom% 1)
  expect_false(pr %:% 1 %>cumuldom% 1)
  expect_false(pr %:% 2 %>cumuldom% 2)

  pr <- newPowerRelationFromString("a > bc > b > ac > ab > c > abc")
  expect_true(cumulativelyDominates(pr, "a", "c"))
  expect_false(cumulativelyDominates(pr, "c", "a"))
  expect_false(cumulativelyDominates(pr, "a", "b"))
  expect_false(cumulativelyDominates(pr, "b", "a"))
  expect_true(cumulativelyDominates(pr, "b", "c"))
  expect_false(cumulativelyDominates(pr, "c", "b"))
  expect_true(pr %:% "a" %>=cumuldom% "c")
  expect_false(pr %:% "c" %>=cumuldom% "a")
  expect_false(pr %:% "a" %>=cumuldom% "b")
  expect_false(pr %:% "b" %>=cumuldom% "a")
  expect_true(pr %:% "b" %>=cumuldom% "c")
  expect_false(pr %:% "c" %>=cumuldom% "b")

  pr <- newPowerRelationFromString("ab ~ ac ~ bc > a ~ b > c")
  expect_true(cumulativelyDominates(pr, "a", "c"))
  expect_true(cumulativelyDominates(pr, "a", "c", strictly = TRUE))
  expect_false(cumulativelyDominates(pr, "c", "a"))
  expect_false(cumulativelyDominates(pr, "c", "a", strictly = TRUE))

  expect_true(cumulativelyDominates(pr, "a", "b"))
  expect_false(cumulativelyDominates(pr, "a", "b", strictly = TRUE))
  expect_true(cumulativelyDominates(pr, "b", "a"))
  expect_false(cumulativelyDominates(pr, "b", "a", strictly = TRUE))

  expect_true(cumulativelyDominates(pr, "b", "c"))
  expect_true(cumulativelyDominates(pr, "b", "c", strictly = TRUE))
  expect_false(cumulativelyDominates(pr, "c", "b"))
  expect_false(cumulativelyDominates(pr, "c", "b", strictly = TRUE))

  expect_true(pr %:% "a" %>=cumuldom% "c")
  expect_false(pr %:% "c" %>=cumuldom% "a")
  expect_true(pr %:% "a" %>=cumuldom% "b")
  expect_true(pr %:% "b" %>=cumuldom% "a")
  expect_true(pr %:% "b" %>=cumuldom% "c")
  expect_false(pr %:% "c" %>=cumuldom% "b")
})


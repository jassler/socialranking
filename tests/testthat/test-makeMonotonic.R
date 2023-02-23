test_that("Make monotonic", {
  pr <- as.PowerRelation('1 > 2')
  expect_equal(
    makePowerRelationMonotonic(pr),
    as.PowerRelation('12 ~ 1 > 2')
  )

  pr <- as.PowerRelation('1 > 2 > 3')
  expect_equal(
    makePowerRelationMonotonic(pr),
    as.PowerRelation('123 ~ 12 ~ 13 ~ 1 > 23 ~ 2 > 3')
  )

  pr <- as.PowerRelation('3 > {} > 2 > 1')
  expect_equal(
    makePowerRelationMonotonic(pr),
    as.PowerRelation('123 ~ 13 ~ 23 ~ 3 > 12 ~ 1 ~ 2 ~ {}')
  )

  pr <- as.PowerRelation("ab > ac > abc > b > a > {} > c > bc")
  expect_equal(
    makePowerRelationMonotonic(pr),
    as.PowerRelation('abc ~ ab > ac > bc ~ b > a > c ~ {}')
  )
})

test_that("Without adding new coalitions", {
  pr <- as.PowerRelation('1 > 2')
  expect_equal(
    makePowerRelationMonotonic(pr, addMissingCoalitions = FALSE),
    as.PowerRelation('1 > 2')
  )

  pr <- as.PowerRelation('12 > 13 ~ 123')
  expect_equal(
    makePowerRelationMonotonic(pr, addMissingCoalitions = FALSE),
    as.PowerRelation('123 ~ 12 > 13')
  )

  pr <- as.PowerRelation('{} > 12 ~ 13 > 123 > 1 > 2 > 3 ~ 23')
  expect_equal(
    makePowerRelationMonotonic(pr, addMissingCoalitions = FALSE),
    1:3 |> as.numeric() |> createPowerset() |> list() |> PowerRelation()
  )
})

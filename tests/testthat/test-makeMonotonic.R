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
})

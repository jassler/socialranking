test_that("Make total", {
  pr <- PowerRelation(list(list(1),list(2)))
  expect_equal(
    appendMissingCoalitions(pr),
    as.PowerRelation('1 > 2 > 12 ~ {}')
  )
  expect_equal(
    appendMissingCoalitions(pr, includeEmptySet = FALSE),
    as.PowerRelation('1 > 2 > 12')
  )

  pr <- PowerRelation(list(list(1,2)))
  expect_equal(
    appendMissingCoalitions(pr),
    as.PowerRelation('1 ~ 2 > 12 ~ {}')
  )
  expect_equal(
    appendMissingCoalitions(pr, includeEmptySet = FALSE),
    as.PowerRelation('1 ~ 2 > 12')
  )

  pr <- PowerRelation(list(list(c(1,2,3)), list(3)))
  expect_equal(
    appendMissingCoalitions(pr),
    as.PowerRelation('123 > 3 > 12 ~ 13 ~ 23 ~ 1 ~ 2 ~ {}')
  )
})

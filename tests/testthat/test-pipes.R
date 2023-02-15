test_that("simple pipe", {
  pr <- as.PowerRelation('ab > a > b')

  expect_equal(testRelation(pr, 'a'), list(pr, 'a'))
  expect_equal(pr %:% 'a', testRelation(pr, 'a'))
})

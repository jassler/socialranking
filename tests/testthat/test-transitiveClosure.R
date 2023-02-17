test_that("no cycles", {
  pr <- as.PowerRelation('1 > 2')
  closured <- transitiveClosure(pr)
  expect_equal(closured, as.PowerRelation('1 > 2'))

  pr <- as.PowerRelation('1 ~ 2')
  closured <- transitiveClosure(pr)
  expect_equal(closured, as.PowerRelation('1 ~ 2'))
})

test_that('with cycles', {
  pr <- suppressWarnings(as.PowerRelation('1 > 2 > 1'))
  closured <- transitiveClosure(pr)
  expect_equal(closured, as.PowerRelation('1 ~ 2'))

  pr <- suppressWarnings(as.PowerRelation('1 ~ 1 ~ 1 > 2 ~ 12 ~ 2'))
  closured <- transitiveClosure(pr)
  expect_equal(closured, as.PowerRelation('1 > 2 ~ 12'))

  pr <- suppressWarnings(as.PowerRelation('1 > 2 > 1 > 13 ~ 3'))
  closured <- transitiveClosure(pr)
  expect_equal(closured, as.PowerRelation('1 ~ 2 > 13 ~ 3'))

  pr <- suppressWarnings(as.PowerRelation('1 > 2 > 1 > 13 ~ 3 > 1'))
  closured <- transitiveClosure(pr)
  expect_equal(closured, as.PowerRelation('1 ~ 2 ~ 13 ~ 3'))

  pr <- suppressWarnings(as.PowerRelation('2 > 1 > 13 ~ 3 > 1'))
  closured <- transitiveClosure(pr)
  expect_equal(closured, as.PowerRelation('2 > 1 ~ 13 ~ 3'))
})


test_that('with multiple cycles', {
  pr <- suppressWarnings(as.PowerRelation('1 > 31 > 1 > 2 ~ 13 > 3'))
  expect_equal(transitiveClosure(pr), as.PowerRelation('1 ~ 13 ~ 2 > 3'))

  pr <- suppressWarnings(as.PowerRelation('1 > 3 > 1 > 2 > 23 > 2'))
  expect_equal(transitiveClosure(pr), as.PowerRelation('1 ~ 3 > 2 ~ 23'))
})

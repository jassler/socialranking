test_that("Generator with two elements", {
  gen <- powerRelationGenerator(c('a','b'))
  expect_true(gen() == as.PowerRelation('a ~ b'))
  expect_true(gen() == as.PowerRelation('a > b'))
  expect_true(gen() == as.PowerRelation('b > a'))
  expect_null(gen())

  gen <- powerRelationGenerator(c('a','b'), startWithLinearOrder = TRUE)
  expect_true(gen() == as.PowerRelation('a > b'))
  expect_true(gen() == as.PowerRelation('b > a'))
  expect_true(gen() == as.PowerRelation('a ~ b'))
  expect_null(gen())
})

test_that("Generator with empty set", {
  gen <- powerRelationGenerator(list('a','b',c()))
  expect_true(gen() == as.PowerRelation('a ~ b ~ {}'))
  expect_true(gen() == as.PowerRelation('(a ~ b)  > {}'))
  expect_true(gen() == as.PowerRelation('(a ~ {}) > b'))
  expect_true(gen() == as.PowerRelation('(b ~ {}) > a'))
  expect_true(gen() == as.PowerRelation('a > (b ~ {})'))
  expect_true(gen() == as.PowerRelation('b > (a ~ {})'))
  expect_true(gen() == as.PowerRelation('{} > (a ~ b)'))
  expect_true(gen() == as.PowerRelation('a > b > {}'))
  expect_true(gen() == as.PowerRelation('a > {} > b'))
  expect_true(gen() == as.PowerRelation('b > a > {}'))
  expect_true(gen() == as.PowerRelation('{} > a > b'))
  expect_true(gen() == as.PowerRelation('b > {} > a'))
  expect_true(gen() == as.PowerRelation('{} > b > a'))
  expect_null(gen())
})

test_that("Too few coalitions given", {
  expect_error(powerRelationGenerator(list()))
  expect_error(powerRelationGenerator(list("a")))
})

test_that("Lookups still give correct values", {
  gen <- powerRelationGenerator(list(c('a','b'),'a','b'))
  gen()
  gen()
  # (ab ~ b) > a
  pr <- gen()
  expect_equal(pr$coalitionLookup(c('a','b')), 1)
  expect_equal(pr$coalitionLookup(c('b','a')), 1)
  expect_equal(pr$coalitionLookup('a'), 2)
  expect_equal(pr$coalitionLookup('b'), 1)
  expect_null(pr$coalitionLookup(c()))

  expect_equal(pr$elementLookup('a'), list(c(1,1), c(2,1)))
  expect_equal(pr$elementLookup('b'), list(c(1,1), c(1,2)))
  expect_null(pr$elementLookup('c'))
})

test_that("Skip partition", {
  gen <- powerRelationGenerator(createPowerset(c(1, 2)))
  gen <- generateNextPartition(gen)
  expect_true(gen() == as.PowerRelation('(12 ~ 1 ~ 2) > {}'))
  expect_true(gen() == as.PowerRelation('(12 ~ 1 ~ {}) > 2'))

  gen <- generateNextPartition(gen)
  expect_true(gen() == as.PowerRelation('(12 ~ 1) > (2 ~ {})'))

  gen <- generateNextPartition(gen)
  expect_true(gen() == as.PowerRelation('12 > (1 ~ 2 ~ {})'))

  gen <- generateNextPartition(gen)
  expect_true(gen() == as.PowerRelation('(12 ~ 1) > 2 > {})'))

  gen <- generateNextPartition(gen)
  expect_true(gen() == as.PowerRelation('12 > (1 ~ 2) > {})'))

  gen <- generateNextPartition(gen)
  expect_true(gen() == as.PowerRelation('12 > 1 > (2 ~ {})'))

  gen <- generateNextPartition(gen)
  expect_true(gen() == as.PowerRelation('12 > 1 > 2 > {}'))

  expect_error(generateNextPartition(gen))
})

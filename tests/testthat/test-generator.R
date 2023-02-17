test_that("Generator with two elements", {
  gen <- powerRelationGenerator(c('a','b'))
  expect_equal(gen(), as.PowerRelation('a ~ b'))
  expect_equal(gen(), as.PowerRelation('a > b'))
  expect_equal(gen(), as.PowerRelation('b > a'))
  expect_null(gen())

  gen <- powerRelationGenerator(c('a','b'), startWithLinearOrder = TRUE)
  expect_equal(gen(), as.PowerRelation('a > b'))
  expect_equal(gen(), as.PowerRelation('b > a'))
  expect_equal(gen(), as.PowerRelation('a ~ b'))
  expect_null(gen())
})

test_that("Generator with empty set", {
  gen <- powerRelationGenerator(list('a','b',c()))
  expect_equal(gen(), as.PowerRelation('a ~ b ~ {}'))
  expect_equal(gen(), as.PowerRelation('(a ~ b)  > {}'))
  expect_equal(gen(), as.PowerRelation('(a ~ {}) > b'))
  expect_equal(gen(), as.PowerRelation('(b ~ {}) > a'))
  expect_equal(gen(), as.PowerRelation('a > (b ~ {})'))
  expect_equal(gen(), as.PowerRelation('b > (a ~ {})'))
  expect_equal(gen(), as.PowerRelation('{} > (a ~ b)'))
  expect_equal(gen(), as.PowerRelation('a > b > {}'))
  expect_equal(gen(), as.PowerRelation('a > {} > b'))
  expect_equal(gen(), as.PowerRelation('b > a > {}'))
  expect_equal(gen(), as.PowerRelation('{} > a > b'))
  expect_equal(gen(), as.PowerRelation('b > {} > a'))
  expect_equal(gen(), as.PowerRelation('{} > b > a'))
  expect_null(gen())
})

test_that("Too few coalitions given", {
  expect_error(powerRelationGenerator(list()))
  expect_error(powerRelationGenerator(list("a")))
})

test_that("Skip partition", {
  gen <- powerRelationGenerator(createPowerset(c(1, 2)))
  gen <- generateNextPartition(gen)
  expect_equal(gen(), as.PowerRelation('(12 ~ 1 ~ 2) > {}'))
  expect_equal(gen(), as.PowerRelation('(12 ~ 1 ~ {}) > 2'))

  gen <- generateNextPartition(gen)
  expect_equal(gen(), as.PowerRelation('(12 ~ 1) > (2 ~ {})'))

  gen <- generateNextPartition(gen)
  expect_equal(gen(), as.PowerRelation('12 > (1 ~ 2 ~ {})'))

  gen <- generateNextPartition(gen)
  expect_equal(gen(), as.PowerRelation('(12 ~ 1) > 2 > {})'))

  gen <- generateNextPartition(gen)
  expect_equal(gen(), as.PowerRelation('12 > (1 ~ 2) > {})'))

  gen <- generateNextPartition(gen)
  expect_equal(gen(), as.PowerRelation('12 > 1 > (2 ~ {})'))

  gen <- generateNextPartition(gen)
  expect_equal(gen(), as.PowerRelation('12 > 1 > 2 > {}'))

  expect_error(generateNextPartition(gen))
})

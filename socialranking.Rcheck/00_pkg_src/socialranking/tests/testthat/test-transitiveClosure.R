test_that("no cycles", {
  pr <- newPowerRelation(1, '>', 2)

  closured <- transitiveClosure(pr)
  expect_equal(closured$rankingCoalitions, list(sets::set(1), sets::set(2)))
  expect_equal(closured$rankingComparators, '>')
  expect_equal(closured$elements, c(1, 2))
  expect_equal(closured$equivalenceClasses, list(list(sets::set(1)), list(sets::set(2))))


  pr <- newPowerRelation(1, '~', 2)

  closured <- transitiveClosure(pr)
  expect_equal(closured$rankingCoalitions, list(sets::set(1), sets::set(2)))
  expect_equal(closured$rankingComparators, '~')
  expect_equal(closured$elements, c(1, 2))
  expect_equal(closured$equivalenceClasses, list(list(sets::set(1), sets::set(2))))
})

test_that('with cycles', {
  pr <- suppressWarnings(newPowerRelation(1, '>', 2, '>', 1))

  closured <- transitiveClosure(pr)
  expect_equal(closured$rankingCoalitions, list(sets::set(1), sets::set(2)))
  expect_equal(closured$rankingComparators, '~')
  expect_equal(closured$elements, c(1, 2))
  expect_equal(closured$equivalenceClasses, list(list(sets::set(1), sets::set(2))))


  pr <- suppressWarnings(newPowerRelation(1, '>', 2, '>', 1, '>', c(1,3), '~', 3))

  closured <- transitiveClosure(pr)
  expect_equal(closured$rankingCoalitions, list(sets::set(1), sets::set(2), sets::set(1,3), sets::set(3)))
  expect_equal(closured$rankingComparators, c('~', '>', '~'))
  expect_equal(closured$elements, c(1, 2, 3))
  expect_equal(closured$equivalenceClasses, list(list(sets::set(1), sets::set(2)), list(sets::set(1,3), sets::set(3))))


  pr <- suppressWarnings(newPowerRelation(1, '>', 2, '>', 1, '>', c(1,3), '~', 3, '>', 1))

  closured <- transitiveClosure(pr)
  expect_equal(closured$rankingCoalitions, list(sets::set(1), sets::set(2), sets::set(1,3), sets::set(3)))
  expect_equal(closured$rankingComparators, c('~', '~', '~'))
  expect_equal(closured$elements, c(1, 2, 3))
  expect_equal(closured$equivalenceClasses, list(list(sets::set(1), sets::set(2), sets::set(1,3), sets::set(3))))



  pr <- suppressWarnings(newPowerRelation(2, '>', 1, '>', c(1,3), '~', 3, '>', 1))

  closured <- transitiveClosure(pr)
  expect_equal(closured$rankingCoalitions, list(sets::set(2), sets::set(1), sets::set(1,3), sets::set(3)))
  expect_equal(closured$rankingComparators, c('>', '~', '~'))
  expect_equal(closured$elements, c(1, 2, 3))
  expect_equal(closured$equivalenceClasses, list(list(sets::set(2)), list(sets::set(1), sets::set(1,3), sets::set(3))))
})


test_that('with multiple cycles', {
  pr <- suppressWarnings(newPowerRelation(1, '>', c(3,1), '>', 1, '>', 2, '~', c(1,3), '>', 3))
  expect_equal(transitiveClosure(pr), newPowerRelation(1, '~', c(1,3), '~', 2, '>', 3))

  pr <- suppressWarnings(newPowerRelation(1, '>', 3, '>', 1, '>', 2, '>', c(2,3), '>', 2))
  expect_equal(transitiveClosure(pr), newPowerRelation(1, '~', 3, '>', 2, '~', c(2,3)))
})

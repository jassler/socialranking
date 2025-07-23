test_that("basic", {
  pr <- PowerRelation(list(list(c(2,1)),list(1),list(2)))
  expect_equal(pr$elements, c(1, 2))
  expect_equal(pr$eqs, structure(list(3, 1, 2), class='eqClasses'))

  expect_equal(pr$coalitionLookup(c(1,2)), 1)
  expect_equal(pr$coalitionLookup(1), 2)
  expect_equal(pr$coalitionLookup(2), 3)
  #
  expect_equal(pr$elementLookup(1), matrix(c(1, 1, 2, 1), nrow=2, dimnames=list(c('E','i'), NULL)))
  expect_equal(pr$elementLookup(2), matrix(c(1, 1, 3, 1), nrow=2, dimnames=list(c('E','i'), NULL)))
})

test_that("element types", {
  pr <- PowerRelation(list(list(c("a",2)), list("a", 2)))
  expect_equal(pr$elements, c('2','a'))
  expect_equal(pr$eqs, structure(list(3, c(2, 1)), class='eqClasses'))
})

test_that("empty eqs exception", {
  expect_error(PowerRelation(list(list())))
  expect_error(PowerRelation(list(list(1), list(), list(2))))
})

test_that("from string", {
  pr <- as.PowerRelation("12 > 1 ~ 2")
  expect_equal(pr, PowerRelation(list(list(c(1,2)), list(1, 2))))

  pr <- as.PowerRelation("ab > a ~ b")
  expect_equal(pr, PowerRelation(list(list(c("a","b")), list("a", "b"))))


  pr <- as.PowerRelation("12 > (1 ~ 2).:;-*/\\")
  expect_equal(pr, PowerRelation(list(list(c(1,2)), list(1, 2))))

  pr <- as.PowerRelation("a ~ {} > b")
  expect_equal(pr, PowerRelation(list(list("a", c()), list("b"))))

  pr <- as.PowerRelation("{} > a > b")
  expect_equal(pr, PowerRelation(list(list(c()), list("a"), list("b"))))
})

test_that("from list", {
  pr <- as.PowerRelation(list(c(1,2), 1, 2))
  expect_equal(pr, PowerRelation(list(list(c(1,2)), list(1), list(2))))

  pr <- as.PowerRelation(list(c(1,2), 1, 2), comparators = '>')
  expect_equal(pr, PowerRelation(list(list(c(1,2)), list(1), list(2))))

  pr <- as.PowerRelation(list(c(1,2), 1, 2), comparators = '~')
  expect_equal(pr, PowerRelation(list(list(c(1,2), 1, 2))))

  pr <- as.PowerRelation(list(c(1,2), 1, 2), comparators = c('>', '~'))
  expect_equal(pr, PowerRelation(list(list(c(1,2)), list(1, 2))))
})


test_that("output", {
  pr <- as.PowerRelation("1 > 2 ~ 12")
  expect_equal(capture.output(pr), "1 > (2 ~ 12)")
  expect_equal(capture.output(pr$eqs), c("E_1 = {{1}}", "E_2 = {{2}, {1, 2}}"))

  pr <- PowerRelation(list(list(c('de', 'at')), list('b', c('de', 'b'))))
  expect_equal(capture.output(pr), "{at, de} > ({b} ~ {b, de})")
  expect_equal(capture.output(pr$eqs), c('E_1 = {{1, 3}}', 'E_2 = {{2}, {2, 3}}'))
})


test_that("cycle warning", {
  pr <- as.PowerRelation('1 > 1')
  result <- evaluate_promise(pr$coalitionLookup(1))
  expect_length(result$warnings, 1)
  expect_equal(result$warnings[1], 'Found duplicate coalition {1} in equivalence class 2. It will be ignored in coalitionLookup(), but not in elementLookup().\nIf this was not a mistake, please file an issue at: https://github.com/jassler/socialranking/issues/')

  pr <- as.PowerRelation("1 > 1 ~ 2 > 2")
  result <- evaluate_promise(pr$coalitionLookup(c(1, 2)))
  expect_length(result$warnings, 2)
  expect_equal(result$warnings[1], "Found duplicate coalition {1} in equivalence class 2. It will be ignored in coalitionLookup(), but not in elementLookup().\nIf this was not a mistake, please file an issue at: https://github.com/jassler/socialranking/issues/")
  expect_equal(result$warnings[2], "Found duplicate coalition {2} in equivalence class 3. It will be ignored in coalitionLookup(), but not in elementLookup().\nIf this was not a mistake, please file an issue at: https://github.com/jassler/socialranking/issues/")
  result <- evaluate_promise(pr$coalitionLookup(c(1, 2)))
  expect_length(result$warnings, 0)

  pr <- as.PowerRelation("12 > 12 ~ 123 > 12")
  result <- evaluate_promise(pr$coalitionLookup(c(1, 2)))
  expect_length(result$warnings, 2)
  expect_equal(result$warnings[1], "Found duplicate coalition {1, 2} in equivalence class 2. It will be ignored in coalitionLookup(), but not in elementLookup().\nIf this was not a mistake, please file an issue at: https://github.com/jassler/socialranking/issues/")
  expect_equal(result$warnings[2], "Found duplicate coalition {1, 2} in equivalence class 3. It will be ignored in coalitionLookup(), but not in elementLookup().\nIf this was not a mistake, please file an issue at: https://github.com/jassler/socialranking/issues/")

  pr <- PowerRelation(list(list(c("ab", "cd", "ef")), list(c("ab", "cd", "ef"))))
  result <- evaluate_promise(pr$coalitionLookup(c('ab', 'cd', 'ef')))
  expect_length(result$warnings, 1)
  expect_equal(result$warnings[1], "Found duplicate coalition {ab, cd, ef} in equivalence class 2. It will be ignored in coalitionLookup(), but not in elementLookup().\nIf this was not a mistake, please file an issue at: https://github.com/jassler/socialranking/issues/")
})

test_that("duplicate elements", {
  result <- evaluate_promise(as.PowerRelation("11 > 12"))
  expect_length(result$warnings, 0)
  expect_equal(capture.output(result$result), '1 > 12')

  result <- evaluate_promise(as.PowerRelation("11 > 121 > 313133"))
  expect_length(result$warnings, 0)
  expect_equal(capture.output(result$result), '1 > 12 > 13')
})

test_that("equality", {
  expect_true(
    as.PowerRelation("1 ~ 12 ~ 3") ==
    as.PowerRelation("12 ~ 3 ~ 1")
  )
  expect_true(
    as.PowerRelation("12 > 1 ~ 2") ==
    as.PowerRelation("12 > 2 ~ 1")
  )

  expect_false(
    as.PowerRelation("12 > 1 ~ 2") ==
    as.PowerRelation("1 > 12 ~ 2")
  )
})

test_that("coalitionsAreIndifferent", {
  pr <- as.PowerRelation("123 ~ 2 > 12 ~ 1 ~ 3 > {}")
  expect_true(coalitionsAreIndifferent(pr, c(1,2,3), 2))
  expect_true(coalitionsAreIndifferent(pr, 2, c(1,2,3)))
  expect_false(coalitionsAreIndifferent(pr, 2, c()))
  expect_false(coalitionsAreIndifferent(pr, 3, c(1,2,3)))

  expect_true(coalitionsAreIndifferent(pr, 7, 2, asBits = TRUE))
  expect_true(coalitionsAreIndifferent(pr, 2, 7, asBits = TRUE))
  expect_false(coalitionsAreIndifferent(pr,2, 0, asBits = TRUE))
  expect_false(coalitionsAreIndifferent(pr,4, 7, asBits = TRUE))
})

test_that("equivalenceClassIndex", {
  pr <- as.PowerRelation("123 ~ 2 > 12 ~ 1 ~ 3 > {}")
  expect_equal(equivalenceClassIndex(pr, c(1,2)), 2)
  expect_equal(equivalenceClassIndex(pr, c(2,1)), 2)
  expect_equal(equivalenceClassIndex(pr, c(1,3)), as.integer(NA))
  expect_equal(equivalenceClassIndex(pr, c()), 3)
  expect_equal(equivalenceClassIndex, coalitionLookup)
})

test_that("elementLookup", {
  pr <- as.PowerRelation("123 ~ 2 > 12 ~ 1 ~ 3 > {}")
  expect_equal(elementLookup(pr, 1), matrix(c(1,1, 2,1, 2,2), nrow=2, dimnames=list(c('E','i'), NULL)))
  expect_equal(elementLookup(pr, 2), matrix(c(1,1, 1,2, 2,1), nrow=2, dimnames=list(c('E','i'), NULL)))
  expect_equal(elementLookup(pr, 3), matrix(c(1,1, 2,3), nrow=2, dimnames=list(c('E','i'), NULL)))
  expect_error(elementLookup(pr, 4))

  pr <- PowerRelation(pr$eqs, elements = 1:4, asBits = TRUE)
  expect_equal(elementLookup(pr, 4), matrix(integer(), nrow=2, ncol=0, dimnames=list(c('E','i'), NULL)))
})

test_that("encodeCoalition", {
  expect_equal(11, encodeCoalition(c(1, 2, 4)))
  expect_equal(11, encodeCoalition(c('a', 'b', 'd'), elements = letters))

  expect_identical(decodeCoalition(11), as.integer(c(1, 2, 4)))
  expect_identical(decodeCoalition(11, elements = letters), c('a', 'b', 'd'))

  expect_equal(0, encodeCoalition(c()))
  expect_equal(integer(), decodeCoalition(0))
})

test_that("Inequality from differently sized lists", {
  expect_false({
    as.PowerRelation("12 > (13 ~ 1) >( 23 ~ 123 ~ 2 ~ 3)") ==
    as.PowerRelation("12 > (13 ~ 1 ~ 23 ~ 123 ~ 2 ~ 3)")
  })
  expect_false({
    as.PowerRelation("12 > (13 ~ 1) > (23 ~ 123 ~ 2 ~ 3)") ==
    as.PowerRelation("(12 ~ 13) > 1 > (23 ~ 123 ~ 2 ~ 3)")
  })
})

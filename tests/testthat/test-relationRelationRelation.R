test_that("works", {
  rel <- function(data, dimname) {
    relations::as.relation(matrix(
      data, nrow = length(dimname), byrow = TRUE, dimnames = list(dimname, dimname)
    ))
  }

  # 1
  pr <- as.PowerRelation('12 > 1 > 2')
  expect_equal(
    powerRelationMatrix(pr),
    rel(c(
      1,1,1,
      0,1,1,
      0,0,1
    ), c("12", "\u200B1", "\u200B\u200B2"))
  )

  # 2
  pr <- as.PowerRelation('12 > 1 ~ 2')
  expect_equal(
    powerRelationMatrix(pr),
    rel(c(
      1,1,1,
      0,1,1,
      0,1,1
    ), c("12", "\u200B1", "\u200B\u200B2"))
  )

  # 3
  pr <- as.PowerRelation('12 > 2 > 1')
  expect_equal(
    powerRelationMatrix(pr),
    rel(c(
      1,1,1,
      0,1,1,
      0,0,1
    ), c("12", "\u200B2", "\u200B\u200B1"))
  )
})

test_that("custom names", {
  rel <- function(data, dimname) {
    relations::as.relation(matrix(
      data, nrow = length(dimname), byrow = TRUE, dimnames = list(dimname, dimname)
    ))
  }

  pr <- as.PowerRelation("12 > 1 ~ 2")
  expect_equal(
    powerRelationMatrix(pr, domainNames = function(x) c("ab","ij","xy")[x] ),
    rel(c(
      1,1,1,
      0,1,1,
      0,1,1
    ), c("ab", "ij", "xy"))
  )
  expect_equal(
    powerRelationMatrix(pr, domainNames = "numericPrec" ),
    rel(c(
      1,1,1,
      0,1,1,
      0,1,1
    ), c("1{12}","2{1}","3{2}"))
  )
  expect_equal(
    powerRelationMatrix(pr, domainNames = "numeric" ),
    rel(c(
      1,1,1,
      0,1,1,
      0,1,1
    ), c("1","2","3"))
  )
  expect_error(powerRelationMatrix(pr, domainNames = "nonexistent"))

  pr <- as.PowerRelation(list(c("ab", "cd"), "ab", "cd"))
  expect_equal(
    powerRelationMatrix(pr),
    rel(c(
      1,1,1,
      0,1,1,
      0,0,1
    ), c("ab,cd","\u200Bab","\u200B\u200Bcd"))
  )
  expect_equal(
    powerRelationMatrix(pr, domainNames = "numericPrec" ),
    rel(c(
      1,1,1,
      0,1,1,
      0,0,1
    ), c("1{ab,cd}","2{ab}","3{cd}"))
  )
})

test_that("cycles", {
  rel <- function(data, dimname) {
    relations::as.relation(matrix(
      data, nrow = length(dimname), byrow = TRUE, dimnames = list(dimname, dimname)
    ))
  }

  # 12 > 1 > 2 > 1
  pr <- suppressWarnings(as.PowerRelation('12 > 1 > 2 > 1'))
  expect_equal(
    powerRelationMatrix(pr),
    rel(c(
      1,1,1,1,
      0,1,1,1,
      0,1,1,1,
      0,1,1,1
    ), c("12", "\u200B1", "\u200B\u200B2", "\u200B\u200B\u200B1"))
  )

  # 1 > 2 > 12 > 1
  pr <- suppressWarnings(as.PowerRelation('1 > 2 > 12 > 1'))
  expect_equal(
    powerRelationMatrix(pr),
    rel(c(
      1,1,1,1,
      1,1,1,1,
      1,1,1,1,
      1,1,1,1
    ), c("1", "\u200B2", "\u200B\u200B12", "\u200B\u200B\u200B1"))
  )

  # 1 > 2 > 1 > 12
  pr <- suppressWarnings(as.PowerRelation('1 > 2 > 1 > 12'))
  expect_equal(
    powerRelationMatrix(pr),
    rel(c(
      1,1,1,1,
      1,1,1,1,
      1,1,1,1,
      0,0,0,1
    ), c("1", "\u200B2", "\u200B\u200B1", "\u200B\u200B\u200B12"))
  )

  # Transitive Closure
  # 1 > 2 > 3 > 1
  # Warning: Cycle found! Call "transitiveClosure(pr)"
  # => 1 ~ 2 ~ 3
})

test_that("as.relation", {
  rel <- function(data, dimname) {
    relations::as.relation(matrix(
      data, nrow = length(dimname), byrow = TRUE, dimnames = list(dimname, dimname)
    ))
  }
  pr <- as.PowerRelation('12 > 1 > 2')

  expect_equal(
    as.relation(pr),
    rel(c(
      1,1,1,
      0,1,1,
      0,0,1
    ), c("12", "\u200B1", "\u200B\u200B2"))
  )
})

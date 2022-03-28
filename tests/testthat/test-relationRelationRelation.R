test_that("works", {
  rel <- function(data, dimname) {
    relations::as.relation(matrix(
      data, nrow = length(dimname), byrow = TRUE, dimnames = list(dimname, dimname)
    ))
  }

  # 1
  pr <- newPowerRelation(c(1,2), ">", 1, ">", 2)
  expect_equal(
    powerRelationMatrix(pr),
    rel(c(
      1,1,1,
      0,1,1,
      0,0,1
    ), c("12", "\u200B1", "\u200B\u200B2"))
  )

  # 2
  pr <- newPowerRelation(c(1,2), ">", 1, "~", 2)
  expect_equal(
    powerRelationMatrix(pr),
    rel(c(
      1,1,1,
      0,1,1,
      0,1,1
    ), c("12", "\u200B1", "\u200B\u200B2"))
  )

  # 3
  pr <- newPowerRelation(c(1,2), ">", 2, ">", 1)
  expect_equal(
    powerRelationMatrix(pr),
    rel(c(
      1,1,1,
      0,1,1,
      0,0,1
    ), c("12", "\u200B2", "\u200B\u200B1"))
  )
})

test_that("cycles", {
  rel <- function(data, dimname) {
    relations::as.relation(matrix(
      data, nrow = length(dimname), byrow = TRUE, dimnames = list(dimname, dimname)
    ))
  }

  # 12 > 1 > 2 > 1
  pr <- suppressWarnings(newPowerRelation(c(1,2), ">", 1, ">", 2, ">", 1))
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
  pr <- suppressWarnings(newPowerRelation(1, ">", 2, ">", c(1,2), ">", 1))
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
  pr <- suppressWarnings(newPowerRelation(1, ">", 2, ">", 1, ">", c(1,2)))
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

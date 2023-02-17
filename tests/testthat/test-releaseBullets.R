test_that("release bullets up to date", {
  expect_equal(
    release_bullets(),
    c("Updated vignettes/prebuild.pdf? (check ignored/vignetter.R)",
      "Updated auto-generated function checks? (check ignored/checkGenerator.R)"
    )
  )
})

test_that("fbg_get_coverage() works", {
  # Wrong input
  expect_error(
    fbg_get_coverage(data.frame("a"), data.frame("a")),
    "Provided site-species object is not a numeric matrix or a data.frame",
    fixed = TRUE
  )
  
  # No species in common
  expect_error(
    fbg_get_coverage(
      site_sp = matrix(1, dimnames = list("s1", "a")),
      traits  = matrix(1, dimnames = list("b", "t1"))
    ),
    "No species found in common between inputs",
    fixed = TRUE
  )
  
  
  # Trait with NAs
  expect_silent(
    test_coverage <- fbg_get_coverage(
      site_sp = matrix(1, dimnames = list("s1", "a")),
      traits  = matrix(NA_real_, dimnames = list("a", "t1"))
    )
  )
  
  # Check output format
  expect_s3_class(test_coverage, "data.frame")
  expect_named(test_coverage, c("site", "trait_coverage"))
  expect_type(test_coverage$site, "character")
  expect_type(test_coverage$trait_coverage, "double")
  expect_equal(test_coverage$site, "s1")
  expect_equal(test_coverage$trait_coverage, 1)
  
  ## Occurrence matrix
  # with only species for which we have traits
  expect_silent(
    test_coverage <- fbg_get_coverage(
      site_sp = matrix(c(1, 0, 1), nrow = 1, ncol = 3,
                       dimnames = list("s1", letters[1:3])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1"))
    )
  )
  expect_equal(test_coverage$trait_coverage, 1)
  # with species for which we don't have all the traits
  expect_silent(
    test_coverage <- fbg_get_coverage(
      site_sp = matrix(c(1, 0, 1, 1), nrow = 1, ncol = 4, 
                       dimnames = list("s1", letters[1:4])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1"))
    )
  )
  expect_equal(test_coverage$trait_coverage, 2/3)
  
  ## Abundance matrix
  # with only species for which we have traits
  expect_silent(
    test_coverage <- fbg_get_coverage(
      site_sp = matrix(c(5, 0, 5), nrow = 1, ncol = 3,
                       dimnames = list("s1", letters[1:3])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1"))
    )
  )
  expect_equal(test_coverage$trait_coverage, 1)
  # with species for which we don't have all the traits
  expect_silent(
    test_coverage <- fbg_get_coverage(
      site_sp = matrix(c(5, 0, 5, 5), nrow = 1, ncol = 4,
                       dimnames = list("s1", letters[1:4])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1"))
    )
  )
  expect_equal(test_coverage$trait_coverage, 2/3)
  
  ## Coverage matrix
  # with only species for which we have traits
  expect_silent(
    test_coverage <- fbg_get_coverage(
      site_sp = matrix(c(0.3, 0, 0.3), nrow = 1, ncol = 3,
                       dimnames = list("s1", letters[1:3])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1"))
    )
  )
  expect_equal(test_coverage$trait_coverage, 1)
  # with species for which we don't have all the traits
  expect_silent(
    test_coverage <- fbg_get_coverage(
      site_sp = matrix(c(0.3, 0, 0.3, 0.3), nrow = 1, ncol = 4,
                       dimnames = list("s1", letters[1:4])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1"))
    )
  )
  expect_equal(test_coverage$trait_coverage, 2/3)
})

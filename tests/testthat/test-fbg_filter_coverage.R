test_that("fbg_filter_coverage() works", {
  # Wrong input
  expect_error(
    fbg_filter_coverage(data.frame("a"), data.frame("a")),
    "Provided site-species object is not a numeric matrix or a data.frame",
    fixed = TRUE
  )
  
  # No species in common
  expect_error(
    fbg_filter_coverage(
      site_sp = matrix(1, dimnames = list("s1", "a")),
      traits  = matrix(1, dimnames = list("b", "t1"))
    ),
    "No species found in common between inputs",
    fixed = TRUE
  )
  
  # No numeric threshold
  expect_error(
    fbg_filter_coverage(
      site_sp = matrix(1, dimnames = list("s1", "a")),
      traits  = matrix(1, dimnames = list("b", "t1")),
      coverage_threshold = "a"
    ),
    "Coverage threshold should be a numeric value >= 0 and <= 1",
    fixed = TRUE
  )
  
  # Threshold > 1
  expect_error(
    fbg_filter_coverage(
      site_sp = matrix(1, dimnames = list("s1", "a")),
      traits  = matrix(1, dimnames = list("b", "t1")),
      coverage_threshold = 2
    ),
    "Coverage threshold should be a numeric value >= 0 and <= 1",
    fixed = TRUE
  )
  
  # Threshold < 0
  expect_error(
    fbg_filter_coverage(
      site_sp = matrix(1, dimnames = list("s1", "a")),
      traits  = matrix(1, dimnames = list("b", "t1")),
      coverage_threshold = -1
    ),
    "Coverage threshold should be a numeric value >= 0 and <= 1",
    fixed = TRUE
  )
  
  # Trait with NAs
  expect_silent(
    filtered_site <- fbg_filter_coverage(
      site_sp = matrix(1, dimnames = list("s1", "a")),
      traits  = matrix(NA_real_, dimnames = list("a", "t1"))
    )
  )
  
  # Check output format
  expect_type(filtered_site, "double")
  expect_equal(nrow(filtered_site), 1L)
  expect_equal(ncol(filtered_site), 1L)
  
  ## Occurrence matrix
  # No site is selected
  expect_silent(
    test_coverage <- fbg_filter_coverage(
      site_sp = matrix(c(1, 0, 1), nrow = 1, ncol = 3, 
                       dimnames = list("s1", letters[1:3])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1")),
      coverage_threshold = 1
    )
  )
  expect_equal(test_coverage,
               matrix(c(1, 0, 1), nrow = 1, ncol = 3, 
                      dimnames = list("s1", letters[1:3])))
  # No site is selected
  expect_message(
    test_coverage <- fbg_filter_coverage(
      site_sp = matrix(c(1, 0, 1, 1), nrow = 1, ncol = 4, 
                       dimnames = list("s1", letters[1:4])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1")),
      coverage_threshold = 1
    ),
    "No sites had the specified trait coverage threshold",
    fixed = TRUE
  )
  expect_equal(test_coverage, NULL)
  
  # Lowering the threshold should work
  expect_silent(
    test_coverage <- fbg_filter_coverage(
      site_sp = matrix(c(1, 0, 1, 1), nrow = 1, ncol = 4, 
                       dimnames = list("s1", letters[1:4])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1")),
      coverage_threshold = 0.6
    )
  )
  expect_equal(test_coverage,
               matrix(c(1, 0, 1, 1), nrow = 1, ncol = 4, 
                      dimnames = list("s1", letters[1:4])))
  
  ## Abundance matrix
  # with only species for which we have traits
  expect_silent(
    test_coverage <- fbg_filter_coverage(
      site_sp = matrix(c(5, 0, 5), nrow = 1, ncol = 3,
                       dimnames = list("s1", letters[1:3])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1"))
    )
  )
  expect_equal(test_coverage, matrix(c(5, 0, 5), nrow = 1, ncol = 3,
                                     dimnames = list("s1", letters[1:3])))
  # with species for which we don't have all the traits
  expect_message(
    test_coverage <- fbg_filter_coverage(
      site_sp = matrix(c(5, 0, 5, 5), nrow = 1, ncol = 4,
                       dimnames = list("s1", letters[1:4])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1"))
    ),
    "No sites had the specified trait coverage threshold",
    fixed = TRUE
  )
  expect_equal(test_coverage, NULL)
  
  # Lowering the threshold should work
  expect_silent(
    test_coverage <- fbg_filter_coverage(
      site_sp = matrix(c(5, 0, 5, 5), nrow = 1, ncol = 4,
                       dimnames = list("s1", letters[1:4])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1")),
      coverage_threshold = 0.6
    )
  )
  expect_equal(test_coverage,
               matrix(c(5, 0, 5, 5), nrow = 1, ncol = 4,
                      dimnames = list("s1", letters[1:4])))
  
  ## Cover matrix
  # with only species for which we have traits
  expect_silent(
    test_coverage <- fbg_filter_coverage(
      site_sp = matrix(c(0.3, 0, 0.3), nrow = 1, ncol = 3,
                       dimnames = list("s1", letters[1:3])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1"))
    )
  )
  expect_equal(test_coverage,
               matrix(c(0.3, 0, 0.3), nrow = 1, ncol = 3,
                      dimnames = list("s1", letters[1:3])))
  # with species for which we don't have all the traits
  expect_message(
    test_coverage <- fbg_filter_coverage(
      site_sp = matrix(c(0.3, 0, 0.3, 0.3), nrow = 1, ncol = 4,
                       dimnames = list("s1", letters[1:4])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1"))
    ),
    "No sites had the specified trait coverage threshold",
    fixed = TRUE
  )
  expect_equal(test_coverage, NULL)
  
  # Lowering the threshold should work
  expect_silent(
    test_coverage <- fbg_filter_coverage(
      site_sp = matrix(c(0.3, 0, 0.3, 0.3), nrow = 1, ncol = 4,
                       dimnames = list("s1", letters[1:4])),
      traits  = matrix(1, nrow = 3, ncol = 1,
                       dimnames = list(letters[1:3], "t1")),
      coverage_threshold = 0.6
    )
  )
  expect_equal(test_coverage,
               matrix(c(0.3, 0, 0.3, 0.3), nrow = 1, ncol = 4,
                      dimnames = list("s1", letters[1:4])))
})

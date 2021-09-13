test_that("fbg_cwm() works", {
  # Wrong input
  expect_error(
    fbg_cwm(data.frame("a"), data.frame("a")),
    "Provided site-species object is not a numeric matrix or a data.frame",
    fixed = TRUE
  )
  
  # No species in common
  expect_error(
    fbg_cwm(
      site_sp = matrix(1, dimnames = list("s1", "a")),
      traits  = matrix(1, dimnames = list("b", "t1"))
    ),
    "No species found in common between inputs",
    fixed = TRUE
  )
  
  # No numeric traits
  expect_error(
    fbg_cwm(
      site_sp = matrix(1, dimnames = list("s1", "a")),
      traits  = matrix("a", dimnames = list("a", "t1"))
    ),
    "CWM can only be computed on numeric traits",
    fixed = TRUE
  )
  
  # Valid input
  expect_silent(
    test_cwm <- fbg_cwm(
      site_sp = matrix(1, dimnames = list("s1", "a")),
      traits  = matrix(1, dimnames = list("a", "t1"))
    )
  )
  
  expect_s3_class(test_cwm, "data.frame")
  expect_named(test_cwm, c("site", "trait", "cwm"))
  expect_equal(dim(test_cwm), c(1, 3))
  expect_equal(test_cwm$cwm, 1)
})

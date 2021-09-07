test_that("check_site_sp() works", {
  # Wrong input
  expect_error(
    check_site_sp("a"),
    "Provided site-species object is not a numeric matrix or a data.frame",
    fixed = TRUE
  )
  expect_error(
    check_site_sp(data.frame("a")),
    "Provided site-species object is not a numeric matrix or a data.frame",
    fixed = TRUE
  )
  
  # Matrix has no rows and/or columns
  expect_error(
    check_site_sp(matrix(ncol = 0, nrow = 0)),
    "Provided site-species object should have at least one row and one column",
    fixed = TRUE
  )
  expect_error(
    check_site_sp(matrix(ncol = 0, nrow = 1)),
    "Provided site-species object should have at least one row and one column",
    fixed = TRUE
  )
  expect_error(
    check_site_sp(matrix(ncol = 1, nrow = 0)),
    "Provided site-species object should have at least one row and one column",
    fixed = TRUE
  )
  
  
  # site-species should not contain negative values
  expect_error(
    check_site_sp(matrix(-1)),
    "Provided site-species matrix contains negative values",
    fixed = TRUE
  )
  expect_error(
    check_site_sp(matrix(c(-1, NA_real_))),
    "Provided site-species matrix contains negative values",
    fixed = TRUE
  )
  
  # Correct input
  expect_silent(check_site_sp(matrix(1)))
  expect_silent(check_site_sp(matrix(1L)))
  expect_silent(check_site_sp(matrix(NA_real_)))
  expect_silent(check_site_sp(matrix(c(1, NA_real_))))
})

test_that("check_traits() works", {
  # Matrix or data.frame has no rows and/or columns
  expect_error(
    check_traits(matrix(ncol = 0, nrow = 0)),
    paste0("Provided species-traits object should have at least one row and ",
           "one column"),
    fixed = TRUE
  )
  expect_error(
    check_traits(matrix(ncol = 0, nrow = 1)),
    paste0("Provided species-traits object should have at least one row and ",
           "one column"),
    fixed = TRUE
  )
  expect_error(
    check_traits(matrix(ncol = 1, nrow = 0)),
    paste0("Provided species-traits object should have at least one row and ",
           "one column"),
    fixed = TRUE
  )
  expect_error(
    check_traits(data.frame()),
    paste0("Provided species-traits object should have at least one row and ",
           "one column"),
    fixed = TRUE
  )
  
  # Correct Input
  expect_silent(matrix(1))
  expect_silent(matrix(1L))
  expect_silent(data.frame(1))
})
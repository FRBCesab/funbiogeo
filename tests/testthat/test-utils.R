test_that("list_common_species() works", {
  
  expect_error(
    list_common_species(letters[1:3], letters[4:6]),
    "No species found in common between inputs"
  )
  
  expect_error(
    list_common_species(c(letters[1:3], NA), letters[4:6]),
    "Species names cannot contain NA"
  )
  
  expect_error(
    list_common_species(letters[1:3], c(NA, letters[4:6])),
    "Species names cannot contain NA"
  )
  
  expect_error(
    list_common_species(c(letters[1:3], NA), c(NA, letters[4:6])),
    "Species names cannot contain NA"
  )
  
  expect_equal(
    list_common_species(letters[1:3], letters[1:3]), 
    letters[1:3])
  
})

test_that("weighted_mean() works", {
  expect_equal(weighted_mean(c(1, 1), c(1, 1)), 1)
  expect_equal(weighted_mean(c(2, 1), c(0.5, 0)), 2)
  expect_equal(weighted_mean(c(2000, 1), c(NA,  1), na.rm = FALSE), NA_real_)
  expect_equal(weighted_mean(c(NA, 1),   c(1e2, 1),  na.rm = FALSE), NA_real_)
  expect_equal(weighted_mean(c(2000, 1), c(NA,  1), na.rm = TRUE),  1)
  expect_equal(weighted_mean(c(NA, 1),  c(1e2,  1),  na.rm = TRUE),  1)
})

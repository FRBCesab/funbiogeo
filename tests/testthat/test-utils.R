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

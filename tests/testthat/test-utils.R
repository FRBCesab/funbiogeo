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


test_that("split_species_categories() works", {

  sp_tr <- data.frame(
    species = c("A", "B", "C"),
    trait = 1:3
  )

  sp_cat_good <- data.frame(
    species = c("A", "B", "F"),
    order = c("AAA", "CCC", "BBB")
  )

  sp_cat_wrong <- data.frame(
    species = c("D", "E", "F"),
    order = c("AAA", "AAA", "BBB")
  )
  
  # No species x categories
  expect_silent({res <- split_species_categories(sp_tr)})

  expect_true(inherits(res, "list"))
  expect_equal(length(res), 1L)
  expect_equal(nrow(res[[1]]), 3L)
  expect_equal(ncol(res[[1]]), 2L)

  # With species x categories & species in common
  expect_silent({res <- split_species_categories(sp_tr, sp_cat_good)})
  
  expect_true(inherits(res, "list"))
  expect_equal(length(res), 2L)
  expect_true("AAA" %in% names(res))
  expect_true("CCC" %in% names(res))
  expect_equal(nrow(res[[1]]), 1L)
  expect_equal(ncol(res[[1]]), 2L)
  expect_equal(nrow(res[[2]]), 1L)
  expect_equal(ncol(res[[2]]), 2L)

  expect_error(
    split_species_categories(sp_tr, sp_cat_wrong),
    paste0(
      "No species of 'species x traits' object found in the ", 
      "'species x categories' object"
    )
  )
})
# Errors on wrong input --------------------------------------------------------

test_that("fb_get_trait_combination_coverage() errors with wrong inputs", {
  
  expect_error(
    fb_get_trait_combination_coverage(site_species, species_traits, "a"),
    "Target combination length 'comb_size' should be numeric",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_trait_combination_coverage(site_species, species_traits, TRUE),
    "Target combination length 'comb_size' should be numeric",
    fixed = TRUE
  )
  
})


# Works with good input --------------------------------------------------------

test_that("fb_get_trait_combination_coverage() works with good inputs", {
  
  # All combinations
  expect_silent(
    res <- fb_get_trait_combination_coverage(site_species, species_traits)
  )
  
  expect_s3_class(res, "data.frame")
  
  expect_equal(
    colnames(res), c("site", "combination_length", "combination_name",
                     "trait_coverage")
  )
  expect_equal(res[1, 2], 1)
  expect_equal(res[1, 3], "adult_body_mass")
  
  # Given trait combination
  expect_silent(
    res <- fb_get_trait_combination_coverage(site_species, species_traits, 2)
  )
  
  expect_s3_class(res, "data.frame")
  
  expect_equal(
    colnames(res), c("site", "combination_length", "combination_name",
                     "trait_coverage")
  )
  expect_equal(res[1, 2], 2)
  expect_equal(res[1, 3], "adult_body_mass__gestation_length")
})
test_that("fb_get_all_coverages() errors with wrong arguments", {
  
  # Missing Arguments
  
  expect_error(
    fb_get_all_coverages(),
    "Argument 'site_species' (site x species data frame) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_all_coverages(site_species),
    "Argument 'species_traits' (species x traits data frame) is required",
    fixed = TRUE
  )
  
  
  # Wrong arguments
  
  expect_error(
    fb_get_all_coverages(data.frame()),
    "The site x species object should have at least one row and one column",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_all_coverages(site_species, data.frame()),
    "The species x traits object should have at least one row and one column",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_all_coverages(list()),
    "The site x species object must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_all_coverages(site_species, list()),
    "The species x traits object must be a data.frame",
    fixed = TRUE
  )
})


test_that("fb_get_all_coverages() works well with good arguments", {
  
  expect_silent(res <- fb_get_all_coverages(site_species, species_traits))
  
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(1505, 8))
  expect_equal(
    colnames(res), c("site", "all_traits", colnames(species_traits)[-1])
  )
  expect_equal(round(res[1, 2], 2), 0.73)
  expect_equal(round(res[2, 5], 2), 0.97)
})
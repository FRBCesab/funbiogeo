test_that("fb_get_all_trait_coverages_by_site() errors with wrong arguments", {
  
  # Missing Arguments
  
  expect_error(
    fb_get_all_trait_coverages_by_site(),
    "Argument 'site_species' (site x species data frame) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_all_trait_coverages_by_site(site_species),
    "Argument 'species_traits' (species x traits data frame) is required",
    fixed = TRUE
  )
  
  
  # Wrong arguments
  
  expect_error(
    fb_get_all_trait_coverages_by_site(data.frame()),
    "The site x species object should have at least one row and one column",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_all_trait_coverages_by_site(site_species, data.frame()),
    "The species x traits object should have at least one row and one column",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_all_trait_coverages_by_site(list()),
    "The site x species object must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_all_trait_coverages_by_site(site_species, list()),
    "The species x traits object must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_all_trait_coverages_by_site(site_species, species_traits, 1),
    "Argument 'all_traits' should be TRUE or FALSE",
    fixed = TRUE
  )
  
})


test_that("fb_get_all_trait_coverages_by_site() works well with good arguments",{
  
  # With all traits
  expect_silent(
    res <- fb_get_all_trait_coverages_by_site(site_species, species_traits)
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(1505, 8))
  expect_equal(
    colnames(res), c("site", "all_traits", colnames(species_traits)[-1])
  )
  expect_equal(round(res[1, 2], 2), 0.73)
  expect_equal(round(res[2, 5], 2), 0.97)
  
  # Without all traits
  expect_silent(
    res <- fb_get_all_trait_coverages_by_site(
      site_species, species_traits, FALSE
    )
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(1505, 7))
  expect_equal(
    colnames(res), c("site", colnames(species_traits)[-1])
  )
  expect_equal(round(res[2, 4], 2), 0.91)
})
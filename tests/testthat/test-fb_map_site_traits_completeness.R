test_that("fb_map_site_traits_completeness() works", {
  expect_silent(
    res <- fb_map_site_traits_completeness(
      site_locations, site_species, species_traits
    )
  )
  
  expect_s3_class(res, "ggplot")
  
  skip("Should test for polygonal sites")
})

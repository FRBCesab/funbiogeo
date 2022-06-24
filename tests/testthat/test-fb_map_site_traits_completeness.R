# Initial data -----------------------------------------------------------------
# Make sf points
site_points <- suppressWarnings(sf::st_centroid(site_locations))

# Make sf lines
site_lines <- sf::st_cast(site_locations, "MULTILINESTRING")

# Actual tests -----------------------------------------------------------------
test_that("fb_map_site_traits_completeness() works", {
  
  # With sf points
  expect_silent(
    res <- fb_map_site_traits_completeness(
      site_points, site_species, species_traits
    )
  )
  
  expect_s3_class(res, "ggplot")
  
  # With sf polygons
  expect_silent(
    res <- fb_map_site_traits_completeness(
      site_locations, site_species, species_traits
    )
  )
  
  expect_s3_class(res, "ggplot")
  
  # with sf lines
  expect_silent(
    res <- fb_map_site_traits_completeness(
      site_lines, site_species, species_traits
    )
  )
  
  expect_s3_class(res, "ggplot")
})

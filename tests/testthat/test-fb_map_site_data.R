# Initial data -----------------------------------------------------------------

site_rich <- fb_count_species_by_site(site_species)

# Make sf points
site_points <- suppressWarnings(sf::st_centroid(site_locations))

# Make sf lines
site_lines <- sf::st_cast(site_locations, "MULTILINESTRING")


# Tests ------------------------------------------------------------------------
test_that("fb_map_site_data() works", {
  
  # Polygons
  expect_silent(
    res <- fb_map_site_data(site_locations, site_rich, "n_species")
  )
  expect_s3_class(res, "ggplot")
  
  # Points
  expect_silent(
    res <- fb_map_site_data(site_points, site_rich, "n_species")
  )
  expect_s3_class(res, "ggplot")
  
  # Lines
  expect_silent(
    res <- fb_map_site_data(site_lines, site_rich, "n_species")
  )
  expect_s3_class(res, "ggplot")
})

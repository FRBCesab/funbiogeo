# Initial data -----------------------------------------------------------------

site_rich <- fb_count_species_by_site(site_species)

# Make sf points
site_points <- suppressWarnings(sf::st_centroid(site_locations))

# Make sf lines
site_lines <- sf::st_cast(site_locations, "MULTILINESTRING")


# Tests ------------------------------------------------------------------------
test_that("fb_map_site_data() errors with wrong input", {
  
  # Missing Arguments
  expect_error(
    res <- fb_map_site_data(site_locations),
    "Argument 'site_data' (site info data.frame) is required",
    fixed = TRUE
  )
  
  # Wrong type
  expect_error(
    res <- fb_map_site_data(site_locations, matrix()),
    "Argument 'site_data' must be a data.frame",
    fixed = TRUE
  )
  
  # Column 'site' not in data.frame
  expect_error(
    res <- fb_map_site_data(site_locations, data.frame()),
    "\"site\" column should be in provided 'site_data'",
    fixed = TRUE
  )
  
  # Missing 'selected_col' argument
  expect_error(
    res <- fb_map_site_data(site_locations, data.frame(site = "a")),
    "Argument 'selected_col' (name of selected column) is required",
    fixed = TRUE
  )
  
  # Specified 'selected_col' not in provided 'site_data'
  expect_error(
    res <- fb_map_site_data(site_locations, data.frame(site = "a", b = 1), "a"),
    "Provided 'selected_col' should be in 'site_data'",
    fixed = TRUE
  )
  
})

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

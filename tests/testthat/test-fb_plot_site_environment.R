# Initial data -----------------------------------------------------------------

# Site x locations object
data("site_locations")


# Environmental rasters
prec   <- system.file("extdata", "annual_tot_prec.tif", 
                      package = "funbiogeo")
tavg   <- system.file("extdata", "annual_mean_temp.tif", 
                      package = "funbiogeo")
layers <- terra::rast(c(tavg, prec))


# Transform 'sf' object into multiple types
# Points
site_points <- suppressWarnings(sf::st_centroid(site_locations))

# Multiline
site_lines <- sf::st_cast(site_locations[1,], "MULTILINESTRING")


# Test: Missing input ----------------------------------------------------------

test_that("fb_plot_site_environment() errors with missing input", {
  expect_error(
    fb_plot_site_environment(),
    "Argument 'sites_locations' (spatial sites 'sf' object) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_plot_site_environment(environment_raster = layers),
    "Argument 'sites_locations' (spatial sites 'sf' object) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_plot_site_environment(site_locations = site_locations),
    "Argument 'environment_raster' (environmental raster) is required",
    fixed = TRUE
  )
})


# Test: Wrong input type -------------------------------------------------------

test_that("fb_plot_site_environment() errors with wrong input type", {
  
  # Wrong site x locations object
  expect_error(
    fb_plot_site_environment(site_locations[[1]], layers),
    "The site x locations object must be an 'sf' object",
    fixed = TRUE
  )
  
  expect_error(
    fb_plot_site_environment(as.list(site_locations), layers),
    "The site x locations object must be an 'sf' object",
    fixed = TRUE
  )
  
  expect_error(
    fb_plot_site_environment(
      site_locations[-c(seq_len(nrow(site_locations))), ], layers
    ),
    "The site x locations object should have at least one row",
    fixed = TRUE
  )
  
  # Wrong environmental raster type
  expect_error(
    fb_plot_site_environment(site_locations, data.frame(c(tavg, prec))),
    "The raster layer must be a 'SpatRaster' object (package `terra`)",
    fixed = TRUE
  )
  
  # Non-existent layers
  expect_error(
    fb_plot_site_environment(site_locations, layers, "bla"),
    paste0("Argument 'first_layer' should be the name of a layer in provided ",
           "'environment_raster'")
  )
  
  expect_error(
    fb_plot_site_environment(site_locations, layers, second_layer = "bla"),
    paste0("Argument 'second_layer' should be the name of a layer in provided ",
           "'environment_raster'")
  )
})


# Test: Good Input -------------------------------------------------------------


test_that("fb_plot_site_environment() works", {
  
  # 'sf' points
  expect_silent(
    suppressWarnings(
      res <- fb_plot_site_environment(head(site_points), layers)
    )
  )
  
  expect_s3_class(res, "ggplot")
  
  # 'sf' polygons
  expect_silent(
    suppressWarnings(
      res <- fb_plot_site_environment(head(site_locations), layers)
    )
  )
  
  expect_s3_class(res, "ggplot")
  
  # 'sf' multline
  expect_silent(
    suppressWarnings(
      res <- fb_plot_site_environment(site_lines, layers)
    )
  )
  
  expect_s3_class(res, "ggplot")
})
# Initial data -----------------------------------------------------------------

# Site x locations object
data("site_locations")


# Environmental rasters
prec   <- system.file("extdata", "annual_tot_prec.tif", 
                      package = "funbiogeo")
tavg   <- system.file("extdata", "annual_mean_temp.tif", 
                      package = "funbiogeo")
layers <- terra::rast(c(tavg, prec))


# Test: Missing input ----------------------------------------------------------

test_that("fb_get_environment() errors with missing input", {
  expect_error(
    fb_get_environment(),
    "Argument 'site_locations' (site x locations 'sf' object) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_environment(environment_raster = layers),
    "Argument 'site_locations' (site x locations 'sf' object) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_environment(site_locations = site_locations),
    "Argument 'environment_raster' (environmental raster) is required",
    fixed = TRUE
  )
})


# Test: Wrong input type -------------------------------------------------------

test_that("fb_get_environment() errors with wrong input type", {
  
  # Wrong site x locations object
  expect_error(
    fb_get_environment(site_locations[[1]], layers),
    "The site x locations object must be an 'sf' object",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_environment(as.list(site_locations), layers),
    "The site x locations object must be an 'sf' object",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_environment(
      site_locations[-c(seq_len(nrow(site_locations))), ], layers
    ),
    "The site x locations object should have at least one row",
    fixed = TRUE
  )
  
  # Wrong environmental raster type
  expect_error(
    fb_get_environment(site_locations, raster::stack(c(tavg, prec))),
    "The raster layer must be a 'SpatRaster' object (package `terra`)",
    fixed = TRUE
  )
})


# Test: Good Input -------------------------------------------------------------

test_that("fb_get_environment() works", {
  
  # Regular input
  expect_silent(
    env_value <- fb_get_environment(
      suppressWarnings(sf::st_centroid(site_locations)), layers
    )
  )
  
  expect_s3_class(env_value, "data.frame")
  expect_named(env_value, c("site", "annual_mean_temp", "annual_tot_prec"))
  expect_equal(dim(env_value), c(1505, 3))
  expect_equal(round(env_value[["annual_tot_prec"]][[1]]), 2480)
  
  # Different CRS
  rob <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  
  layers_prj <- terra::project(layers, rob)
  
  expect_silent(
    env_value <- fb_get_environment(
      suppressWarnings(sf::st_centroid(site_locations)), layers_prj
    )
  )
  
  expect_s3_class(env_value, "data.frame")
  expect_named(env_value, c("site", "annual_mean_temp", "annual_tot_prec"))
  expect_equal(dim(env_value), c(1505, 3))
  expect_equal(env_value[["annual_mean_temp"]][[1]], 6.6, tolerance = 0.01)
})

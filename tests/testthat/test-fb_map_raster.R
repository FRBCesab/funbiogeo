test_that("fb_map_raster() works", {
  
  
  # Initial data ----
  
  data("site_locations")
  
  prec   <- system.file("extdata", "annual_tot_prec.tif", 
                        package = "funbiogeo")
  tavg   <- system.file("extdata", "annual_mean_temp.tif", 
                        package = "funbiogeo")
  layers <- terra::rast(c(tavg, prec))
  
  
  ## Wrong input ----
  
  expect_error(
    fb_map_raster(),
    "Argument 'x' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_map_raster(site_locations),
    "Argument 'x' must be a 'SpatRaster' object (package terra)",
    fixed = TRUE
  )
  
  expect_error(
    fb_map_raster(layers),
    "Argument 'x' must be a single layer 'SpatRaster' object",
    fixed = TRUE
  )
  
  x <- terra::rast(prec)
  expect_error(
    fb_map_raster(x, add = FALSE),
    NA)
  
  
  ## Working ----
  
  x <- terra::rast(prec)
  
  expect_silent(fb_map_raster(x))
  
  expect_s3_class(x <- fb_map_raster(x), "ggplot")
  
})

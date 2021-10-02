test_that("fb_get_environment() works", {
  
  # Initial data ----
  
  data("sites_locs")

  ## Import climate rasters ----
  
  prec   <- system.file("extdata", "annual_tot_prec.tif", 
                        package = "funbiogeo")
  tavg   <- system.file("extdata", "annual_mean_temp.tif", 
                        package = "funbiogeo")
  layers <- terra::rast(c(tavg, prec))
  
  
  # Wrong input ----
  
  expect_error(
    fb_get_environment(),
    "Argument 'sites_locations' (sites x locations matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_environment(environment_raster = layers),
    "Argument 'sites_locations' (sites x locations matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_environment(sites_locations = sites_locs),
    "Argument 'environment_raster' (environmental rasters) is required",
    fixed = TRUE
  )
  
  
  # Wrong input type ----
  
  expect_error(
    fb_get_environment(sites_locs[ , 1], layers),
    "The sites x locations object must be a matrix or a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_environment(as.list(sites_locs), layers),
    "The sites x locations object must be a matrix or a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_environment(data.frame(sites_locs[ , 1]), layers),
    paste0("The sites x locations object should have two columns (longitude ", 
           "and latitude)"),
    fixed = TRUE
  )
  
  expect_error(
    fb_get_environment(sites_locs[-c(seq_len(nrow(sites_locs))), ], layers),
    "The sites x locations object should have at least one row",
    fixed = TRUE
  )
  
  data_test <- sites_locs
  rownames(data_test) <- NULL
  
  expect_error(
    fb_get_environment(data_test, layers),
    "The sites x locations object must have row names (sites names)",
    fixed = TRUE
  )
  
  data_test <- data.matrix(data_test)
  
  expect_error(
    fb_get_environment(data_test, layers),
    "The sites x locations object must have row names (sites names)",
    fixed = TRUE
  )
  
  layers <- raster::stack(c(tavg, prec))
  
  expect_error(
    fb_get_environment(sites_locs, layers),
    "The raster layer must be a 'SpatRaster' object (package terra)",
    fixed = TRUE
  )
  
  layers <- terra::rast(c(tavg, prec))
  
  expect_error(
    fb_get_environment(sites_locs, layers, crs = NULL),
    "Argument 'crs' (coordinate system) must a character of length 1",
    fixed = TRUE
  )
  
  
  # Different CRS ----
  
  rob <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  
  layers_prj <- terra::project(layers, rob)
  
  expect_silent(env_value <- fb_get_environment(sites_locs, layers_prj))
  
  expect_s3_class(env_value, "data.frame")
  expect_named(env_value, c("site", "annual_mean_temp", "annual_tot_prec"))
  expect_equal(dim(env_value), c(5770, 3))
  expect_equal(env_value[["annual_mean_temp"]][[1]], 11.0539, tolerance = 0.01)
  
  
  # Good input ----

  expect_silent(env_value <- fb_get_environment(sites_locs, layers))
  
  expect_s3_class(env_value, "data.frame")
  expect_named(env_value, c("site", "annual_mean_temp", "annual_tot_prec"))
  expect_equal(dim(env_value), c(5770, 3))
  expect_equal(env_value[["annual_tot_prec"]][[1]], 1121, tolerance = 0.000001)
})

# Initial data -----------------------------------------------------------------

# Site x locations object
data("sites_locs")

sites_locs <- sites_locs[!duplicated(sites_locs),]
sites_locs[["site"]] <- rownames(sites_locs)
rownames(sites_locs) <- NULL
sites_locs <- sf::st_as_sf(
  sites_locs, coords = 1:2, crs = 4326
)

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
    "Argument 'sites_locations' (sites x locations 'sf' object) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_environment(environment_raster = layers),
    "Argument 'sites_locations' (sites x locations 'sf' object) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_environment(sites_locations = sites_locs),
    "Argument 'environment_raster' (environmental raster) is required",
    fixed = TRUE
  )
})


# Test: Wrong input type -------------------------------------------------------

test_that("fb_get_environment() errors with wrong input type", {
  
  # Wrong site x locations object
  expect_error(
    fb_get_environment(sites_locs[[1]], layers),
    "The sites x locations object must be an 'sf' object",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_environment(as.list(sites_locs), layers),
    "The sites x locations object must be an 'sf' object",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_environment(sites_locs[-c(seq_len(nrow(sites_locs))), ], layers),
    "The sites x locations object should have at least one row",
    fixed = TRUE
  )
  
  # Wrong environmental raster
  
  expect_error(
    fb_get_environment(sites_locs, raster::stack(c(tavg, prec))),
    "The raster layer must be a 'SpatRaster' object (package `terra`)",
    fixed = TRUE
  )
  
  # Wrong CRS value
  expect_error(
    fb_get_environment(sites_locs, layers, crs = NULL),
    "Argument 'crs' (coordinate system) must a character of length 1",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_environment(sites_locs, layers, crs = 1),
    "Argument 'crs' (coordinate system) must a character of length 1",
    fixed = TRUE
  )
})


# Test: Good Input -------------------------------------------------------------

test_that("fb_get_environment() works", {
  
  # Regular input
  expect_silent(env_value <- fb_get_environment(sites_locs, layers))
  
  expect_s3_class(env_value, "data.frame")
  expect_named(env_value, c("site", "annual_mean_temp", "annual_tot_prec"))
  expect_equal(dim(env_value), c(3265, 3))
  expect_equal(env_value[["annual_tot_prec"]][[1]], 1121)
  
  # Different CRS
  rob <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  
  layers_prj <- terra::project(layers, rob)
  
  expect_silent(env_value <- fb_get_environment(sites_locs, layers_prj))
  
  expect_s3_class(env_value, "data.frame")
  expect_named(env_value, c("site", "annual_mean_temp", "annual_tot_prec"))
  expect_equal(dim(env_value), c(3265, 3))
  expect_equal(env_value[["annual_mean_temp"]][[1]], 11.0539, tolerance = 0.01)
})

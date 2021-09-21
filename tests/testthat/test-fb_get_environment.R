test_that("fb_get_environment() works", {
  
  skip_if_not_installed("sf")
  skip_if_not_installed("raster")
  
  
  # Initial data ----
  
  srtm = raster::raster("data_srtm.tif")
  zion = sf::st_read("data_zion_points.gpkg")
  zion = sf::st_transform(zion, raster::crs(srtm))
  
  
  # Wrong input ----
  
  # Missing input
  expect_error(
    fb_get_environment(environment_raster = srtm),
    "Argument 'site_locations' (sites x location object) is required",
    fixed = TRUE
  )
  expect_error(
    fb_get_environment(site_locations = zion),
    "Argument 'environment_raster' (environmental raster) is required",
    fixed = TRUE
  )
  
  # Wrong input type
  
  # Different input CRSs
  
  
  # Good input ----
  # Input without site name
  expect_silent(
    env_value <- fb_get_environment(zion, srtm)
  )
  
  expect_s3_class(env_value, "data.frame")
  expect_named(env_value, c("ID", "data_srtm"))
  expect_equal(dim(env_value), c(30, 2))
  expect_equal(env_value[["data_srtm"]][[1]], 1802, tolerance = 0.000001)
  
  # With site names (it should give the name of the given sites)
  
})

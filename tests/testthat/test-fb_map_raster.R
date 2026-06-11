data("woodiv_locations")
site_locations <- woodiv_locations

prec <- system.file("extdata", "annual_tot_prec.tif", package = "funbiogeo")
tavg <- system.file("extdata", "annual_mean_temp.tif", package = "funbiogeo")
layers <- terra::rast(c(tavg, prec))

test_that("fb_map_raster() errors", {
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
    NA
  )

  expect_error(
    fb_map_raster(x, background = 3),
    "The 'background' argument should either be TRUE or FALSE"
  )
})


test_that("fb_map_raster() works", {
  if (is_fedora()) {
    skip("Skip tests on Fedora Linux")
  }

  x <- terra::rast(prec)

  expect_silent({x <- fb_map_raster(x)})

  expect_s3_class(x, "ggplot")

  vdiffr::expect_doppelganger(
    "fb_map_raster-default",
    x
  )

  x <- terra::rast(prec)

  expect_silent({y <- fb_map_raster(x, background = TRUE)})

  expect_s3_class(y, "ggplot")

  vdiffr::expect_doppelganger(
    "fb_map_raster-background",
    y
  )
})

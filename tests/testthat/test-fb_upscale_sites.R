test_that("fb_upscale_sites() works", {
  
  data("sites_locs")
  data("species_occs")
  
  tavg_file <- system.file("extdata", "annual_mean_temp.tif", 
                           package = "funbiogeo")
  tavg <- terra::rast(tavg_file)
  
  # Wrong input ----
  
  expect_error(
    fb_upscale_sites(),
    "Argument 'sites_locations' (sites x locations matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_upscale_sites(sites_locs),
    "Argument 'data' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_upscale_sites(sites_locs, as.list(species_occs)),
    "Argument 'data' must be a matrix or a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_upscale_sites(sites_locs, 
                     species_occs[-c(seq_len(nrow(species_occs))), ]),
    "Argument 'data' should have at least one row and one column",
    fixed = TRUE
  )
  
  data_test <- as.data.frame(species_occs)
  rownames(data_test) <- NULL
  
  expect_error(
    fb_upscale_sites(sites_locs, data_test),
    "Argument 'data' must have row names (sites names)",
    fixed = TRUE
  )
  
  data_test <- data.matrix(data_test)
  
  expect_error(
    fb_upscale_sites(sites_locs, data_test),
    "Argument 'data' must have row names (sites names)",
    fixed = TRUE
  )
  
  data_test <- species_occs
  data_test <- data.frame(data_test, test = rep("A", nrow(data_test)))
  
  expect_error(
    fb_upscale_sites(sites_locs, data_test),
    paste0("Argument 'data' must contain only numeric values. Sites names ", 
           "must be provided as row names"),
    fixed = TRUE
  )
  
  expect_error(
    fb_upscale_sites(sites_locs, species_occs),
    "Argument 'grid' is required",
    fixed = TRUE
  )
  
  grid_test <- raster::raster(tavg_file)
  
  expect_error(
    fb_upscale_sites(sites_locs, species_occs, grid_test),
    "The 'grid' raster must be a 'SpatRaster' object (package terra)",
    fixed = TRUE
  )
  
  grid_test <- terra::rast(tavg_file)
  terra::crs(grid_test) <- NA
  
  expect_error(
    fb_upscale_sites(sites_locs, species_occs, grid_test),
    "The 'grid' raster must have a CRS (coordinate system)",
    fixed = TRUE
  )
  
  expect_error(
    fb_upscale_sites(sites_locs, species_occs, tavg, crs = NULL),
    "Argument 'crs' (coordinate system) must be a character of length 1",
    fixed = TRUE
  )
  
  
  ## Working ----
  
  expect_silent(ras <- fb_upscale_sites(sites_locs, species_occs[ , 1:2], tavg))
  
  expect_s4_class(ras, "SpatRaster")
  expect_named(ras, c("acer_negundo", "acer_nigrum"))
  expect_equal(dim(ras), c(61, 139, 2))
  expect_equal(ras[][1401], 0, tolerance = 0.000001)
  
  
  ## Projection ----
  
  rob <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  
  tavg_prj <- terra::project(tavg, rob)
  
  expect_silent(ras <- fb_upscale_sites(sites_locs, species_occs[ , 1:2], 
                                        tavg_prj))
  
  expect_s4_class(ras, "SpatRaster")
  expect_named(ras, c("acer_negundo", "acer_nigrum"))
})

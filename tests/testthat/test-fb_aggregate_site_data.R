# Initial data -----------------------------------------------------------------
data("site_locs")
data("species_occs")

# Convert site locations in to an 'sf' object
site_locs <- site_locs[!duplicated(site_locs),]
site_locs[["site"]] <- rownames(site_locs)
rownames(site_locs) <- NULL
site_locs <- sf::st_as_sf(
  site_locs, coords = 1:2, crs = 4326
)


# Get proper raster file
tavg_file <- system.file("extdata", "annual_mean_temp.tif", 
                         package = "funbiogeo")
tavg <- terra::rast(tavg_file)


# Test: Missing Input ----------------------------------------------------------

test_that("fb_aggregate_site_data() errors with missing input", {
  
  expect_error(
    fb_aggregate_site_data(),
    "Argument 'site_locations' (site x locations matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_aggregate_site_data(site_locs),
    "Argument 'site_data' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_aggregate_site_data(site_locs, species_occs),
    "Argument 'agg_grid' is required",
    fixed = TRUE
  )
})

# Test: Wrong Input ------------------------------------------------------------

test_that("fb_aggregate_site_data() errors with wrong input", {
  
  # Wrong 'site_data' argument
  expect_error(
    fb_aggregate_site_data(site_locs, as.list(species_occs)),
    "Argument 'site_data' must be a matrix or a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_aggregate_site_data(site_locs, 
                     species_occs[-c(seq_len(nrow(species_occs))), ]),
    "Argument 'site_data' should have at least one row and one column",
    fixed = TRUE
  )
  
  data_test <- as.data.frame(species_occs)
  rownames(data_test) <- NULL
  
  expect_error(
    fb_aggregate_site_data(site_locs, data_test),
    "Argument 'site_data' must have row names (sites names)",
    fixed = TRUE
  )
  
  data_test <- data.matrix(data_test)
  
  expect_error(
    fb_aggregate_site_data(site_locs, data_test),
    "Argument 'site_data' must have row names (sites names)",
    fixed = TRUE
  )
  
  data_test <- species_occs
  data_test <- data.frame(data_test, test = rep("A", nrow(data_test)))
  
  expect_error(
    fb_aggregate_site_data(site_locs, data_test),
    paste0("Argument 'site_data' must contain only numeric values. ",
           "Sites names must be provided as row names"),
    fixed = TRUE
  )
  
  # Wrong 'agg_grid' argument
  grid_test <- raster::raster(tavg_file)
  
  expect_error(
    fb_aggregate_site_data(site_locs, species_occs, grid_test),
    "The 'agg_grid' raster must be a 'SpatRaster' object (package `terra`)",
    fixed = TRUE
  )
  
  grid_test <- terra::rast(tavg_file)
  terra::crs(grid_test) <- NA
  
  expect_error(
    fb_aggregate_site_data(site_locs, species_occs, grid_test),
    "The 'agg_grid' raster must have a CRS (coordinate system)",
    fixed = TRUE
  )
})


# Test: Good input -------------------------------------------------------------

test_that("fb_aggregate_site_data() works", {
  
  # No reprojection
  expect_silent(ras <- fb_aggregate_site_data(site_locs, species_occs[ , 1:2], tavg))
  
  expect_s4_class(ras, "SpatRaster")
  expect_named(ras, c("acer_negundo", "acer_nigrum"))
  expect_equal(dim(ras), c(61, 139, 2))
  expect_equal(ras[][1401], 0, tolerance = 0.000001)
  
  
  # Change projection of rasters
  rob <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  
  tavg_prj <- terra::project(tavg, rob)
  
  expect_silent(ras <- fb_aggregate_site_data(site_locs, species_occs[ , 1:2], 
                                        tavg_prj))
  
  expect_s4_class(ras, "SpatRaster")
  expect_named(ras, c("acer_negundo", "acer_nigrum"))
})
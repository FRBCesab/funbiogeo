# Initial data from the package ------------------------------------------------
data("woodiv_locations")
data("woodiv_site_species")

site_locations <- woodiv_locations
site_species   <- woodiv_site_species


# Prepare input site_locations data --------------------------------------------
# Points
site_points <- suppressWarnings(sf::st_centroid(site_locations))

# Multiline
site_lines <- sf::st_cast(site_locations[1,], "MULTILINESTRING")


# Grid input data --------------------------------------------------------------
# Raster grid
tavg_file <- system.file("extdata", "annual_mean_temp.tif", 
                         package = "funbiogeo")
tavg <- terra::rast(tavg_file)

# Square grid
square_grid <- st_make_grid(head(site_locations), cellsize = c(300e3, 300e3)) |>
  sf::st_as_sf()

# Irregular polygons (countries)
countries <- system.file(
  "extdata", "countries_sf.rds", package = "funbiogeo"
) |>
  readRDS()

# Lines
transects <- system.file(
  "extdata", "woodiv_transects.rds", package = "funbiogeo"
) |>
  readRDS()


# Test: Missing Input ----------------------------------------------------------

test_that("fb_aggregate_site_data() errors with missing input", {
  
  expect_error(
    fb_aggregate_site_data(),
    "Argument 'sites_locations' (spatial sites 'sf' object) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_aggregate_site_data(site_locations),
    "Argument 'site_data' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_aggregate_site_data(site_locations, site_species),
    "Argument 'agg_geom' is required",
    fixed = TRUE
  )
})

# Test: Wrong Input ------------------------------------------------------------

test_that("fb_aggregate_site_data() errors with wrong input", {
  
  # Wrong 'site_data' argument
  expect_error(
    fb_aggregate_site_data(site_locations, as.list(site_species)),
    "Argument 'site_data' must be a matrix or a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_aggregate_site_data(site_locations, 
                           site_species[-c(seq_len(nrow(site_species))), ]),
    "Argument 'site_data' should have at least one row and one column",
    fixed = TRUE
  )
  
  # Wrong 'agg_geom' argument
  grid_test <- data.frame(tavg_file)
  
  expect_error(
    fb_aggregate_site_data(site_locations, site_species, grid_test),
    paste0(
      "The 'agg_geom' raster must be a 'SpatRaster' (package `terra`) or an ",
      "'sf' object"
    ),
    fixed = TRUE
  )
  
  grid_test <- terra::rast(tavg_file)
  terra::crs(grid_test) <- NA
  
  expect_error(
    fb_aggregate_site_data(site_locations, site_species, grid_test),
    "The 'agg_geom' raster must have a CRS (coordinate system)",
    fixed = TRUE
  )
})


# Test: Good input -------------------------------------------------------------

test_that("fb_aggregate_site_data() works", {
  
  ## Points spatial data
  # Unprojected raster
  expect_silent(
    ras <- fb_aggregate_site_data(site_points, site_species[, 1:3], tavg)
  )
  
  expect_s4_class(ras, "SpatRaster")
  expect_named(ras, c("country", "AALB", "ACEP"))
  expect_equal(dim(ras), c(290, 405, 3))
  expect_equal(ras[][82006], 2, tolerance = 0.000001)
  
  
  # Projected raster
  rob <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  
  tavg_prj <- terra::project(tavg, rob)
  
  expect_silent(
    ras <- fb_aggregate_site_data(site_points, site_species[, 1:3], tavg_prj)
  )
  
  expect_s4_class(ras, "SpatRaster")
  expect_named(ras, c("country", "AALB", "ACEP"))
  
  
  ## Polygon spatial data
  # Regular
  expect_silent(
    ras <- fb_aggregate_site_data(site_locations, site_species[, 1:3], tavg)
  )
  
  expect_s4_class(ras, "SpatRaster")
  expect_named(ras, c("country", "AALB", "ACEP"))
  expect_equal(dim(ras), c(290, 405, 3))
  expect_equal(ras[][82006], 2, tolerance = 0.000001)
  
  # Projected Raster
  expect_silent(
    ras <- fb_aggregate_site_data(site_locations, site_species[, 1:3], tavg_prj)
  )
  
  expect_s4_class(ras, "SpatRaster")
  expect_named(ras, c("country", "AALB", "ACEP"))
  
  # Square grid
  expect_silent(
    agg <- fb_aggregate_site_data(
      site_locations, site_species[, 1:3], square_grid
    )
  )
  
  expect_s3_class(agg, "sf")
  expect_named(agg, c("AALB", "ACEP", "geometry"))
  
  
  # Irregular polygons (countries)
  expect_silent(
    agg <- fb_aggregate_site_data(
      site_locations, site_species[, 1:3], countries
    )
  )
  
  expect_s3_class(agg, "sf")
  expect_named(agg, c("AALB", "ACEP", "geometry"))
  
  
  # Lines
  expect_silent(
    agg <- fb_aggregate_site_data(
      site_locations, site_species[, 1:3], transects
    )
  )
  
  expect_s3_class(agg, "sf")
  expect_named(agg, c("AALB", "ACEP", "geometry"))
  
  
  
  ## Multiline spatial data
  # Unprojected raster
  expect_silent(
    ras <- fb_aggregate_site_data(site_lines, site_species[, 1:3], tavg)
  )
  
  expect_s4_class(ras, "SpatRaster")
  expect_named(ras, c("country", "AALB", "ACEP"))
  expect_equal(dim(ras), c(290, 405, 3))
  
  # Projected Raster
  expect_silent(
    ras <- fb_aggregate_site_data(site_lines, site_species[, 1:3], tavg_prj)
  )
  
  expect_s4_class(ras, "SpatRaster")
  expect_named(ras, c("country", "AALB", "ACEP"))
  
  
  
  
})

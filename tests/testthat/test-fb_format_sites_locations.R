test_that("fb_format_sites_locations() works", {
  
  filename <- system.file("extdata", "raw_trees_data.csv", 
                          package = "funbiogeo")
  all_data <- read.csv2(filename)
  
  
  # Wrong inputs ----
  
  expect_error(
    fb_format_sites_locations(),
    "Argument 'data' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data[ , 1]),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(as.list(all_data)),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(data.frame(all_data[ , 1])),
    "Argument 'data' must be a data.frame with at least two columns",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data[-c(1:nrow(all_data)), ]),
    "Argument 'data' must be a data.frame with at least one row",
    fixed = TRUE
  )
  
  
  # Argument site ----
  
  expect_error(
    fb_format_sites_locations(all_data),
    "Argument 'site' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, 1),
    "Argument 'site' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, c("site", "country")),
    "Argument 'site' must be a character of length 1 (column name)",
    fixed = TRUE
  )

  expect_error(
    fb_format_sites_locations(all_data, NULL),
    "Argument 'site' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, NA),
    "Argument 'site' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, "location"),
    "The column 'location' is absent from 'data'",
    fixed = TRUE
  )
  
  
  # Argument longitude ----
  
  expect_error(
    fb_format_sites_locations(all_data, "site"),
    "Argument 'longitude' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, "site", 3),
    "Argument 'longitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, "site", c("longitude", "x_utm")),
    "Argument 'longitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, "site", NULL),
    "Argument 'longitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, "site", NA),
    "Argument 'longitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, "site", "x_utm"),
    "The column 'x_utm' is absent from 'data'",
    fixed = TRUE
  )
  
  all_data_test <- all_data
  all_data_test$"longitude" <- as.character(all_data_test$"longitude")
  
  expect_error(
    fb_format_sites_locations(all_data_test, "site", "longitude"),
    "The column 'longitude' is must be a numeric",
    fixed = TRUE
  )
  
  
  # Argument latitude ----
  
  expect_error(
    fb_format_sites_locations(all_data, "site", "longitude"),
    "Argument 'latitude' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, "site", "longitude", 4),
    "Argument 'latitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, "site", "longitude", 
                              c("latitude", "y")),
    "Argument 'latitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, "site", "longitude", NULL),
    "Argument 'latitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, "site", "longitude", NA),
    "Argument 'latitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, "site", "longitude", "y_utm"),
    "The column 'y_utm' is absent from 'data'",
    fixed = TRUE
  )
  
  all_data_test <- all_data
  all_data_test$"latitude" <- as.character(all_data_test$"latitude")
  
  expect_error(
    fb_format_sites_locations(all_data_test, "site", "longitude", "latitude"),
    "The column 'latitude' is must be a numeric",
    fixed = TRUE
  )
  
  
  # Argument na_rm ----
  
  expect_error(
    fb_format_sites_locations(all_data, "site", "longitude", "latitude", 
                              na_rm = 0),
    "Argument 'na_rm' must be TRUE or FALSE",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, "site", "longitude", "latitude", 
                              na_rm = 1),
    "Argument 'na_rm' must be TRUE or FALSE",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_locations(all_data, "site", "longitude", "latitude", 
                              na_rm = 0:1),
    "Argument 'na_rm' must be TRUE or FALSE",
    fixed = TRUE
  )
  
  
  # Non-unique coordinates ----
  
  all_data_test <- all_data
  all_data_test <- rbind(all_data_test[1, ], all_data_test)
  all_data_test[1, "longitude"] <- 9999
  
  expect_error(
    fb_format_sites_locations(all_data_test, "site", "longitude", "latitude"),
    "Some sites have non-unique longitude",
    fixed = TRUE
  )
  
  all_data_test <- all_data
  all_data_test <- rbind(all_data_test[1, ], all_data_test)
  all_data_test[1, "latitude"] <- 9999
  
  expect_error(
    fb_format_sites_locations(all_data_test, "site", "longitude", "latitude"),
    "Some sites have non-unique latitude",
    fixed = TRUE
  )
  
  
  # Working ----
  
  sites_locations <- fb_format_sites_locations(all_data, "site", "longitude", 
                                               "latitude")
  expect_true(is.matrix(sites_locations))
  expect_equal(nrow(sites_locations), 836L)
  expect_equal(ncol(sites_locations), 2L)
  expect_false("site" %in% colnames(sites_locations))
  expect_equal(sites_locations[1, 2], 41.88894)
  
  
  # NA values ----
  
  sites_locations <- fb_format_sites_locations(all_data, "site", "longitude", 
                                               "latitude", na_rm = TRUE)
  
  expect_true(is.matrix(sites_locations))
  expect_equal(nrow(sites_locations), 836L)
  
  all_data_test <- all_data
  all_data_test[1, "longitude"] <- NA
  all_data_test[2, "latitude"] <- NA
  
  sites_locations <- fb_format_sites_locations(all_data_test, "site", 
                                               "longitude", "latitude", 
                                               na_rm = TRUE)
  
  expect_true(is.matrix(sites_locations))
  expect_equal(nrow(sites_locations), 834L)
})

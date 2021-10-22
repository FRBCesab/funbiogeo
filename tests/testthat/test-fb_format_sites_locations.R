# Initial data -----------------------------------------------------------------

filename <- system.file("extdata", "raw_trees_data.csv", 
                        package = "funbiogeo")
all_data <- read.csv2(filename)


# Test: Missing input ----------------------------------------------------------

test_that("fb_format_site_locations() errors with missing input", {
  expect_error(
    fb_format_site_locations(),
    "Argument 'input_data' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data),
    "Argument 'site' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data, "site"),
    "Argument 'longitude' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data, "site", "longitude"),
    "Argument 'latitude' is required",
    fixed = TRUE
  )
})

# Test: Missing cols in input --------------------------------------------------
test_that("fb_format_site_locations() errors with columns missing in input", {
  
  # site column in input_data
  expect_error(
    fb_format_site_locations(all_data, "location"),
    "The column 'location' is absent from 'input_data'",
    fixed = TRUE
  )
  
  # longitude column in input_data
  expect_error(
    fb_format_site_locations(all_data, "site", "x_utm"),
    "The column 'x_utm' is absent from 'input_data'",
    fixed = TRUE
  )
  
  # latitude column in input_data
  expect_error(
    fb_format_site_locations(all_data, "site", "longitude", "y_utm"),
    "The column 'y_utm' is absent from 'input_data'",
    fixed = TRUE
  )
  
  
})


# Test: Wrong input type -------------------------------------------------------
test_that("fb_format_site_locations() errors with wrong input type", {
  
  # Argument 'input_data'
  expect_error(
    fb_format_site_locations(all_data[ , 1]),
    "Argument 'input_data' must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(as.list(all_data)),
    "Argument 'input_data' must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(data.frame(all_data[ , 1])),
    "Argument 'input_data' must be a data.frame with at least two columns",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data[-c(seq_len(nrow(all_data))), ]),
    "Argument 'input_data' must be a data.frame with at least one row",
    fixed = TRUE
  )
  
  # Argument 'site'
  expect_error(
    fb_format_site_locations(all_data, 1),
    "Argument 'site' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data, c("site", "country")),
    "Argument 'site' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data, NULL),
    "Argument 'site' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data, NA),
    "Argument 'site' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  # Argument 'longitude'
  expect_error(
    fb_format_site_locations(all_data, "site", 3),
    "Argument 'longitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data, "site", c("longitude", "x_utm")),
    "Argument 'longitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data, "site", NULL),
    "Argument 'longitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data, "site", NA),
    "Argument 'longitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  # Argument 'latitude'
  expect_error(
    fb_format_site_locations(all_data, "site", "longitude", 4),
    "Argument 'latitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data, "site", "longitude", 
                              c("latitude", "y")),
    "Argument 'latitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data, "site", "longitude", NULL),
    "Argument 'latitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data, "site", "longitude", NA),
    "Argument 'latitude' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  # Wrong latitude and longitude type
  all_data_test <- all_data
  all_data_test$"longitude" <- as.character(all_data_test$"longitude")
  
  expect_error(
    fb_format_site_locations(all_data_test, "site", "longitude"),
    "The column 'longitude' is must be a numeric",
    fixed = TRUE
  )
  
  all_data_test <- all_data
  all_data_test$"latitude" <- as.character(all_data_test$"latitude")
  
  expect_error(
    fb_format_site_locations(all_data_test, "site", "longitude", "latitude"),
    "The column 'latitude' is must be a numeric",
    fixed = TRUE
  )
  
  # Argument crs
  expect_error(
    fb_format_site_locations(all_data, "site", "longitude", "latitude", "bla"),
    "Argument 'crs' should be valid CRS or coercible to one with sf::st_crs()",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data, "site", "longitude", "latitude", 4.2),
    "Argument 'crs' should be valid CRS or coercible to one with sf::st_crs()",
    fixed = TRUE
  )
  
  # Argument na_rm
  expect_error(
    fb_format_site_locations(all_data, "site", "longitude", "latitude", 
                              na_rm = 0),
    "Argument 'na_rm' must be TRUE or FALSE",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data, "site", "longitude", "latitude", 
                              na_rm = 1),
    "Argument 'na_rm' must be TRUE or FALSE",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_site_locations(all_data, "site", "longitude", "latitude", 
                              na_rm = 0:1),
    "Argument 'na_rm' must be TRUE or FALSE",
    fixed = TRUE
  )
  

})


# Test: Valid data -------------------------------------------------------------
test_that("fb_format_site_locations() works with valid input", {
  
  # Regular Input
  site_locations <- fb_format_site_locations(all_data, "site", "longitude", 
                                               "latitude")
  expect_true(is(site_locations, "sf"))
  expect_equal(nrow(site_locations), 836L)
  expect_equal(ncol(site_locations), 2L)
  expect_true("site" %in% colnames(site_locations))
  expect_equal(sf::st_coordinates(site_locations)[1,1], 39.91707)
  
  
  # Including NA values
  all_data_test <- all_data
  all_data_test[1, "longitude"] <- NA
  all_data_test[2, "latitude"] <- NA
  
  site_locations <- fb_format_site_locations(all_data_test, "site", 
                                               "longitude", "latitude", 
                                               na_rm = TRUE)
  expect_equal(nrow(site_locations), 834L)
})

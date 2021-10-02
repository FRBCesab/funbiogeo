test_that("fb_format_sites_species() works", {
  
  filename <- system.file("extdata", "raw_trees_data.csv", 
                          package = "funbiogeo")
  all_data <- read.csv2(filename)
  
  
  # Wrong inputs ----
  
  expect_error(
    fb_format_sites_species(),
    "Argument 'data' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data[ , 1]),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(as.list(all_data)),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(data.frame(all_data[ , 1])),
    "Argument 'data' must be a data.frame with at least two columns",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data[-c(seq_len(nrow(all_data))), ]),
    "Argument 'data' must be a data.frame with at least one row",
    fixed = TRUE
  )
  
  
  # Argument site ----
  
  expect_error(
    fb_format_sites_species(all_data),
    "Argument 'site' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, 1),
    "Argument 'site' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, c("site", "country")),
    "Argument 'site' must be a character of length 1 (column name)",
    fixed = TRUE
  )

  expect_error(
    fb_format_sites_species(all_data, NULL),
    "Argument 'site' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, NA),
    "Argument 'site' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, "location"),
    "The column 'location' is absent from 'data'",
    fixed = TRUE
  )
  
  
  # Argument species ----
  
  expect_error(
    fb_format_sites_species(all_data, "site"),
    "Argument 'species' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, "site", 2),
    "Argument 'species' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, "site", c("genus", "species")),
    "Argument 'species' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, "site", NULL),
    "Argument 'species' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, "site", NA),
    "Argument 'species' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, "site", "genus"),
    "The column 'genus' is absent from 'data'",
    fixed = TRUE
  )
  
  
  # Argument value ----
  
  expect_error(
    fb_format_sites_species(all_data, "site", "species"),
    "Argument 'value' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, "site", "species", 5),
    "Argument 'value' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, "site", "species", c("count", "abund")),
    "Argument 'value' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, "site", "species", NULL),
    "Argument 'value' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, "site", "species", NA),
    "Argument 'value' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, "site", "species", "abund"),
    "The column 'abund' is absent from 'data'",
    fixed = TRUE
  )
  
  all_data_test <- all_data
  all_data_test$"count" <- as.character(all_data_test$"count")
  
  expect_error(
    fb_format_sites_species(all_data_test, "site", "species", "count"),
    "The column 'count' is must be a numeric",
    fixed = TRUE
  )
  
  
  # Argument na_to_zero ----
  
  expect_error(
    fb_format_sites_species(all_data, "site", "species", "count", 
                            na_to_zero = 0),
    "Argument 'na_to_zero' must be TRUE or FALSE",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, "site", "species", "count", 
                            na_to_zero = 1),
    "Argument 'na_to_zero' must be TRUE or FALSE",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_sites_species(all_data, "site", "species", "count", 
                            na_to_zero = 0:1),
    "Argument 'na_to_zero' must be TRUE or FALSE",
    fixed = TRUE
  )
  
  
  # Working ----
  
  sites_species <- fb_format_sites_species(all_data, "site", "species", "count",
                                           na_to_zero = TRUE)
  expect_true(is.matrix(sites_species))
  expect_equal(nrow(sites_species), 836L)
  expect_equal(ncol(sites_species), 3L)
  expect_false("site" %in% colnames(sites_species))
  expect_equal(sites_species[1, 2], 0L)
  
  sites_species <- fb_format_sites_species(all_data, "site", "species", "count",
                                           na_to_zero = FALSE)
  expect_equal(sites_species[1, 2], NA_real_)
})

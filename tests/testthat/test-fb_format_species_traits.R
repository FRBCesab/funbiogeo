test_that("fb_format_species_traits() works", {
  
  filename <- system.file("extdata", "raw_mammals_data.csv", 
                          package = "funbiogeo")
  all_data <- read.csv(filename)
  
  
  # Wrong inputs ----
  
  expect_error(
    fb_format_species_traits(),
    "Argument 'data' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_traits(all_data[ , 1]),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_traits(as.list(all_data)),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_traits(data.frame(all_data[ , 
                                                -c(seq_len(ncol(all_data)))])),
    "Argument 'data' must be a data.frame with at least one column",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_traits(all_data[-c(seq_len(nrow(all_data))), ]),
    "Argument 'data' must be a data.frame with at least one row",
    fixed = TRUE
  )
  
  
  # Argument species ----
  
  expect_error(
    fb_format_species_traits(all_data),
    "Argument 'species' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_traits(all_data, 1),
    "Argument 'species' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_traits(all_data, c("site", "country")),
    "Argument 'species' must be a character of length 1 (column name)",
    fixed = TRUE
  )

  expect_error(
    fb_format_species_traits(all_data, NULL),
    "Argument 'species' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_traits(all_data, NA),
    "Argument 'species' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_traits(all_data, "location"),
    "The column 'location' is absent from 'data'",
    fixed = TRUE
  )
  
  
  # Arguments traits ----
  
  expect_error(
    fb_format_species_traits(all_data, "species"),
    "Argument 'traits' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_traits(all_data, "species", 3),
    "Argument 'traits' must be a character of length >= 1 (column names)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_traits(all_data, "species", NULL),
    "Argument 'traits' must be a character of length >= 1 (column names)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_traits(all_data, "site", NA),
    "Argument 'traits' must be a character of length >= 1 (column names)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_traits(all_data, "species", c("adult_body_mass", 
                                                    "gestation_length2",
                                                    "litter_size",
                                                    "max_longevity",
                                                    "sexual_maturity_age",
                                                    "diet_breadth")),
    "Some traits columns are absent from 'data'",
    fixed = TRUE
  )
  
  
  # Non-unique traits values per species ----
  
  all_data_test <- all_data
  all_data_test <- rbind(all_data_test[1, ], all_data_test)
  all_data_test[1, "adult_body_mass"] <- 9999
  
  expect_error(
    fb_format_species_traits(all_data_test, "species", c("adult_body_mass", 
                                                         "gestation_length",
                                                         "litter_size",
                                                         "max_longevity",
                                                         "sexual_maturity_age",
                                                         "diet_breadth")),
    "Some species have non-unique trait values",
    fixed = TRUE
  )
  
  all_data_test <- all_data
  all_data_test <- rbind(all_data_test[1, ], all_data_test)
  all_data_test[1, "adult_body_mass"] <- NA
  
  expect_error(
    fb_format_species_traits(all_data_test, "species", c("adult_body_mass", 
                                                         "gestation_length",
                                                         "litter_size",
                                                         "max_longevity",
                                                         "sexual_maturity_age",
                                                         "diet_breadth")),
    "Some species have non-unique trait values",
    fixed = TRUE
  )
  
  
  # Working ----
  
  species_traits <- fb_format_species_traits(all_data, "species", 
                                             c("adult_body_mass", 
                                               "gestation_length",
                                               "litter_size",
                                               "max_longevity",
                                               "sexual_maturity_age",
                                               "diet_breadth"))
  expect_true(is.data.frame(species_traits))
  expect_equal(nrow(species_traits), 10L)
  expect_equal(ncol(species_traits), 6L)
  expect_false("species" %in% colnames(species_traits))
  expect_equal(species_traits[1, 2], 235, tolerance = 7)
  
})

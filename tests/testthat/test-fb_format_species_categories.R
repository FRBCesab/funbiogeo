test_that("fb_format_species_categories() works", {
  
  filename <- system.file("extdata", "woodiv_raw_data.csv", 
                          package = "funbiogeo")
  all_data <- read.csv(filename)
  
  
  # Wrong inputs ----
  
  expect_error(
    fb_format_species_categories(),
    "Argument 'data' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_categories(all_data[ , 1]),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_categories(as.list(all_data)),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_categories(data.frame(all_data[ , 
                                                  -c(seq_len(ncol(all_data)))])),
    "Argument 'data' must be a data.frame with at least two columns",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_categories(all_data[-c(seq_len(nrow(all_data))), ]),
    "Argument 'data' must be a data.frame with at least one row",
    fixed = TRUE
  )
  
  
  # Argument species ----
  
  expect_error(
    fb_format_species_categories(all_data),
    "Argument 'species' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_categories(all_data, 1),
    "Argument 'species' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_categories(all_data, c("site", "country")),
    "Argument 'species' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_categories(all_data, NULL),
    "Argument 'species' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_categories(all_data, NA),
    "Argument 'species' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_categories(all_data, "location"),
    "The column 'location' is absent from 'data'",
    fixed = TRUE
  )
  
  
  # Arguments category ----
  
  expect_error(
    fb_format_species_categories(all_data, "species"),
    "Argument 'category' is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_categories(all_data, "species", 3),
    "Argument 'category' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_categories(all_data, "species", NULL),
    "Argument 'category' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_categories(all_data, "species", NA),
    "Argument 'category' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_categories(all_data, "species", c("site", "country")),
    "Argument 'category' must be a character of length 1 (column name)",
    fixed = TRUE
  )
  
  expect_error(
    fb_format_species_categories(all_data, "species", "category"),
    "The column 'category' is absent from 'data'",
    fixed = TRUE
  )
  
  
  # Non-unique categories per species ----
  
  test_data <- all_data
  test_data[1, "genus"] <- "Pyrus"

  expect_error(
    fb_format_species_categories(test_data, "species", "genus"),
    "Some species have non-unique category values",
    fixed = TRUE
  )
  
  # Working ----
  
  species_categories <- fb_format_species_categories(
    all_data, "species", "genus"
  )
  expect_true(is.data.frame(species_categories))
  expect_equal(nrow(species_categories), 24L)
  expect_equal(ncol(species_categories), 2L)
  expect_true("species" %in% colnames(species_categories))
  expect_equal(species_categories[1, 2], "Juniperus")
})

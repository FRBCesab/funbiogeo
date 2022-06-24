# Fake datasets ----------------------------------------------------------------

species_traits <- data.frame(
  species = paste0("sp", 1:4),
  t1      = c(NA, NA, 100, 400),
  t2      = c(2.2, 5.0, 200, 200)
)

species_traits2 <- data.frame(
  species = paste0("sp", 1:4),
  t1      = c(NA, NA, NA, NA),
  t2      = c(2.2, 5.0, 200, NA)
)

species_traits3 <- data.frame(
  species = paste0("sp", 1:4),
  t1      = c(NA, 2.5, 100, 400),
  t2      = c(NA, 5.0, 200, 200)
)

species_traits4 <- data.frame(
  species = paste0("sp", 1:4),
  t1      = c(400, 400, 400, 400),
  t2      = c(NA, 5.0, 200, 200)
)

# Wrong inputs ----------------------------------------------------------------

test_that("fb_filter_traits_by_species_coverage() errors with wrong inputs", {
  

  # Missing argument  
  expect_error(
    fb_filter_traits_by_species_coverage(),
    "Argument 'species_traits' (species x traits data frame) is required",
    fixed = TRUE
  )
  
  expect_error(
    {
      st2 <- species_traits
      colnames(st2) <- NULL
      fb_filter_traits_by_species_coverage(st2)
    },
    "The species x traits object must have column names (trait names)",
    fixed = TRUE
  )
  
  expect_error(
    fb_filter_traits_by_species_coverage(species_traits[ , -1, drop = FALSE]),
    "The species x traits object must contain the 'species' column",
    fixed = TRUE
  )
  
  
  # No numeric threshold
  expect_error(
    fb_filter_traits_by_species_coverage(species_traits, 
                                         threshold_species_proportion = "a"),
    paste0("Argument 'threshold_species_proportion' (species coverage ",
           "proportion) must be numeric"),
    fixed = TRUE
  )
  
  
  # Threshold > 1
  expect_error(
    fb_filter_traits_by_species_coverage(species_traits, 
                                         threshold_species_proportion = 2),
    paste0("Argument 'threshold_species_proportion' ",
           "(species coverage proportion) should be a numeric value >= 0 and ",
           "<= 1"),
    fixed = TRUE
  )
  
  
  # Threshold < 0
  expect_error(
    fb_filter_traits_by_species_coverage(species_traits, 
                                         threshold_species_proportion = -1),
    paste0("Argument 'threshold_species_proportion' ",
           "(species coverage proportion) should be a numeric value >= 0 and ",
           "<= 1"),
    fixed = TRUE
  )
  
  
  # Check for only NA for some traits
  expect_message(
    fb_filter_traits_by_species_coverage(species_traits2, 
                                         threshold_species_proportion = 0),
    "Some traits have only NA values. Maybe you would like to remove them.",
    fixed = TRUE
  )
  
  expect_message(
    fb_filter_traits_by_species_coverage(
      species_traits4, threshold_species_proportion = 0
    ),
    paste0("Some traits have no variability (one single value). ",
           "Maybe you would like to remove them."),
    fixed = TRUE
  )
  
  expect_message(
    test_coverage <- fb_filter_traits_by_species_coverage(
      species_traits3, threshold_species_proportion = 1
    ),
    "No trait has the specified species coverage threshold",
    fixed = TRUE
  )
  
  expect_s3_class(test_coverage, "data.frame")
  expect_equal(nrow(test_coverage), 0)
  expect_equal(colnames(test_coverage), colnames(species_traits3))
})


# Well-formed input ------------------------------------------------------------
test_that("fb_filter_traits_by_species_coverage() successully works", {
  
  expect_silent(
    test_coverage <- fb_filter_traits_by_species_coverage(species_traits, 0)
  )
  
  expect_identical(ncol(test_coverage), ncol(species_traits))
  
  expect_silent(
    test_coverage <- fb_filter_traits_by_species_coverage(species_traits, 1)
  )
  
  expect_s3_class(test_coverage, "data.frame")
  expect_named(test_coverage, c("species", "t2"))
  expect_type(test_coverage$"species", "character")
  expect_type(test_coverage$"t2", "double")
  expect_equal(test_coverage$"species"[1], "sp1")
  expect_equal(test_coverage$"t2"[1], 2.2)
  
  # Test for one trait with all NA
  expect_message(
    test_coverage <- fb_filter_traits_by_species_coverage(species_traits2, 0.75)
  )
  
  expect_equal(ncol(test_coverage), 2)
  
  # Test for one species with all NA
  expect_silent(
    test_coverage <- fb_filter_traits_by_species_coverage(species_traits3, 0.1)
  )
  
  expect_equal(nrow(test_coverage), 4)
})

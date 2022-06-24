# Fake datasets ----

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
  t1      = c(NA, 2.5, NA, 400),
  t2      = c(2.2, NA, 200, NA)
)

test_that("fb_filter_species_by_trait_coverage() errors with wrong inputs", {
  
  # Wrong inputs ----
  
  expect_error(
    fb_filter_species_by_trait_coverage(),
    "Argument 'species_traits' (species x traits data frame) is required",
    fixed = TRUE
  )
  
  expect_error(
    {
      st2 <- species_traits
      colnames(st2) <- NULL
      fb_filter_species_by_trait_coverage(st2)
    },
    "The species x traits object must have column names (trait names)",
    fixed = TRUE
  )
  
  expect_error(
    fb_filter_species_by_trait_coverage(species_traits[ , -1, drop = FALSE]),
    "The species x traits object must contain the 'species' column",
    fixed = TRUE
  )
  
  
  # No numeric threshold ----
  
  expect_error(
    fb_filter_species_by_trait_coverage(species_traits, 
                                         threshold_traits_proportion = "a"),
    paste0("Argument 'threshold_traits_proportion' (trait coverage proportion)",
           " must be numeric"),
    fixed = TRUE
  )
  
  
  # Threshold > 1 ----
  
  expect_error(
    fb_filter_species_by_trait_coverage(species_traits, 
                                         threshold_traits_proportion = 2),
    paste0("Argument 'threshold_traits_proportion' (trait coverage proportion)",
           " should be a numeric value >= 0 and <= 1"),
    fixed = TRUE
  )
  
  
  # Threshold < 0 ----
  
  expect_error(
    fb_filter_species_by_trait_coverage(species_traits, 
                                         threshold_traits_proportion = -1),
    paste0("Argument 'threshold_traits_proportion' (trait coverage proportion)",
           " should be a numeric value >= 0 and <= 1"),
    fixed = TRUE
  )
  
  
  # Check for only NA for some species ----
  
  expect_message(
    test_na_trait <- fb_filter_species_by_trait_coverage(
      species_traits2, threshold_traits_proportion = 0
    ),
    paste0("Some species have only NA values for all traits. ",
           "Maybe you would like to remove them."),
    fixed = TRUE
  )
})

# Well-formed inputs -----------------------------------------------------------

test_that("fb_filter_species_by_trait_coverage() successully works", {
  
  
  expect_silent(
    test_coverage <- fb_filter_species_by_trait_coverage(species_traits, 0)
  )
  
  expect_identical(nrow(test_coverage), nrow(species_traits))
  
  expect_silent(
    test_coverage <- fb_filter_species_by_trait_coverage(species_traits, 1)
  )
  
  
  # Output format
  expect_s3_class(test_coverage, "data.frame")
  expect_named(test_coverage, c("species", "t1", "t2"))
  expect_type(test_coverage$"species", "character")
  expect_type(test_coverage$"t1", "double")
  expect_type(test_coverage$"t2", "double")
  expect_equal(test_coverage$"species"[1], "sp3")
  expect_equal(test_coverage$"t2"[1], 200)
  
  # Test for one trait with all NA
  expect_message(
    test_coverage <- fb_filter_species_by_trait_coverage(species_traits2, 0.5),
    paste0("Some species have only NA values for all traits. ",
           "Maybe you would like to remove them."),
    fixed = TRUE
  )
  
  expect_equal(nrow(test_coverage), 3)
  
  # Test for one species with all NA
  expect_message(
    test_coverage <- fb_filter_species_by_trait_coverage(species_traits3, 0.1),
    paste0("Some species have only NA values for all traits. ",
           "Maybe you would like to remove them."),
    fixed = TRUE
  )
  
  expect_equal(nrow(test_coverage), 3)
  
  # Test for no species selected
  
  expect_message(
    test_coverage <- fb_filter_species_by_trait_coverage(species_traits4, 1),
    "No species has the specified traits coverage threshold",
    fixed = TRUE
  )
  
  expect_identical(test_coverage, species_traits4[NULL,])
})

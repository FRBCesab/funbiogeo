test_that("fb_filter_coverage() works", {
  
  
  sites_species  <- matrix(c(1, 10, 10, 1, 10, 1), ncol = 3)
  species_traits <- matrix(c(1.1, 2.5, 100, 400), ncol = 2)
  
  
  # Wrong inputs ----
  
  expect_error(
    fb_filter_coverage(species_traits = species_traits),
    "Argument 'sites_species' (sites x species matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_filter_coverage(sites_species = sites_species),
    "Argument 'species_traits' (species x traits matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_filter_coverage(sites_species, species_traits),
    "The sites x species object must have row names (sites names)",
    fixed = TRUE
  )
  
  rownames(sites_species) <- paste0("site_", 1:nrow(sites_species))
  
  expect_error(
    fb_filter_coverage(sites_species, species_traits),
    "The sites x species object must have column names (species names)",
    fixed = TRUE
  )
  
  colnames(sites_species) <- paste0("species_", LETTERS[1:ncol(sites_species)])
  
  expect_error(
    fb_filter_coverage(sites_species, species_traits),
    "The species x traits object must have row names (sites names)",
    fixed = TRUE
  )
  
  rownames(species_traits) <- paste0("species_", 
                                     LETTERS[1:ncol(species_traits)])
  
  expect_error(
    fb_filter_coverage(sites_species, species_traits),
    "The species x traits object must have column names (species names)",
    fixed = TRUE
  )
  
  colnames(species_traits) <- paste0("trait_", 1:ncol(species_traits))
  
  
  # No species in common ----
  
  rownames(species_traits) <- paste0("species_", 
                                     LETTERS[10 + (1:ncol(species_traits))])
  
  expect_error(
    fb_filter_coverage(sites_species, species_traits),
    "No species found in common between inputs",
    fixed = TRUE
  )
  
  
  # No numeric threshold ----
  
  expect_error(
    fb_filter_coverage(
      sites_species  = matrix(1, dimnames = list("s1", "a")),
      species_traits = matrix(1, dimnames = list("b", "t1")),
      coverage_threshold = "a"),
    "Coverage threshold should be a numeric value >= 0 and <= 1",
    fixed = TRUE
  )
  
  
  # Threshold > 1 ----
  
  expect_error(
    fb_filter_coverage(
      sites_species  = matrix(1, dimnames = list("s1", "a")),
      species_traits = matrix(1, dimnames = list("b", "t1")),
      coverage_threshold = 2),
    "Coverage threshold should be a numeric value >= 0 and <= 1",
    fixed = TRUE
  )
  
  
  # Threshold < 0 ----
  
  expect_error(
    fb_filter_coverage(
      sites_species  = matrix(1, dimnames = list("s1", "a")),
      species_traits = matrix(1, dimnames = list("b", "t1")),
      coverage_threshold = -1),
    "Coverage threshold should be a numeric value >= 0 and <= 1",
    fixed = TRUE
  )
  
  # Trait with NAs ----
  
  expect_silent(
    filtered_site <- fb_filter_coverage(
      sites_species  = matrix(1, dimnames = list("s1", "a")),
      species_traits = matrix(NA_real_, dimnames = list("a", "t1")))
  )
  
  # Check output format ----
  
  expect_type(filtered_site, "double")
  expect_equal(nrow(filtered_site), 1L)
  expect_equal(ncol(filtered_site), 1L)
  
  
  # Occurrence matrix ----
  # (no site is selected)
  
  expect_silent(
    test_coverage <- fb_filter_coverage(
      sites_species  = matrix(c(1, 0, 1), nrow = 1, ncol = 3, 
                              dimnames = list("s1", letters[1:3])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1")),
      coverage_threshold = 1)
  )
  
  expect_equal(test_coverage, matrix(c(1, 0, 1), nrow = 1, ncol = 3, 
                                     dimnames = list("s1", letters[1:3])))
  
  
  # Occurrence matrix ----
  # (no site is selected)
  
  expect_message(
    test_coverage <- fb_filter_coverage(
      sites_species  = matrix(c(1, 0, 1, 1), nrow = 1, ncol = 4, 
                              dimnames = list("s1", letters[1:4])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1")),
      coverage_threshold = 1),
    "No sites has the specified trait coverage threshold",
    fixed = TRUE
  )
  
  expect_true(is.matrix(test_coverage))
  expect_equal(nrow(test_coverage), 0L)
  expect_equal(ncol(test_coverage), 4L)
  
  
  # Occurrence matrix ----
  # Lowering the threshold should work ----
  
  expect_silent(
    test_coverage <- fb_filter_coverage(
      sites_species  = matrix(c(1, 0, 1, 1), nrow = 1, ncol = 4, 
                              dimnames = list("s1", letters[1:4])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1")),
      coverage_threshold = 0.6)
  )
  
  expect_equal(test_coverage, matrix(c(1, 0, 1, 1), nrow = 1, ncol = 4, 
                                     dimnames = list("s1", letters[1:4])))
  
  # Abundance matrix ----
  # (with only species for which we have species_traits)
  
  expect_silent(
    test_coverage <- fb_filter_coverage(
      sites_species  = matrix(c(5, 0, 5), nrow = 1, ncol = 3,
                              dimnames = list("s1", letters[1:3])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1")))
  )
  
  expect_equal(test_coverage, matrix(c(5, 0, 5), nrow = 1, ncol = 3,
                                     dimnames = list("s1", letters[1:3])))
  
  # Abundance matrix ----
  # (with species for which we don't have all the species_traits)
  
  expect_message(
    test_coverage <- fb_filter_coverage(
      sites_species  = matrix(c(5, 0, 5, 5), nrow = 1, ncol = 4,
                              dimnames = list("s1", letters[1:4])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1"))),
    "No sites has the specified trait coverage threshold",
    fixed = TRUE
  )
  
  expect_true(is.matrix(test_coverage))
  expect_equal(nrow(test_coverage), 0L)
  expect_equal(ncol(test_coverage), 4L)
  
  
  # Abundance matrix ----
  # Lowering the threshold should work
  
  expect_silent(
    test_coverage <- fb_filter_coverage(
      sites_species  = matrix(c(5, 0, 5, 5), nrow = 1, ncol = 4,
                              dimnames = list("s1", letters[1:4])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1")),
      coverage_threshold = 0.6)
  )
  
  expect_equal(test_coverage, matrix(c(5, 0, 5, 5), nrow = 1, ncol = 4,
                                     dimnames = list("s1", letters[1:4])))
  
  
  # Cover matrix ----
  # (with only species for which we have species_traits)
  
  expect_silent(
    test_coverage <- fb_filter_coverage(
      sites_species  = matrix(c(0.3, 0, 0.3), nrow = 1, ncol = 3,
                              dimnames = list("s1", letters[1:3])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1")))
  )
  
  expect_equal(test_coverage, matrix(c(0.3, 0, 0.3), nrow = 1, ncol = 3,
                                     dimnames = list("s1", letters[1:3])))
  
  
  # Cover matrix ----
  # (with species for which we don't have all the species_traits)
  
  expect_message(
    test_coverage <- fb_filter_coverage(
      sites_species  = matrix(c(0.3, 0, 0.3, 0.3), nrow = 1, ncol = 4,
                              dimnames = list("s1", letters[1:4])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1"))),
    "No sites has the specified trait coverage threshold",
    fixed = TRUE
  )
  
  expect_true(is.matrix(test_coverage))
  expect_equal(nrow(test_coverage), 0L)
  expect_equal(ncol(test_coverage), 4L)
  
  
  # Cover matrix ----
  # Lowering the threshold should work
  
  expect_silent(
    test_coverage <- fb_filter_coverage(
      sites_species  = matrix(c(0.3, 0, 0.3, 0.3), nrow = 1, ncol = 4,
                              dimnames = list("s1", letters[1:4])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1")),
      coverage_threshold = 0.6)
  )
  
  expect_equal(test_coverage, matrix(c(0.3, 0, 0.3, 0.3), nrow = 1, ncol = 4,
                                     dimnames = list("s1", letters[1:4])))
  
})

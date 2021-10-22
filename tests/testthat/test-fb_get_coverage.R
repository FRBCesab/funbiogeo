test_that("fb_get_coverage() works", {
  
  
  site_species  <- matrix(c(1, 10, 10, 1, 10, 1), ncol = 3)
  species_traits <- matrix(c(1.1, 2.5, 100, 400), ncol = 2)
  
  
  # Wrong inputs ----
  
  expect_error(
    fb_get_coverage(species_traits = species_traits),
    "Argument 'site_species' (site x species matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_coverage(site_species = site_species),
    "Argument 'species_traits' (species x traits matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_coverage(site_species, species_traits),
    "The site x species object must have row names (sites names)",
    fixed = TRUE
  )
  
  rownames(site_species) <- paste0("site_", seq_len(nrow(site_species)))
  
  expect_error(
    fb_get_coverage(site_species, species_traits),
    "The site x species object must have column names (species names)",
    fixed = TRUE
  )
  
  colnames(site_species) <- paste0("species_", LETTERS[
    seq_len(ncol(site_species))])
  
  expect_error(
    fb_get_coverage(site_species, species_traits),
    "The species x traits object must have row names (species names)",
    fixed = TRUE
  )
  
  rownames(species_traits) <- paste0("species_", 
                                     LETTERS[seq_len(ncol(species_traits))])
  
  expect_error(
    fb_get_coverage(site_species, species_traits),
    "The species x traits object must have column names (traits names)",
    fixed = TRUE
  )
  
  colnames(species_traits) <- paste0("trait_", seq_len(ncol(species_traits)))
  
  
  # No species in common ----
  
  rownames(species_traits) <- paste0("species_", 
                                     LETTERS[10 + 
                                               (seq_len(ncol(species_traits)))])
  
  expect_error(
    fb_get_coverage(site_species, species_traits),
    "No species found in common between inputs",
    fixed = TRUE
  )
  
  
  # Trait with NAs ----
  
  expect_silent(
    test_coverage <- fb_get_coverage(
      site_species  = matrix(1, dimnames = list("s1", "a")),
      species_traits = matrix(NA_real_, dimnames = list("a", "t1"))
    )
  )
  
  
  # Check output format ----
  
  expect_s3_class(test_coverage, "data.frame")
  expect_named(test_coverage, c("site", "trait_coverage"))
  expect_type(test_coverage$"site", "character")
  expect_type(test_coverage$"trait_coverage", "double")
  expect_equal(test_coverage$"site", "s1")
  expect_equal(test_coverage$"trait_coverage", 1)
  
  
  # Occurrence matrix ----
  # (with only species for which we have traits)
  
  expect_silent(
    test_coverage <- fb_get_coverage(
      site_species  = matrix(c(1, 0, 1), nrow = 1, ncol = 3,
                              dimnames = list("s1", letters[1:3])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1")))
  )
  
  expect_equal(test_coverage$"trait_coverage", 1)
  
  
  # Occurrence matrix ----
  # (with species for which we don't have all the traits)
  
  expect_silent(
    test_coverage <- fb_get_coverage(
      site_species  = matrix(c(1, 0, 1, 1), nrow = 1, ncol = 4, 
                              dimnames = list("s1", letters[1:4])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1")))
  )
  
  expect_equal(test_coverage$"trait_coverage", 2 / 3)
  
  
  # Abundance matrix ----
  # (with only species for which we have traits)
  
  expect_silent(
    test_coverage <- fb_get_coverage(
      site_species  = matrix(c(5, 0, 5), nrow = 1, ncol = 3,
                              dimnames = list("s1", letters[1:3])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1")))
  )
  
  expect_equal(test_coverage$"trait_coverage", 1)
  
  
  # Abundance matrix ----
  # (with species for which we don't have all the traits)
 
  expect_silent(
    test_coverage <- fb_get_coverage(
      site_species  = matrix(c(5, 0, 5, 5), nrow = 1, ncol = 4,
                              dimnames = list("s1", letters[1:4])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1")))
  )
  
  expect_equal(test_coverage$"trait_coverage", 2 / 3)
  
  
  # Coverage matrix ----
  # (with only species for which we have traits)
  
  expect_silent(
    test_coverage <- fb_get_coverage(
      site_species  = matrix(c(0.3, 0, 0.3), nrow = 1, ncol = 3,
                              dimnames = list("s1", letters[1:3])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1")))
  )
  
  expect_equal(test_coverage$"trait_coverage", 1)
  
  
  # Coverage matrix ----
  # (with species for which we don't have all the traits)
  
  expect_silent(
    test_coverage <- fb_get_coverage(
      site_species  = matrix(c(0.3, 0, 0.3, 0.3), nrow = 1, ncol = 4,
                              dimnames = list("s1", letters[1:4])),
      species_traits = matrix(1, nrow = 3, ncol = 1,
                              dimnames = list(letters[1:3], "t1")))
  )
  
  expect_equal(test_coverage$"trait_coverage", 2 / 3)
})

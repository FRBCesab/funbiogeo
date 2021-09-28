test_that("fb_cwm() works", {
  
  sites_species  <- matrix(c(1, 10, 10, 1, 10, 1), ncol = 3)
  species_traits <- matrix(c(1.1, 2.5, 100, 400), ncol = 2)
  
  
  # Wrong inputs ----

  expect_error(
    fb_cwm(species_traits = species_traits),
    "Argument 'sites_species' (sites x species matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_cwm(sites_species = sites_species),
    "Argument 'species_traits' (species x traits matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_cwm(species_traits = species_traits),
    "Argument 'sites_species' (sites x species matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_cwm(sites_species = sites_species),
    "Argument 'species_traits' (species x traits matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_cwm(sites_species, species_traits),
    "The sites x species object must have row names (sites names)",
    fixed = TRUE
  )
  
  rownames(sites_species) <- paste0("site_", 1:nrow(sites_species))
  
  expect_error(
    fb_cwm(sites_species, species_traits),
    "The sites x species object must have column names (species names)",
    fixed = TRUE
  )
  
  colnames(sites_species) <- paste0("species_", LETTERS[1:ncol(sites_species)])
  
  expect_error(
    fb_cwm(sites_species, species_traits),
    "The species x traits object must have row names (species names)",
    fixed = TRUE
  )
  
  rownames(species_traits) <- paste0("species_", 
                                     LETTERS[1:ncol(species_traits)])
  
  expect_error(
    fb_cwm(sites_species, species_traits),
    "The species x traits object must have column names (traits names)",
    fixed = TRUE
  )
  
  colnames(species_traits) <- paste0("trait_", 1:ncol(species_traits))

  
  
  # No species in common ----
  
  rownames(species_traits) <- paste0("species_", 
                                     LETTERS[10 + (1:ncol(species_traits))])
  
  expect_error(
    fb_cwm(sites_species, species_traits),
    "No species found in common between inputs",
    fixed = TRUE
  )
  
  
  # No numeric traits ----
  
  species_traits <- matrix(LETTERS[1:4], ncol = 2)
  colnames(species_traits) <- paste0("trait_", 1:ncol(species_traits))
  rownames(species_traits) <- paste0("species_", 
                                     LETTERS[1:ncol(species_traits)])
  
  expect_error(
    fb_cwm(sites_species, species_traits),
    "CWM can only be computed on numeric traits",
    fixed = TRUE
  )
  
  
  # Valid input ----
  
  species_traits <- matrix(c(1.1, 2.5, 100, 400), ncol = 2)
  colnames(species_traits) <- paste0("trait_", 1:ncol(species_traits))
  rownames(species_traits) <- paste0("species_", 
                                     LETTERS[1:ncol(species_traits)])
  
  expect_silent(
    test_cwm <- fb_cwm(sites_species, species_traits)
  )
  
  expect_s3_class(test_cwm, "data.frame")
  expect_named(test_cwm, c("site", "trait", "cwm"))
  expect_equal(dim(test_cwm), c(4, 3))
  expect_equal(test_cwm$"cwm"[1], 2.372727, tolerance = 0.000001)
  
})

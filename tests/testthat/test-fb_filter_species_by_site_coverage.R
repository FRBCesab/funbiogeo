# Fake dataset ----

site_species <- data.frame(
  site = 1:4,
  sp1  = c( 0, 1, 0, 1),
  sp2  = c( 1, 1, 0, 0)
)

site_species2 <- data.frame(
  site = 1:4,
  sp1  = c(NA, NA, NA, NA),
  sp2  = c( 1,  1,  1,  1)
)

site_species3 <- data.frame(
  site = 1:4,
  sp1  = c(NA, 1, 0, 0),
  sp2  = c(NA, 1, 1, 1)
)

test_that("fb_filter_species_by_site_coverage() errors with wrong inputs", {
  
  # Wrong inputs ----
  
  expect_error(
    fb_filter_species_by_site_coverage(),
    "Argument 'site_species' (site x species data frame) is required",
    fixed = TRUE
  )
  
  expect_error(
    {
      st2 <- site_species
      colnames(st2) <- NULL
      fb_filter_species_by_site_coverage(st2)
    },
    "The site x species object must have column names (species names)",
    fixed = TRUE
  )
  
  expect_error(
    fb_filter_species_by_site_coverage(site_species[ , -1, drop = FALSE]),
    "The site x species object must contain the 'site' column",
    fixed = TRUE
  )
  
  
  # No numeric threshold ----
  
  expect_error(
    fb_filter_species_by_site_coverage(site_species, 
                                        threshold_sites_proportion = "a"),
    "Coverage threshold should be a numeric value >= 0 and <= 1",
    fixed = TRUE
  )
  
  
  # Threshold > 1 ----
  
  expect_error(
    fb_filter_species_by_site_coverage(site_species, 
                                        threshold_sites_proportion = 2),
    "Coverage threshold should be a numeric value >= 0 and <= 1",
    fixed = TRUE
  )
  
  
  # Threshold < 0 ----
  
  expect_error(
    fb_filter_species_by_site_coverage(site_species, 
                                        threshold_sites_proportion = -1),
    "Coverage threshold should be a numeric value >= 0 and <= 1",
    fixed = TRUE
  )
})

# Well-formed inputs -----------------------------------------------------------

test_that("fb_filter_species_by_site_coverage() successully works", {
  
  
  expect_silent(
    test_coverage <- fb_filter_species_by_site_coverage(site_species, 0)
  )
  
  expect_identical(nrow(test_coverage), nrow(site_species))

  
  # Output format
  expect_s3_class(test_coverage, "data.frame")
  expect_named(test_coverage, c("site", "sp1", "sp2"))
  expect_type(test_coverage$"site", "integer")
  expect_type(test_coverage$"sp1", "double")
  expect_type(test_coverage$"sp2", "double")
  expect_equal(test_coverage$"site"[1], 1)
  expect_equal(test_coverage$"sp2"[1], 1)
  
  # Test for one species with all NA
  expect_silent({
    test_coverage <- fb_filter_species_by_site_coverage(site_species2, 0.5)
  })
  
  expect_equal(ncol(test_coverage), 2)
  
  # Test for one site without any species
  expect_silent(
    test_coverage <- fb_filter_species_by_site_coverage(site_species3, 0.1)
  )
  
  expect_equal(ncol(test_coverage), 3)
  
  # Test for no species selected
  
  expect_message(
    test_coverage <- fb_filter_species_by_site_coverage(site_species, 1),
    "All species are absent from the study area",
    fixed = TRUE
  )
  
  expect_identical(test_coverage, site_species[NULL,])
})

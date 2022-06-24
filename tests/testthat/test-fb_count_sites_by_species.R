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

site_species4 <- data.frame(
  site = 1:4,
  sp1  = c(NA, 0, NA,  0),
  sp2  = c( 0, 0, NA, NA)
)

test_that("fb_count_sites_by_species() errors with wrong inputs", {
  
  # Wrong inputs ----
  
  expect_error(
    fb_count_sites_by_species(),
    "Argument 'site_species' (site x species data frame) is required",
    fixed = TRUE
  )
  
  expect_error(
    {
      st2 <- site_species
      colnames(st2) <- NULL
      fb_count_sites_by_species(st2)
    },
    "The site x species object must have column names (species names)",
    fixed = TRUE
  )
  
  expect_error(
    fb_count_sites_by_species(site_species[ , -1, drop = FALSE]),
    "The site x species object must contain the 'site' column",
    fixed = TRUE
  )
})


test_that("fb_count_sites_by_species() successfully works", {
  
  # Success ----
  
  expect_silent({
    test_coverage <- fb_count_sites_by_species(site_species)
  })
  
  
  # Output format ----
  
  expect_s3_class(test_coverage, "data.frame")
  expect_named(test_coverage, c("species", "n_sites", "coverage"))
  expect_type(test_coverage$"species", "character")
  expect_type(test_coverage$"n_sites", "integer")
  expect_type(test_coverage$"coverage", "double")
  expect_equal(test_coverage$"species"[1], "sp1")
  expect_equal(test_coverage$"n_sites"[1], 2)
  expect_equal(test_coverage$"coverage"[1], 0.5)
  
  # Test for a species with all NA ----
  
  expect_silent({
    test_coverage <- fb_count_sites_by_species(site_species2)
  })
  
  expect_equal(nrow(test_coverage), ncol(site_species2) - 1)
  expect_equal(ncol(test_coverage), 3)
  
  expect_equal(test_coverage$"species"[2], "sp1")
  expect_equal(test_coverage$"n_sites"[2], 0)
  expect_equal(test_coverage$"coverage"[2], 0)
  
  # Test for a site without any species (NA) ----
  
  expect_silent({
    test_coverage <- fb_count_sites_by_species(site_species3)
  })
  
  expect_equal(nrow(test_coverage), ncol(site_species2) - 1)
  expect_equal(ncol(test_coverage), 3)
  
  expect_equal(test_coverage$"species"[1], "sp2")
  expect_equal(test_coverage$"n_sites"[1], 3)
  expect_equal(test_coverage$"coverage"[1], 0.75)
  
  # Test for all sites without any species (0 and NA) ----
  
  expect_silent({
    test_coverage <- fb_count_sites_by_species(site_species4)
  })
  
  expect_equal(nrow(test_coverage), ncol(site_species4) - 1)
  expect_equal(ncol(test_coverage), 3)
  
  expect_equal(test_coverage$"species"[1], "sp1")
  expect_equal(sum(test_coverage$"n_sites"), 0)
  expect_equal(sum(test_coverage$"coverage"), 0)
})

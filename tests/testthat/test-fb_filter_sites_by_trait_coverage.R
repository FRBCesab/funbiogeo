# Preliminary data --------

site_species  <- data.frame(
  site = letters[1:5],
  sp1  = c(0, 0, 10, 0, 0),
  sp2  = c(1, 10, 0, 25, 40),
  sp3  = c(22, 8, 3, 0, 12),
  sp4  = c(3, 0, 2, 12, 0)
)
species_traits <- data.frame(
  species = paste0("sp", 1:4),
  t1      = c(1.1, 2.5, 100, 400)
)

# Data with non numeric traits
species_traits_2 <- data.frame(
  species = paste0("sp", 1:4),
  t1      = c(1.1, 2.5, 100, 400),
  t2      = c("a", "a", "b", "b"),
  t3      = factor(c("a", "a", "b", "b")),
  t4      = ordered(c("a", "a", "b", "b"))
)

# Data with missing data
species_traits_na <- data.frame(
  species = paste0("sp", 1:4),
  t1      = c(1.1, 2.5, NA, NA)
)

# Occurrence matrix
occ_dat <- data.frame(
  site = letters[1:5],
  sp1  = c(0, 0, 1, 0, 0),
  sp2  = c(1, 1, 0, 1, 1),
  sp3  = c(1, 1, 1, 0, 1),
  sp4  = c(1, 0, 1, 1, 0)
)

# Relative cover matrix
rel_dat <- site_species[,2:5]/rowSums(site_species[,2:5])
rel_dat[["site"]] <- site_species[["site"]]
rel_dat <- rel_dat[, c(5, 1:4)]


# Wrong inputs -----------------------------------------------------------------

test_that("fb_filter_sites_by_trait_coverage() errors with wrong input", {
  
  expect_error(
    fb_filter_sites_by_trait_coverage(species_traits = species_traits),
    "Argument 'site_species' (site x species matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_filter_sites_by_trait_coverage(site_species = site_species),
    "Argument 'species_traits' (species x traits matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    {
      sp2 <- site_species
      colnames(sp2) <- NULL
      fb_filter_sites_by_trait_coverage(sp2, species_traits)
    },
    "The site x species object must have column names (species names)",
    fixed = TRUE
  )
  
  expect_error(
    {
      st2 <- species_traits
      colnames(st2) <- NULL
      fb_filter_sites_by_trait_coverage(site_species, st2)
    },
    "The species x traits object must have column names (trait names)",
    fixed = TRUE
  )
  
  expect_error(
    fb_filter_sites_by_trait_coverage(site_species[,-1], species_traits),
    "The site x species object must contain the 'site' column",
    fixed = TRUE
  )
  
  expect_error(
    fb_filter_sites_by_trait_coverage(site_species, species_traits[,-1, drop = FALSE]),
    "The species x traits object must contain the 'species' column",
    fixed = TRUE
  )
  
  
  # No species in common
  expect_error(
    fb_filter_sites_by_trait_coverage(site_species[,1:3], species_traits[3:4,]),
    "No species found in common between inputs",
    fixed = TRUE
  )
  
  
  # No numeric threshold
  expect_error(
    fb_filter_sites_by_trait_coverage(
      site_species, species_traits, threshold_traits_proportion = "a"
    ),
    "Coverage threshold should be a numeric value >= 0 and <= 1",
    fixed = TRUE
  )
  
  
  # Threshold > 1
  
  expect_error(
    fb_filter_sites_by_trait_coverage(
      site_species, species_traits, threshold_traits_proportion = 2
    ),
    "Coverage threshold should be a numeric value >= 0 and <= 1",
    fixed = TRUE
  )
  
  
  # Threshold < 0
  
  expect_error(
    fb_filter_sites_by_trait_coverage(
      site_species, species_traits, threshold_traits_proportion = -1
    ),
    "Coverage threshold should be a numeric value >= 0 and <= 1",
    fixed = TRUE
  )
  
  # Trait with NAs
  
  expect_silent(
    {
      st2 <- species_traits
      st2[1, 2] <- NA
      filtered_site <- fb_filter_sites_by_trait_coverage(site_species, st2)
    }
  )
  
  expect_identical(filtered_site, site_species[-3,])

})

# Occurrence matrix ----------------------------------------------------------

test_that("fb_filter_sites_by_trait_coverage() works with occurrence matrices", {  
  
  ## Only numeric trait
  # Working
  expect_silent(test_coverage <- fb_filter_sites_by_trait_coverage(occ_dat, species_traits, 1))
  
  expect_identical(test_coverage, occ_dat)
  
  
  # No site is selected
  expect_message(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      occ_dat, species_traits[1, , drop = FALSE],
      threshold_traits_proportion = 1
    ),
    "No sites has the specified trait coverage threshold",
    fixed = TRUE
  )
  
  expect_identical(test_coverage, occ_dat[NULL,])
  
  
  # Lower threshold
  expect_silent(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      occ_dat, species_traits[1, , drop = FALSE],
      threshold_traits_proportion = 0.3
    )
  )
  
  expect_identical(test_coverage, occ_dat[3,,])
  
  
  ## Missing trait data
  expect_silent(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      occ_dat, species_traits_na, 1/2
    )
  )
  expect_identical(test_coverage, occ_dat[c(2, 4, 5),])
  
  
  ## Multiple trait types
  # Working
  expect_silent(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      occ_dat, species_traits_2, 1
    )
  )
  
  expect_identical(test_coverage, occ_dat)
  
  
  # No site is selected
  expect_message(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      occ_dat, species_traits_2[1, , drop = FALSE],
      threshold_traits_proportion = 1
    ),
    "No sites has the specified trait coverage threshold",
    fixed = TRUE
  )
  
  expect_identical(test_coverage, occ_dat[NULL,])
  
  # Lower threshold
  expect_silent(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      occ_dat, species_traits_2[1, , drop = FALSE],
      threshold_traits_proportion = 0.3
    )
  )
  
  expect_identical(test_coverage, occ_dat[3,,])

})


# Abundance matrix -----------------------------------------------------------

test_that("fb_filter_sites_by_trait_coverage() works with abundance matrices", {  
  
  ## Only numeric trait
  # (with only species for which we have species_traits)
  expect_silent(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      site_species, species_traits
    )
  )
  
  expect_identical(test_coverage, site_species)
  
  # No sites selected
  expect_message(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      site_species, species_traits[1,, drop = FALSE], 1
    ),
    "No sites has the specified trait coverage threshold",
    fixed = TRUE
  )
  
  expect_identical(test_coverage, site_species[NULL,])
  

  # Lower threshold
  expect_silent(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      site_species, species_traits[1,, drop = FALSE], 0.5
    )
  )
  
  expect_identical(test_coverage, site_species[3,])
  
  
  ## Missing trait data
  expect_silent(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      site_species, species_traits_na, 1/2
    )
  )
  expect_identical(test_coverage, site_species[2:5,])
  
  ## Multiple trait types
  # (with only species for which we have traits of species)
  expect_silent(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      site_species, species_traits_2
    )
  )
  
  expect_identical(test_coverage, site_species)
  
  # No sites selected
  expect_message(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      site_species, species_traits_2[1,, drop = FALSE], 1
    ),
    "No sites has the specified trait coverage threshold",
    fixed = TRUE
  )
  
  expect_identical(test_coverage, site_species[NULL,])

  
  # Lower threshold
  expect_silent(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      site_species, species_traits_2[1,, drop = FALSE], 0.5
    )
  )
  
  expect_identical(test_coverage, site_species[3,])
  
})


# Cover matrix ---------------------------------------------------------------

test_that("fb_filter_sites_by_trait_coverage() works with cover matrices", {
  ## Only numeric trait
  # (with only species for which we have species_traits)
  expect_silent(
    test_coverage <- fb_filter_sites_by_trait_coverage(rel_dat, species_traits)
  )
  
  expect_identical(test_coverage, rel_dat)
  
  
  # (with species for which we don't have all the species_traits)
  expect_message(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      rel_dat, species_traits[1,, drop = FALSE], 1
    ),
    "No sites has the specified trait coverage threshold",
    fixed = TRUE
  )
  
  expect_identical(test_coverage, rel_dat[NULL,])
  
  
  # Lower threshold should work
  
  expect_silent(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      rel_dat, species_traits[1,, drop = FALSE],
      threshold_traits_proportion = 0.6
    )
  )
  
  expect_identical(test_coverage, rel_dat[3,])
  
  ## Missing Trait Data
  expect_silent(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      rel_dat, species_traits_na, 1/2
    )
  )
  expect_identical(test_coverage, rel_dat[2:5,])
  
  ## Multiple trait types
  # (with only species for which we have traits)
  expect_silent(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      rel_dat, species_traits_2
    )
  )
  
  expect_identical(test_coverage, rel_dat)
  
  # (with species for which we don't have all the species_traits_2)
  expect_message(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      rel_dat, species_traits_2[1,, drop = FALSE], 1
    ),
    "No sites has the specified trait coverage threshold",
    fixed = TRUE
  )
  
  expect_identical(test_coverage, rel_dat[NULL,])
  
  
  # Lower threshold should work
  expect_silent(
    test_coverage <- fb_filter_sites_by_trait_coverage(
      rel_dat, species_traits_2[1,, drop = FALSE],
      threshold_traits_proportion = 0.6
    )
  )
  
  expect_identical(test_coverage, rel_dat[3,])
  
})

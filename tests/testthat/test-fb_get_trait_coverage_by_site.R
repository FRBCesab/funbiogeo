
# Preliminary data -------------------------------------------------------------
# Abundance matrix
site_species  <- data.frame(
  site = letters[1:5],
  sp1  = c(0, 0, 10, 0, 0),
  sp2  = c(1, 10, 0, 25, 40),
  sp3  = c(22, 8, 3, 0, 12),
  sp4  = c(3, 0, 2, 12, 0)
)


# Data with only numeric traits
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

test_that("fb_get_trait_coverage_by_site() errors with wrong inputs", {
  
  expect_error(
    fb_get_trait_coverage_by_site(species_traits = species_traits),
    "Argument 'site_species' (site x species data.frame) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_trait_coverage_by_site(site_species = site_species),
    "Argument 'species_traits' (species x traits data.frame) is required",
    fixed = TRUE
  )
  
  expect_error(
    {
      sp2 <- site_species
      colnames(sp2) <- NULL
      fb_get_trait_coverage_by_site(sp2, species_traits)
    },
    "The site x species object must have column names (species names)",
    fixed = TRUE
  )
  
  expect_error(
    {
      st2 <- species_traits
      colnames(st2) <- NULL
      fb_get_trait_coverage_by_site(site_species, st2)
    },
    "The species x traits object must have column names (trait names)",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_trait_coverage_by_site(site_species[,-1], species_traits),
    "The site x species object must contain the 'site' column",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_trait_coverage_by_site(
      site_species, species_traits[,-1, drop = FALSE]
    ),
    "The species x traits object must contain the 'species' column",
    fixed = TRUE
  )
  
  # No species in common
  
  expect_error(
    fb_get_trait_coverage_by_site(site_species[, 1:2], species_traits[3:4,]),
    "No species found in common between inputs",
    fixed = TRUE
  )
  
  
  # Trait with NAs
  
  expect_silent({
    st3 <- species_traits
    st3[1, 2] <- NA
    test_coverage <- fb_get_trait_coverage_by_site(
      site_species[1,, drop = FALSE], st3
    )
  })
  
  
  # Check output format
  
  expect_s3_class(test_coverage, "data.frame")
  expect_named(test_coverage, c("site", "trait_coverage"))
  expect_type(test_coverage$"site", "character")
  expect_type(test_coverage$"trait_coverage", "double")
  expect_equal(test_coverage$"site", "a")
  expect_equal(test_coverage$"trait_coverage", 1)
  
})


# Occurrence matrix ------------------------------------------------------------

test_that("fb_get_trait_coverage_by_site() works with occurrence matrices", {
  
  ## Only numeric traits
  # (with only species for which we have traits)
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(occ_dat, species_traits)
  )
  
  expect_equal(test_coverage[["trait_coverage"]][[1]], 1)
  
  # (with species for which we don't have all the traits)
  
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(
      occ_dat, species_traits[1:3,]
    )
  )
  
  expect_equal(test_coverage[["trait_coverage"]][[1]], 2/3)
  
  
  ## Missing trait
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(occ_dat, species_traits_na)
  )
  expect_equal(test_coverage[["trait_coverage"]], c(1/3, 1/2, 1/3, 1/2, 1/2))
  
  
  ## Multiple trait types
  # (with only species for which we have traits)
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(occ_dat, species_traits_2)
  )
  
  expect_equal(test_coverage[["trait_coverage"]][[1]], 1)
  
  # (with species for which we don't have all the traits)
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(
      occ_dat, species_traits_2[1:3,]
    )
  )
  
  expect_equal(test_coverage[["trait_coverage"]][[1]], 2/3)
  
})


# Abundance matrix -------------------------------------------------------------

test_that("fb_get_trait_coverage_by_site() works with abundance matrices", {
  
  ## Only numeric traits
  # (with only species for which we have traits)
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(site_species, species_traits)
  )
  
  expect_equal(test_coverage[["trait_coverage"]][[1]], 1)
  
  # (with species for which we don't have all the traits)
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(
      site_species, species_traits[1:3,]
    )
  )
  
  expect_equal(test_coverage[["trait_coverage"]][[4]], 25/37)

  
  ## Missing trait
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(
      site_species, species_traits_na
    )
  )
  expect_equal(
    test_coverage[["trait_coverage"]], c(1/26, 10/18, 10/15, 25/37, 40/52)
  )
  
  ## Multiple trait types
  # (with only species for which we have traits)
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(
      site_species, species_traits_2
    )
  )
  
  expect_equal(test_coverage[["trait_coverage"]][[1]], 1)
  
  
  # (with species for which we don't have all the traits)
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(
      site_species, species_traits_2[1:3,]
    )
  )
  
  expect_equal(test_coverage[["trait_coverage"]][[4]], 25/37)
  
})


# Coverage matrix --------------------------------------------------------------

test_that("fb_get_trait_coverage_by_site() works with coverage matrices", {
  
  ## Only numeric trait
  # (with only species for which we have traits)
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(rel_dat, species_traits)
  )
  
  expect_equal(test_coverage[["trait_coverage"]][[1]], 1)
  
  # (with species for which we don't have all the traits)
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(
      rel_dat, species_traits[1:3,]
      )
  )
  
  expect_equal(test_coverage[["trait_coverage"]][[4]], 25/ 37)
  
  
  ## Missing trait
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(rel_dat, species_traits_na)
  )
  expect_equal(
    test_coverage[["trait_coverage"]], c(1/26, 10/18, 10/15, 25/37, 40/52)
  )
  
  
  ## Multiple trait types
  # (with only species for which we have traits)
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(rel_dat, species_traits_2)
  )
  
  expect_equal(test_coverage[["trait_coverage"]][[1]], 1)
  
  
  # (with species for which we don't have all the traits)
  expect_silent(
    test_coverage <- fb_get_trait_coverage_by_site(
      rel_dat, species_traits_2[1:3,]
    )
  )

  expect_equal(test_coverage[["trait_coverage"]][[4]], 25/37)
})

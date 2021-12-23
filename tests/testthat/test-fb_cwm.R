# Preliminary data -------------------------------------------------------------

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


# Wrong Inputs -----------------------------------------------------------------

test_that("fb_cwm() fails with wrong inputs", {
  
  expect_error(
    fb_cwm(species_traits = species_traits),
    "Argument 'site_species' (site x species matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_cwm(site_species = site_species),
    "Argument 'species_traits' (species x traits matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_cwm(species_traits = species_traits),
    "Argument 'site_species' (site x species matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_cwm(site_species = site_species),
    "Argument 'species_traits' (species x traits matrix) is required",
    fixed = TRUE
  )
  
  expect_error(
    {
      sp2 <- site_species
      colnames(sp2) <- NULL
      fb_cwm(sp2, species_traits)
    },
    "The site x species object must have column names (species names)",
    fixed = TRUE
  )
  
  expect_error(
    {
      st2 <- species_traits
      colnames(st2) <- NULL
      fb_cwm(site_species, st2)
    },
    "The species x traits object must have column names (trait names)",
    fixed = TRUE
  )
  
  expect_error(
    fb_cwm(site_species[,-1], species_traits),
    "The site x species object must contain the 'site' column",
    fixed = TRUE
  )
  
  expect_error(
    fb_cwm(site_species, species_traits[,-1, drop = FALSE]),
    "The species x traits object must contain the 'species' column",
    fixed = TRUE
  )
  
  
  # No species in common
  
  expect_error(
    fb_cwm(site_species[,1:3], species_traits[3:4,]),
    "No species found in common between inputs",
    fixed = TRUE
  )
  
  
  # No numeric traits
  
  expect_error(
    fb_cwm(site_species, species_traits[, 1, drop = FALSE]),
    "CWM can only be computed on numeric traits",
    fixed = TRUE
  )

})


# Valid Inputs -----------------------------------------------------------------

test_that("fb_cwm() works with valid inputs", {
  
  expect_silent(test_cwm <- fb_cwm(site_species, species_traits))
  
  expect_s3_class(test_cwm, "data.frame")
  expect_named(test_cwm, c("site", "trait", "cwm"))
  expect_equal(dim(test_cwm), c(5, 3))
  expect_equal(test_cwm[["cwm"]][1], 130.86538, tolerance = 0.000001)
  
})

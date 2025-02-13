data("woodiv_site_species")
data("woodiv_traits")

site_species   <- woodiv_site_species
species_traits <- woodiv_traits

test_that("fb_get_all_trait_coverages_by_site() errors with wrong arguments", {
  
  # Missing Arguments
  
  expect_error(
    fb_get_all_trait_coverages_by_site(),
    "Argument 'site_species' (site x species data frame) is required",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_all_trait_coverages_by_site(site_species),
    "Argument 'species_traits' (species x traits data frame) is required",
    fixed = TRUE
  )
  
  
  # Wrong arguments
  
  expect_error(
    fb_get_all_trait_coverages_by_site(data.frame()),
    "The site x species object should have at least one row and one column",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_all_trait_coverages_by_site(site_species, data.frame()),
    "The species x traits object should have at least one row and one column",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_all_trait_coverages_by_site(list()),
    "The site x species object must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_all_trait_coverages_by_site(site_species, list()),
    "The species x traits object must be a data.frame",
    fixed = TRUE
  )
  
  expect_error(
    fb_get_all_trait_coverages_by_site(site_species, species_traits, 1),
    "Argument 'all_traits' should be TRUE or FALSE",
    fixed = TRUE
  )
  
})


test_that(
  "fb_get_all_trait_coverages_by_site() works well with good arguments",{
    
    # With all traits
    expect_silent(
      res <- fb_get_all_trait_coverages_by_site(site_species, species_traits)
    )
    
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(4723, 6))
    expect_equal(
      colnames(res), c("site", "all_traits", colnames(species_traits)[-1])
    )
    expect_equal(round(res[1, 2], 2), 1.0)
    expect_equal(round(res[2, 5], 2), 1.0)
    
    # Without all traits
    expect_silent(
      res <- fb_get_all_trait_coverages_by_site(
        site_species, species_traits, FALSE
      )
    )
    
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(0, 5))
    expect_equal(
      colnames(res), c("site", colnames(species_traits)[-1])
    )
    expect_equal(round(res[2, 4], 2), NA_real_)
    
  }
)


test_that("fb_get_all_trait_coverages_by_site() works for edge cases", {
  
  ## With all traits
  # Single species (from site-species)
  expect_silent(
    res <- fb_get_all_trait_coverages_by_site(
      site_species[, 1:2], species_traits, TRUE
    )
  )
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(4723, 6))
  expect_equal(
    colnames(res), c("site", "all_traits", colnames(species_traits)[-1])
  )
  
  # Single species (from trait)
  expect_silent(
    res <- fb_get_all_trait_coverages_by_site(
      site_species, species_traits[1,, drop = FALSE], TRUE
    )
  )
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(4723, 6))
  expect_equal(
    colnames(res), c("site", "all_traits", colnames(species_traits)[-1])
  )
  
  # Both
  expect_silent(
    res <- fb_get_all_trait_coverages_by_site(
      site_species[, 1:2], species_traits[1,, drop = FALSE], TRUE
    )
  )
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(4723, 6))
  expect_equal(
    colnames(res), c("site", "all_traits", colnames(species_traits)[-1])
  )
  
  
  ## Without all traits
  # Single species (from site-species)
  expect_silent(
    res <- fb_get_all_trait_coverages_by_site(
      site_species[, 1:2], species_traits, FALSE
    )
  )
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(0, 5))
  expect_equal(
    colnames(res), c("site", colnames(species_traits)[-1])
  )
  
  # Single species (from trait)
  expect_silent(
    res <- fb_get_all_trait_coverages_by_site(
      site_species, species_traits[1,, drop = FALSE], FALSE
    )
  )
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(0, 5))
  expect_equal(
    colnames(res), c("site", colnames(species_traits)[-1])
  )
  
  # Both
  expect_silent(
    res <- fb_get_all_trait_coverages_by_site(
      site_species[, 1:2], species_traits[1,, drop = FALSE], FALSE
    )
  )
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(0, 5))
  expect_equal(
    colnames(res), c("site", colnames(species_traits)[-1])
  )
  
})
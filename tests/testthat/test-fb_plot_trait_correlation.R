data("woodiv_traits")
species_traits <- woodiv_traits

# Initial data -----------------------------------------------------------------

sp_trait <- data.frame(
  species = letters[1:3],
  trait1  = letters[1:3],
  trait2  = 1:3,
  trait3  = 3:1,
  trait4  = factor(letters[1:3])
)


# Actual Tests -----------------------------------------------------------------

test_that("fb_plot_trait_correlation() works", {
  
  # Wrong input, only non-numerical traits
  expect_error(
    fb_plot_trait_correlation(sp_trait[, 1:2]),
    "No numerical traits found, cannot plot trait correlations",
    fixed = TRUE
  )
  
  # Wrong input, only non-numerical traits
  expect_error(
    fb_plot_trait_correlation(sp_trait[, c(1:2, 5)]),
    "No numerical traits found, cannot plot trait correlations",
    fixed = TRUE
  )
  
  # Good input
  expect_silent(
    given_plot <- fb_plot_trait_correlation(species_traits)
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  vdiffr::expect_doppelganger(
    "fb_plot_trait_correlation-default", 
    given_plot
  )
  

  expect_message(
    given_plot <- fb_plot_trait_correlation(sp_trait),
    paste0(
      "Non-numerical traits found, only keeping numerical traits ",
      "to display trait correlations"
    ),
    fixed = TRUE
  )
  
  expect_s3_class(given_plot, "ggplot")

  vdiffr::expect_doppelganger(
    "fb_plot_trait_correlation-withmsg", 
    given_plot
  )

  
  ## Works with species categories
  # Single category 
  expect_silent(
    given_plot <- fb_plot_trait_correlation(
      sp_trait[, -c(2, 5)],
      data.frame(species = sp_trait$species, category = "A")
    )
  )

  expect_s3_class(given_plot, "ggplot")

  vdiffr::expect_doppelganger(
    "fb_plot_trait_correlation-onecat", 
    given_plot
  )
})

test_that("fb_plot_trait_correlation() skipped", {

  skip()

  # Less categories than species
  expect_silent(
    given_plot <- fb_plot_trait_correlation(
      sp_trait[, -c(2, 5)],
      data.frame(species  = sp_trait$species, category = c(1, 1, 2))
    )
  )

  expect_s3_class(given_plot, "ggplot")

  vdiffr::expect_doppelganger(
    "fb_plot_trait_correlation-fewcat", 
    given_plot
  )
  
  # As many categories as species
  expect_silent(
    given_plot <- fb_plot_trait_correlation(
      sp_trait[, -c(2, 5)],
      data.frame(species  = sp_trait$species,
                 category = sp_trait$species)
    )
  )

  expect_s3_class(given_plot, "ggplot")
  
  vdiffr::expect_doppelganger(
    "fb_plot_trait_correlation-allcat", 
    given_plot
  )
})

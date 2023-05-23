# Initial data -----------------------------------------------------------------

sp_trait <- data.frame(species = letters[1:3], trait1 = letters[1:3],
                       trait2  = 1:3, trait3 = 3:1)


# Actual Tests -----------------------------------------------------------------

test_that("fb_plot_trait_correlation() works", {
  
  # Wrong input
  expect_error(
    fb_plot_trait_correlation(sp_trait[, 1:2]),
    "No numerical traits found, cannot plot trait correlations",
    fixed = TRUE
  )
  
  # Good input
  expect_silent(res <- fb_plot_trait_correlation(species_traits))
  
  expect_s3_class(res, "ggplot")
  
  
  expect_message(
    res <- fb_plot_trait_correlation(sp_trait),
    paste0(
      "Non-numerical traits found, only keeping numerical traits ",
      "to show trait correlation"
    ),
    fixed = TRUE
  )
  
  expect_s3_class(res, "ggplot")
  
  ## Works with species categories
  # Single category 
  expect_silent(
    given_plot <- fb_plot_trait_correlation(
      sp_trait[, -2], data.frame(species  = sp_trait$species, category = "A")
    )
  )
  
  # Less categories than species
  expect_silent(
    given_plot <- fb_plot_trait_correlation(
      sp_trait[, -2],
      data.frame(species  = sp_trait$species, category = c(1, 1, 2))
    )
  )
  
  # As many categories as species
  expect_silent(
    given_plot <- fb_plot_trait_correlation(
      sp_trait[, -2],
      data.frame(species  = sp_trait$species,
                 category = sp_trait$species)
    )
  )
})

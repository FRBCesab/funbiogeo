data("woodiv_traits")
species_traits <- woodiv_traits

test_that("fb_plot_trait_combination_frequencies() works", {

  expect_silent(
    given_plot <- fb_plot_trait_combination_frequencies(species_traits)
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  vdiffr::expect_doppelganger(
    "fb_plot_tr_comb_freq-default", 
    given_plot
  )
  
  expect_silent(
    given_plot <- fb_plot_trait_combination_frequencies(
      species_traits, NULL, "complete"
    )
  )

  expect_s3_class(given_plot, "ggplot")
  
  vdiffr::expect_doppelganger(
    "fb_plot_tr_comb_freq-complete", 
    given_plot
  )
  
  ## Works with species categories

  # Single category 
  expect_silent(
    given_plot <- fb_plot_trait_combination_frequencies(
      species_traits,
      data.frame(species  = species_traits$species,
                 category = "A")
    )
  )

  expect_s3_class(given_plot, "ggplot")
  
  vdiffr::expect_doppelganger(
    "fb_plot_tr_comb_freq-onecat", 
    given_plot
  )
  
  # Less categories than species
  expect_silent(
    given_plot <- fb_plot_trait_combination_frequencies(
      species_traits,
      data.frame(
        species  = species_traits$species,
        category = sample(letters[1:3], nrow(species_traits), replace = TRUE)
      )
    )
  )

  expect_s3_class(given_plot, "ggplot")
  
  # vdiffr::expect_doppelganger(
  #   "fb_plot_trait_combination_frequencies-fewcat", 
  #   given_plot
  # )
  
  # As many categories as species
  expect_silent(
    given_plot <- fb_plot_trait_combination_frequencies(
      species_traits,
      data.frame(species  = species_traits$species,
                 category = species_traits$species)
    )
  )

  expect_s3_class(given_plot, "ggplot")
  
  # vdiffr::expect_doppelganger(
  #   "fb_plot_trait_combination_frequencies-allcat", 
  #   given_plot
  # )
})

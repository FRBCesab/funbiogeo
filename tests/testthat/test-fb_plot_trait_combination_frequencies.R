data("woodiv_traits")
species_traits <- woodiv_traits

test_that("fb_plot_trait_combination_frequencies() works", {
  expect_silent(
    res <- fb_plot_trait_combination_frequencies(species_traits)
  )
  
  expect_s3_class(res, "ggplot")
  
  expect_silent(
    res <- fb_plot_trait_combination_frequencies(
      species_traits, NULL, "complete"
    )
  )
  
  expect_s3_class(res, "ggplot")
  
  
  ## Works with species categories
  # Single category 
  expect_silent(
    given_plot <- fb_plot_trait_combination_frequencies(
      species_traits,
      data.frame(species  = species_traits$species,
                 category = "A")
    )
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
  
  # As many categories as species
  expect_silent(
    given_plot <- fb_plot_trait_combination_frequencies(
      species_traits,
      data.frame(species  = species_traits$species,
                 category = species_traits$species)
    )
  )
})

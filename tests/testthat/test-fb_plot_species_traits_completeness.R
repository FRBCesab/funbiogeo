test_that("fb_plot_species_traits_completeness works", {
  expect_silent(
    given_plot <- fb_plot_species_traits_completeness(species_traits)
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  # Without 'all_traits' added
  expect_silent(
    given_plot <- fb_plot_species_traits_completeness(
      species_traits, all_traits =  FALSE
    )
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  # Test that graph works with non-continuous traits
  example_traits <- data.frame(
    species = letters[1:3],
    trait1  = 1:3,
    trait2  = LETTERS[1:3]
  )
  
  expect_silent(
    given_plot <- fb_plot_species_traits_completeness(
      example_traits, all_traits = FALSE
    )
  )
  
  # Test that function works with a single trait
  
  expect_silent(
    given_plot <- fb_plot_species_traits_completeness(
      example_traits[, 1:2], all_traits = FALSE
    )
  )
  
  
  ## Works with species categories
  # Single category 
  expect_silent(
    given_plot <- fb_plot_species_traits_completeness(
      example_traits,
      data.frame(species  = example_traits$species,
                 category = "A"),
      all_traits = FALSE
    )
  )
  
  # Less categories than species
  expect_silent(
    given_plot <- fb_plot_species_traits_completeness(
      example_traits,
      data.frame(species  = example_traits$species,
                 category = c(1, 1, 2)),
      all_traits = FALSE
    )
  )
  
  # As many categories as species
  expect_silent(
    given_plot <- fb_plot_species_traits_completeness(
      example_traits,
      data.frame(species  = example_traits$species,
                 category = example_traits$species),
      all_traits = FALSE
    )
  )
  
})

test_that("fb_plot_species_traits_completeness() fails gracefully", {
  
  expect_error(
    fb_plot_species_traits_completeness(species_traits, FALSE),
    "'species_categories' isn't a two-column data.frame"
  )
  
})
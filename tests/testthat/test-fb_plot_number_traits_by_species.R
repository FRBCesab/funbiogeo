test_that("fb_plot_number_traits_by_species works", {
  expect_silent(given_plot <- fb_plot_number_traits_by_species(species_traits))
  
  expect_s3_class(given_plot, "ggplot")
  
  
  expect_silent(
    given_plot <- fb_plot_number_traits_by_species(
      species_traits, threshold_species_proportion = 1/3
    )
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  # Check with non-continuous traits
  example_traits <- data.frame(
    species = letters[1:3],
    trait1  = 1:3,
    trait2  = LETTERS[1:3]
  )
  
  expect_silent(
    given_plot <- fb_plot_number_traits_by_species(example_traits)
  )
  
  # Test that function works with a single trait
  
  expect_silent(
    given_plot <- fb_plot_number_traits_by_species(example_traits[, 1:2])
  )
  
  
  ## Works with species categories
  # Single category 
  expect_silent(
    given_plot <- fb_plot_number_traits_by_species(
      example_traits,
      data.frame(species  = example_traits$species,
                 category = "A")
    )
  )
  
  # Less categories than species
  expect_silent(
    given_plot <- fb_plot_number_traits_by_species(
      example_traits,
      data.frame(species  = example_traits$species,
                 category = c(1, 1, 2))
    )
  )
  
  # As many categories as species
  expect_silent(
    given_plot <- fb_plot_number_traits_by_species(
      example_traits,
      data.frame(species  = example_traits$species,
                 category = example_traits$species)
    )
  )
  
})

test_that("fb_plot_number_traits_by_species() fails gracefully", {
  
  expect_error(
    fb_plot_number_traits_by_species(species_traits, FALSE),
    "'species_categories' isn't a two-column data.frame"
  )
  
})
test_that("fb_plot_species_traits_completeness works", {
  expect_silent(
    given_plot <- fb_plot_species_traits_completeness(species_traits)
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  # Without 'all_traits' added
  expect_silent(
    given_plot <- fb_plot_species_traits_completeness(species_traits, FALSE)
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  # Test that graph works with non-continuous traits
  example_traits <- data.frame(
    species = letters[1:3],
    trait1  = 1:3,
    trait2  = LETTERS[1:3]
  )
  
  expect_silent(
    given_plot <- fb_plot_species_traits_completeness(example_traits, FALSE)
  )
  
  # Test that function works with single trait
  
  expect_silent(
    given_plot <- fb_plot_species_traits_completeness(
      example_traits[, 1:2], FALSE
    )
  )
  
})
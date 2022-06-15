test_that("fb_plot_number_traits_by_species works", {
  expect_silent(given_plot <- fb_plot_number_traits_by_species(species_traits))
  
  expect_s3_class(given_plot, "ggplot")
  
  
  expect_silent(
    given_plot <- fb_plot_number_traits_by_species(species_traits, 3)
  )
  
  expect_s3_class(given_plot, "ggplot")
})
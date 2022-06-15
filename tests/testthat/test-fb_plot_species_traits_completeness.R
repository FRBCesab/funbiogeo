test_that("fb_plot_species_traits_completeness works", {
  expect_silent(
    given_plot <- fb_plot_species_traits_completeness(species_traits)
  )
  
  expect_s3_class(given_plot, "ggplot")
})
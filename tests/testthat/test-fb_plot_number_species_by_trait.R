test_that("fb_plot_number_species_by_trait works", {
  expect_silent(given_plot <- fb_plot_number_species_by_trait(species_traits))
  
  expect_s3_class(given_plot, "ggplot")
})

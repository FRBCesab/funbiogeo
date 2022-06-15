test_that("fb_plot_site_traits_completeness works", {
  expect_silent(
    given_plot <- fb_plot_site_traits_completeness(site_species, species_traits)
  )
  
  expect_s3_class(given_plot, "ggplot2")
})

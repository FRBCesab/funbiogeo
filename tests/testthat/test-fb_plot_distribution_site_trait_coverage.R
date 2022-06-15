test_that("fb_plot_distribution_site_trait_coverage() works", {
  expect_silent(
    suppressMessages(
      res <- fb_plot_distribution_site_trait_coverage(
        site_species, species_traits
      )
    )
  )
  
  expect_s3_class(res, "ggplot")
})

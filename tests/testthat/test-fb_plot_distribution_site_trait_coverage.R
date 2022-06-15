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

test_that("ggridges installation is checked", {
  
  skip_if_not_installed("mockery")
  
  # Mock function
  mockery::stub(
    fb_plot_distribution_site_trait_coverage, "is_ggridges_installed", FALSE
  )
  
  expect_error(
    fb_plot_distribution_site_trait_coverage(
      site_species, species_traits
    ),
    paste0("This function requires 'ggridges' to work\n",
           "Please run \"install.packages('ggridges')\""),
    fixed = TRUE
  )
})

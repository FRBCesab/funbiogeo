test_that("fb_plot_trait_combination_frequencies() works", {
  expect_silent(
    res <- fb_plot_trait_combination_frequencies(species_traits)
  )
  
  expect_s3_class(res, "ggplot")
  
  expect_silent(
    res <- fb_plot_trait_combination_frequencies(species_traits, "complete")
  )
  
  expect_s3_class(res, "ggplot")
})

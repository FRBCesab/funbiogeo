# Initial data -----------------------------------------------------------------

sp_trait <- data.frame(species = letters[1:3], trait1 = letters[1:3],
                       trait2  = 1:3)

# Actual Tests -----------------------------------------------------------------

test_that("fb_plot_trait_correlation() works", {
  
  # Wrong input
  expect_error(
    fb_plot_trait_correlation(sp_trait[, 1:2]),
    "No numerical traits found, cannot plot trait correlations",
    fixed = TRUE
  )
  
  # Good input
  expect_silent(res <- fb_plot_trait_correlation(species_traits))
  
  expect_s3_class(res, "ggplot")
  
  
  expect_message(
    res <- fb_plot_trait_correlation(sp_trait),
    paste0(
      "Non-numerical traits found, only keeping numerical traits ",
      "to show trait correlation"
    ),
    fixed = TRUE
  )
  
  expect_s3_class(res, "ggplot")
})

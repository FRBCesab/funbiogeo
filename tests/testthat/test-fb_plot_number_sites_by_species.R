test_that("fb_plot_number_sites_by_species works", {
  
  # Less than 30 species
  expect_silent(
    given_plot <- fb_plot_number_sites_by_species(woodiv_site_species[, 1:5]))
  
  expect_s3_class(given_plot, "ggplot")
  
  expect_silent(
    given_plot <- fb_plot_number_sites_by_species(woodiv_site_species[, 1:5], 0.5)
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  # More than 30 species
  # expect_message(
  #   given_plot <- fb_plot_number_sites_by_species(site_species[, 1:32]),
  #   paste0(
  #     "There are more than 30 species, the y-axis will label the position ",
  #     "of 30 evenly spaced species (along their prevalence)"
  #   ), fixed = TRUE 
  # )
  
  # expect_s3_class(given_plot, "ggplot")
})

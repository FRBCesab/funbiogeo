test_that("fb_plot_number_sites_by_species works", {
  
  # More than 30 species
  # Create a dataset with more than 30 species
  larger_site_species <- cbind(woodiv_site_species, woodiv_site_species[, -1])
  colnames(larger_site_species)[-1] <- paste0(
    "sp_", seq(ncol(larger_site_species) - 1)
  )
  
  expect_message(
    given_plot <- fb_plot_number_sites_by_species(larger_site_species),
    paste0("There are more than 30 species, the y-axis will label the position",
           " of 30 evenly spaced species (along their prevalence)"),
    fixed = TRUE
  )
  
  # Less than 30 species
  expect_silent(
    given_plot <- fb_plot_number_sites_by_species(woodiv_site_species[, 1:5])
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  vdiffr::expect_doppelganger(
    "fb_plot_nb_si_by_sp-default", 
    given_plot
  )
  
  expect_silent(
    given_plot <- fb_plot_number_sites_by_species(
      woodiv_site_species[, 1:5], 0.5
    )
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  vdiffr::expect_doppelganger(
    "fb_plot_nb_si_by_sp-threshold", 
    given_plot
  )
  
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

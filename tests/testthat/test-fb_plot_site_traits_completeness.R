test_that("fb_plot_site_traits_completeness works", {
  expect_silent(
    given_plot <- fb_plot_site_traits_completeness(site_species, species_traits)
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  expect_silent(
    given_plot <- fb_plot_site_traits_completeness(
      site_species, species_traits, NULL, FALSE
    )
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  ## Works with species categories
  # Single category 
  expect_silent(
    given_plot <- fb_plot_site_traits_completeness(
      site_species, species_traits,
      data.frame(species  = species_traits$species,
                 category = "A")
    )
  )
  
  # Less categories than species
  expect_silent(
    given_plot <- fb_plot_site_traits_completeness(
      site_species, species_traits,
      data.frame(
        species  = species_traits$species,
        category = sample(letters[1:3], nrow(species_traits), replace = TRUE)
      )
    )
  )
})

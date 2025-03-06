data("woodiv_site_species")
data("woodiv_traits")
site_species   <- woodiv_site_species
species_traits <- woodiv_traits

test_that("fb_plot_site_traits_completeness works", {

  expect_silent(
    given_plot <- fb_plot_site_traits_completeness(site_species, species_traits)
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  vdiffr::expect_doppelganger(
    "fb_plot_si_tr_comp-default", 
    given_plot
  )

  
  ## Works with species categories

  # Single category 

  expect_silent(
    given_plot <- fb_plot_site_traits_completeness(
      site_species, species_traits,
      data.frame(species  = species_traits$species,
                 category = "A")
    )
  )

  expect_s3_class(given_plot, "ggplot")
  
  # vdiffr::expect_doppelganger(
  #   "fb_plot_si_tr_comp-onecat", 
  #   given_plot
  # )
  
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

  expect_s3_class(given_plot, "ggplot")
  
  # vdiffr::expect_doppelganger(
  #   "fb_plot_si_tr_comp-fewcat", 
  #   given_plot
  # )
  
  # As many categories as species
  expect_silent(
    given_plot <- fb_plot_site_traits_completeness(
      site_species, species_traits,
      data.frame(species  = species_traits$species,
                 category = species_traits$species)
    )
  )

  expect_s3_class(given_plot, "ggplot")
  
  # vdiffr::expect_doppelganger(
  #   "fb_plot_si_tr_comp-allcat", 
  #   given_plot
  # )
})

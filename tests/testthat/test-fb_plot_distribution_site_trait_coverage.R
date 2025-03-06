data("woodiv_site_species")
data("woodiv_traits")
site_species   <- woodiv_site_species
species_traits <- woodiv_traits

test_that("fb_plot_distribution_site_trait_coverage() works", {

  expect_silent(
    suppressMessages(
      given_plot <- fb_plot_distribution_site_trait_coverage(
        site_species, species_traits
      )
    )
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  # Skip vdiffr test on Linux oldrel test
  skip_if(
    R.version$system == "x86_64, linux-gnu" &
      R.version$version.string == "R version 4.3.3 (2024-02-29)"
  )
  
  vdiffr::expect_doppelganger(
    "fb_plot_dist_site_trait_cov-default", 
    given_plot
  )
  
  # Removing all_traits
  # expect_silent(
  #   suppressMessages(
  #     res <- fb_plot_distribution_site_trait_coverage(
  #       site_species, species_traits, FALSE
  #     )
  #   )
  # )
  
  # expect_s3_class(res, "ggplot")
  
  
  ## Works with species categories

  # Single category 
  expect_silent(
    suppressMessages(
      given_plot <- fb_plot_distribution_site_trait_coverage(
        site_species, species_traits,
        data.frame(species  = species_traits$species, category = "A")
      )
    )
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  # vdiffr::expect_doppelganger(
  #   "fb_plot_distribution_site_trait_coverage-onecat", 
  #   given_plot
  # )
  
  
  # Less categories than species
  expect_silent(
    suppressMessages(
      given_plot <- fb_plot_distribution_site_trait_coverage(
        site_species, species_traits,
        data.frame(
          species  = species_traits$species,
          category = sample(letters[1:3], nrow(species_traits), replace = TRUE)
        )
      )
    )
  )
  
  expect_s3_class(given_plot, "ggplot")
  
  # vdiffr::expect_doppelganger(
  #   "fb_plot_distribution_site_trait_coverage-fewcat", 
  #   given_plot
  # )
  
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

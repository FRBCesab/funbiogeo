data("woodiv_traits")
species_traits <- woodiv_traits

test_that("fb_plot_species_traits_completeness works", {
  expect_silent(
    {
      given_plot <- fb_plot_species_traits_completeness(species_traits)
    }
  )

  expect_s3_class(given_plot, "ggplot")

  vdiffr::expect_doppelganger(
    "fb_plot_sp_tr_comp-default",
    given_plot
  )

  # Without 'all_traits' added

  expect_silent(
    {
      given_plot <- fb_plot_species_traits_completeness(
        species_traits,
        all_traits = FALSE
      )
    }
  )

  expect_s3_class(given_plot, "ggplot")

  vdiffr::expect_doppelganger(
    "fb_plot_sp_tr_comp-noall",
    given_plot
  )

  # Test that graph works with non-continuous traits

  example_traits <- data.frame(
    species = letters[1:3],
    trait1 = 1:3,
    trait2 = LETTERS[1:3]
  )

  expect_silent(
    {
      given_plot <- fb_plot_species_traits_completeness(
        example_traits,
        all_traits = FALSE
      )
    }
  )

  expect_s3_class(given_plot, "ggplot")

  vdiffr::expect_doppelganger(
    "fb_plot_sp_tr_comp-nquant",
    given_plot
  )

  # Test that function works with a single trait

  expect_silent(
    {
      given_plot <- fb_plot_species_traits_completeness(
        example_traits[, 1:2],
        all_traits = FALSE
      )
    }
  )

  expect_s3_class(given_plot, "ggplot")

  vdiffr::expect_doppelganger(
    "fb_plot_sp_tr_comp-one",
    given_plot
  )

  ## Works with species categories

  # Single category

  expect_silent(
    {
      given_plot <- fb_plot_species_traits_completeness(
        example_traits,
        data.frame(species = example_traits$species, category = "A"),
        all_traits = FALSE
      )
    }
  )

  expect_s3_class(given_plot, "ggplot")

  vdiffr::expect_doppelganger(
    "fb_plot_sp_tr_comp-onecat",
    given_plot
  )

  # Less categories than species

  expect_silent(
    {
      given_plot <- fb_plot_species_traits_completeness(
        example_traits,
        data.frame(species = example_traits$species, category = c(1, 1, 2)),
        all_traits = FALSE
      )
    }
  )

  expect_s3_class(given_plot, "ggplot")

  vdiffr::expect_doppelganger(
    "fb_plot_sp_tr_comp-fewcat",
    given_plot
  )

  # As many categories as species

  expect_silent(
    {
      given_plot <- fb_plot_species_traits_completeness(
        example_traits,
        data.frame(
          species = example_traits$species,
          category = example_traits$species
        ),
        all_traits = FALSE
      )
    }
  )

  expect_s3_class(given_plot, "ggplot")

  vdiffr::expect_doppelganger(
    "fb_plot_sp_tr_comp-allcat",
    given_plot
  )
})

test_that("fb_plot_species_traits_completeness() fails gracefully", {
  expect_error(
    fb_plot_species_traits_completeness(species_traits, FALSE),
    "The species x categories object must be a data.frame"
  )
})

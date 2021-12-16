# Tests for check site x species  ----------------------------------------------
test_that("check_site_species() works", {
  
  # Wrong input type ----
  
  expect_error(
    check_site_species("a"),
    "The site x species object must be a data.frame",
    fixed = TRUE)
  
  expect_error(
    check_site_species(1:10),
    "The site x species object must be a data.frame",
    fixed = TRUE)
  
  expect_error(
    check_site_species(list()),
    "The site x species object must be a data.frame",
    fixed = TRUE)
  
  
  # Matrix has no rows and/or columns ----
  
  expect_error(
    check_site_species(data.frame(NULL)),
    "The site x species object should have at least one row and one column",
    fixed = TRUE)
  
  # Check for sites and species names ----
  
  mat <- matrix(1:10, ncol = 2)
  dat <- as.data.frame(mat)
  
  expect_error(
    check_site_species(dat),
    "The site x species object must contain the 'site' column",
    fixed = TRUE
  )
  
  dat[["site"]] <- "a"
  colnames(dat) <- NULL
  
  expect_error(
    check_site_species(dat),
    "The site x species object must have column names (species names)",
    fixed = TRUE
  )
  
  
  # Matrix should not contain negative values ----
  
  mat <- matrix(c(1:9, -1), ncol = 2)
  rownames(mat) <- paste0("site_", seq_len(nrow(mat)))
  colnames(mat) <- paste0("species_", LETTERS[seq_len(ncol(mat))])
  
  dat <- as.data.frame(mat)
  dat[["site"]] <- rownames(mat)
  
  expect_error(
    check_site_species(dat),
    "The site x species object cannot contain negative values",
    fixed = TRUE)
  
  expect_error(
    check_site_species(dat),
    "The site x species object cannot contain negative values",
    fixed = TRUE)
  
  
  # Correct input ----
  
  mat <- matrix(1:10, ncol = 2)
  rownames(mat) <- paste0("site_", seq_len(nrow(mat)))
  colnames(mat) <- paste0("species_", LETTERS[seq_len(ncol(mat))])
  dat <- as.data.frame(mat)
  dat[["site"]] <- rownames(mat)
  
  expect_silent(check_site_species(dat))
  
  expect_equal(check_site_species(dat), NULL)
  
  # With missing data
  mat[1, 1] <- NA
  dat <- as.data.frame(mat)
  dat[["site"]] <- rownames(mat)
  
  expect_silent(check_site_species(dat))
  
  mat <- matrix(c(1, rep(NA, 9)), ncol = 2)
  rownames(mat) <- paste0("site_", seq_len(nrow(mat)))
  colnames(mat) <- paste0("species_", LETTERS[seq_len(ncol(mat))])
  dat <- as.data.frame(mat)
  dat[["site"]] <- rownames(mat)
  
  expect_silent(check_site_species(dat))
})


# Tests for check species x traits ---------------------------------------------

test_that("check_species_traits() works", {
  
  
  # Wrong input type ----
  
  expect_error(
    check_species_traits("a"),
    "The species x traits object must be a data.frame",
    fixed = TRUE)
  
  expect_error(
    check_species_traits(1:10),
    "The species x traits object must be a data.frame",
    fixed = TRUE)
  
  expect_error(
    check_species_traits(list()),
    "The species x traits object must be a data.frame",
    fixed = TRUE)
  
  
  # Matrix or data.frame has no rows and/or columns ----

  expect_error(
    check_species_traits(data.frame()),
    paste0("The species x traits object should have at least one row and ",
           "one column"),
    fixed = TRUE)
  
  
  # Check for sites and species names ----
  
  mat <- matrix(1:10, ncol = 2)
  dat <- as.data.frame(mat)
  
  expect_error(
    check_species_traits(dat),
    "The species x traits object must contain the 'species' column",
    fixed = TRUE
  )
  
  mat <- matrix(1:10, ncol = 2)
  rownames(mat) <- paste0("species_", seq_len(nrow(mat)))
  dat <- as.data.frame(mat)
  
  colnames(dat) <- NULL
  
  expect_error(
    check_species_traits(dat),
    "The species x traits object must have column names (trait names)",
    fixed = TRUE)
  
  
  # Correct Input ----

  mat <- matrix(1:10, ncol = 2)
  rownames(mat) <- paste0("species_", seq_len(nrow(mat)))
  colnames(mat) <- paste0("trait_", LETTERS[seq_len(ncol(mat))])
  
  dat <- as.data.frame(mat)
  dat[["species"]] <- rownames(mat)
  
  expect_silent(check_species_traits(dat))
  
  expect_equal(check_species_traits(dat), NULL)
  
  dat$"trait_3" <- LETTERS[seq_len(nrow(dat))]
  
  expect_silent(check_species_traits(dat))
  
  dat[1, 3] <- NA
  
  expect_silent(check_species_traits(dat))
})


# Tests for check site x locations ---------------------------------------------

test_that("check_site_locations() works", {
  
  data("site_locations")
  
  sites_sf <- site_locations
  sites_sf[["site"]] <- rownames(sites_sf)
  rownames(sites_sf) <- NULL
  sites_sf <- sf::st_as_sf(sites_sf, coords = 1:2)
  
  # Wrong input ----
  
  expect_error(
    check_site_locations(site_locations),
    "The site x locations object must be an 'sf' object",
    fixed = TRUE
  )
  
  expect_error(
    check_site_locations(as.list(site_locations)),
    "The site x locations object must be an 'sf' object",
    fixed = TRUE
  )
  
  expect_error(
    check_site_locations(site_locations[, 1, drop = FALSE]),
    paste0("The site x locations object must be an 'sf' object"),
    fixed = TRUE
  )
  
  expect_error(
    check_site_locations(sites_sf[-c(seq_len(nrow(sites_sf))),]),
    "The site x locations object should have at least one row",
    fixed = TRUE
  )
  
  expect_silent(check_site_locations(sites_sf))
  
  expect_equal(check_site_locations(sites_sf), NULL)
})

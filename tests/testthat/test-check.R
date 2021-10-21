# Tests for check site x species  ----------------------------------------------
test_that("check_sites_species() works", {
  
  # Wrong input type ----
  
  expect_error(
    check_sites_species("a"),
    "The sites x species object must be a matrix or a data.frame",
    fixed = TRUE)
  
  expect_error(
    check_sites_species(1:10),
    "The sites x species object must be a matrix or a data.frame",
    fixed = TRUE)
  
  expect_error(
    check_sites_species(list()),
    "The sites x species object must be a matrix or a data.frame",
    fixed = TRUE)
  
  
  # Matrix has no rows and/or columns ----
  
  expect_error(
    check_sites_species(matrix(ncol = 0, nrow = 0)),
    "The sites x species object should have at least one row and one column",
    fixed = TRUE)
  
  expect_error(
    check_sites_species(matrix(ncol = 0, nrow = 1)),
    "The sites x species object should have at least one row and one column",
    fixed = TRUE)
  
  expect_error(
    check_sites_species(matrix(ncol = 1, nrow = 0)),
    "The sites x species object should have at least one row and one column",
    fixed = TRUE)
  
  
  # Check for sites and species names ----
  
  mat <- matrix(1:10, ncol = 2)
  dat <- as.data.frame(mat)
  
  expect_error(
    check_sites_species(mat),
    "The sites x species object must have row names (sites names)",
    fixed = TRUE)
  
  expect_error(
    check_sites_species(dat),
    "The sites x species object must have row names (sites names)",
    fixed = TRUE)
  
  mat <- matrix(1:10, ncol = 2)
  rownames(mat) <- paste0("site_", seq_len(nrow(mat)))
  dat <- as.data.frame(mat)
  
  expect_error(
    check_sites_species(mat),
    "The sites x species object must have column names (species names)",
    fixed = TRUE)
  
  expect_error(
    check_sites_species(dat),
    "The sites x species object must have column names (species names)",
    fixed = TRUE)
  
  colnames(dat) <- NULL
  
  expect_error(
    check_sites_species(dat),
    "The sites x species object must have column names (species names)",
    fixed = TRUE)
  
  
  # Check for numeric ----
  
  mat <- as.data.frame(matrix(1:10, ncol = 2))
  rownames(mat) <- paste0("site_", seq_len(nrow(mat)))
  colnames(mat) <- paste0("species_", LETTERS[seq_len(ncol(mat))])
  mat$"site" <- rownames(mat)
  
  expect_error(
    check_sites_species(mat),
    paste0("The sites x species object must contain only numeric values. ", 
           "Sites names must be provided as row names"),
    fixed = TRUE)
  
  
  # Matrix should not contain negative values ----
  
  mat <- matrix(c(1:9, -1), ncol = 2)
  rownames(mat) <- paste0("site_", seq_len(nrow(mat)))
  colnames(mat) <- paste0("species_", LETTERS[seq_len(ncol(mat))])
  
  expect_error(
    check_sites_species(mat),
    "The sites x species object cannot contain negative values",
    fixed = TRUE)
  
  mat[1, 1] <- NA
  
  expect_error(
    check_sites_species(mat),
    "The sites x species object cannot contain negative values",
    fixed = TRUE)
  
  
  # Correct input ----
  
  mat <- matrix(1:10, ncol = 2)
  rownames(mat) <- paste0("site_", seq_len(nrow(mat)))
  colnames(mat) <- paste0("species_", LETTERS[seq_len(ncol(mat))])
  
  expect_silent(check_sites_species(mat))
  
  expect_equal(check_sites_species(mat), NULL)
  
  mat[1, 1] <- NA
  
  expect_silent(check_sites_species(mat))
  
  mat <- matrix(c(1, rep(NA, 9)), ncol = 2)
  rownames(mat) <- paste0("site_", seq_len(nrow(mat)))
  colnames(mat) <- paste0("species_", LETTERS[seq_len(ncol(mat))])
  
  expect_silent(check_sites_species(mat))
})


# Tests for check species x traits ---------------------------------------------

test_that("check_species_traits() works", {
  
  
  # Wrong input type ----
  
  expect_error(
    check_species_traits("a"),
    "The species x traits object must be a matrix or a data.frame",
    fixed = TRUE)
  
  expect_error(
    check_species_traits(1:10),
    "The species x traits object must be a matrix or a data.frame",
    fixed = TRUE)
  
  expect_error(
    check_species_traits(list()),
    "The species x traits object must be a matrix or a data.frame",
    fixed = TRUE)
  
  
  # Matrix or data.frame has no rows and/or columns ----
  
  expect_error(
    check_species_traits(matrix(ncol = 0, nrow = 0)),
    paste0("The species x traits object should have at least one row and ",
           "one column"),
    fixed = TRUE)
  
  expect_error(
    check_species_traits(matrix(ncol = 0, nrow = 1)),
    paste0("The species x traits object should have at least one row and ",
           "one column"),
    fixed = TRUE)
  
  expect_error(
    check_species_traits(matrix(ncol = 1, nrow = 0)),
    paste0("The species x traits object should have at least one row and ",
           "one column"),
    fixed = TRUE)
  
  expect_error(
    check_species_traits(data.frame()),
    paste0("The species x traits object should have at least one row and ",
           "one column"),
    fixed = TRUE)
  
  
  # Check for sites and species names ----
  
  mat <- matrix(1:10, ncol = 2)
  dat <- as.data.frame(mat)
  
  expect_error(
    check_species_traits(mat),
    "The species x traits object must have row names (species names)",
    fixed = TRUE)
  
  expect_error(
    check_species_traits(dat),
    "The species x traits object must have row names (species names)",
    fixed = TRUE)
  
  mat <- matrix(1:10, ncol = 2)
  rownames(mat) <- paste0("species_", seq_len(nrow(mat)))
  dat <- as.data.frame(mat)
  
  expect_error(
    check_species_traits(mat),
    "The species x traits object must have column names (traits names)",
    fixed = TRUE)
  
  expect_error(
    check_species_traits(dat),
    "The species x traits object must have column names (traits names)",
    fixed = TRUE)
  
  colnames(dat) <- NULL
  
  expect_error(
    check_species_traits(dat),
    "The species x traits object must have column names (traits names)",
    fixed = TRUE)
  
  
  # Correct Input ----

  mat <- matrix(1:10, ncol = 2)
  rownames(mat) <- paste0("species_", seq_len(nrow(mat)))
  colnames(mat) <- paste0("trait_", LETTERS[seq_len(ncol(mat))])
  
  expect_silent(check_species_traits(mat))
  
  expect_equal(check_species_traits(mat), NULL)
  
  dat <- as.data.frame(mat)
  
  expect_silent(check_species_traits(dat))
  
  dat$"trait_3" <- LETTERS[seq_len(nrow(dat))]
  
  expect_silent(check_species_traits(dat))
  
  dat[1, 3] <- NA
  
  expect_silent(check_species_traits(dat))
})


# Tests for check site x locations ---------------------------------------------

test_that("check_sites_locations() works", {
  
  data("sites_locs")
  
  sites_sf <- sites_locs
  sites_sf[["site"]] <- rownames(sites_sf)
  rownames(sites_sf) <- NULL
  sites_sf <- sf::st_as_sf(sites_sf, coords = 1:2)
  
  # Wrong input ----
  
  expect_error(
    check_sites_locations(sites_locs),
    "The sites x locations object must be an 'sf' object",
    fixed = TRUE
  )
  
  expect_error(
    check_sites_locations(as.list(sites_locs)),
    "The sites x locations object must be an 'sf' object",
    fixed = TRUE
  )
  
  expect_error(
    check_sites_locations(sites_locs[, 1, drop = FALSE]),
    paste0("The sites x locations object must be an 'sf' object"),
    fixed = TRUE
  )
  
  expect_error(
    check_sites_locations(sites_sf[-c(seq_len(nrow(sites_sf))),]),
    "The sites x locations object should have at least one row",
    fixed = TRUE
  )
  
  expect_silent(check_sites_locations(sites_sf))
  
  expect_equal(check_sites_locations(sites_sf), NULL)
})

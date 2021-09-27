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
  rownames(mat) <- paste0("site_", 1:nrow(mat))
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
  rownames(mat) <- paste0("site_", 1:nrow(mat))
  colnames(mat) <- paste0("species_", LETTERS[1:ncol(mat)])
  mat$"site" <- rownames(mat)
  
  expect_error(
    check_sites_species(mat),
    paste0("The sites x species object must contain only numeric values. ", 
           "Sites names must be provided as row names"),
    fixed = TRUE)
  
  
  # Matrix should not contain negative values ----
  
  mat <- matrix(c(1:9, -1), ncol = 2)
  rownames(mat) <- paste0("site_", 1:nrow(mat))
  colnames(mat) <- paste0("species_", LETTERS[1:ncol(mat)])
  
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
  rownames(mat) <- paste0("site_", 1:nrow(mat))
  colnames(mat) <- paste0("species_", LETTERS[1:ncol(mat)])
  
  expect_silent(check_sites_species(mat))
  
  expect_equal(check_sites_species(mat), NULL)
  
  mat[1, 1] <- NA
  
  expect_silent(check_sites_species(mat))
  
  mat <- matrix(c(1, rep(NA, 9)), ncol = 2)
  rownames(mat) <- paste0("site_", 1:nrow(mat))
  colnames(mat) <- paste0("species_", LETTERS[1:ncol(mat)])
  
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
  rownames(mat) <- paste0("species_", 1:nrow(mat))
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
  rownames(mat) <- paste0("species_", 1:nrow(mat))
  colnames(mat) <- paste0("trait_", LETTERS[1:ncol(mat)])
  
  expect_silent(check_species_traits(mat))
  
  expect_equal(check_species_traits(mat), NULL)
  
  dat <- as.data.frame(mat)
  
  expect_silent(check_species_traits(dat))
  
  dat$"trait_3" <- LETTERS[1:nrow(dat)]
  
  expect_silent(check_species_traits(dat))
  
  dat[1, 3] <- NA
  
  expect_silent(check_species_traits(dat))
})


# Tests for check site x locations ---------------------------------------------

test_that("check_sites_locations() works", {
  
  skip_if_not_installed("sf")
  
  # Wrong input ----
  
  expect_error(
    check_sites_locations("a"),
    "The sites x locations object should be an 'sf' object",
    fixed = TRUE
  )
  
  expect_error(
    check_sites_locations(NULL),
    "The sites x locations object should be an 'sf' object",
    fixed = TRUE
  )
  
  # Good input ---
  
  pt1 <- sf::st_point(c(0,1))
  pt2 <- sf::st_point(c(1,1))
  d <- data.frame(a = 1:2)
  d$geom <- sf::st_sfc(pt1, pt2)
  df <- sf::st_as_sf(d)
  
  expect_silent(check_sites_locations(df))
  
  expect_equal(check_sites_locations(df), NULL)
})

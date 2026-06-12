# Tests for check site x species  ----------------------------------------------
test_that("check_site_species() works", {
  # Wrong input type ----

  expect_error(
    check_site_species(),
    "Argument 'site_species' (site x species data frame) is required",
    fixed = TRUE
  )

  expect_error(
    check_site_species("a"),
    "The site x species object must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    check_site_species(1:10),
    "The site x species object must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    check_site_species(list()),
    "The site x species object must be a data.frame",
    fixed = TRUE
  )

  # Matrix has no rows and/or columns ----

  expect_error(
    check_site_species(data.frame(NULL)),
    "The site x species object should have at least one row and one column",
    fixed = TRUE
  )

  # Check for sites and species names ----

  mat <- matrix(1:10, ncol = 2)
  dat <- as.data.frame(mat)

  expect_error(
    check_site_species(dat),
    "The site x species object must contain the 'site' column",
    fixed = TRUE
  )

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
    fixed = TRUE
  )

  mat <- matrix(c(1:10), ncol = 2)
  rownames(mat) <- paste0("site_", seq_len(nrow(mat)))
  colnames(mat) <- paste0("species_", LETTERS[seq_len(ncol(mat))])

  dat <- as.data.frame(mat)
  dat[["site"]] <- rownames(mat)

  dat2 <- dat
  dat2[1, "site"] <- NA

  expect_error(
    check_site_species(dat2),
    "The column 'site' of site x species cannot contain missing values",
    fixed = TRUE
  )

  dat2[1, "site"] <- dat2[2, "site"]

  expect_error(
    check_site_species(dat2),
    "The column 'site' of site x species cannot contain duplicated values",
    fixed = TRUE
  )

  # Correct input ----

  mat <- matrix(1:10, ncol = 2)
  rownames(mat) <- paste0("site_", seq_len(nrow(mat)))
  colnames(mat) <- paste0("species_", LETTERS[seq_len(ncol(mat))])
  dat <- as.data.frame(mat)
  dat[["site"]] <- rownames(mat)

  expect_silent(check_site_species(dat))

  expect_null(check_site_species(dat))

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
    check_species_traits(),
    "Argument 'species_traits' (species x traits data frame) is required",
    fixed = TRUE
  )

  expect_error(
    check_species_traits("a"),
    "The species x traits object must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    check_species_traits(1:10),
    "The species x traits object must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    check_species_traits(list()),
    "The species x traits object must be a data.frame",
    fixed = TRUE
  )

  # Matrix or data.frame has no rows and/or columns ----

  expect_error(
    check_species_traits(data.frame()),
    paste0(
      "The species x traits object should have at least one row and ",
      "one column"
    ),
    fixed = TRUE
  )

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
    fixed = TRUE
  )

  mat <- matrix(1:10, ncol = 2)
  rownames(mat) <- paste0("species_", seq_len(nrow(mat)))
  dat <- as.data.frame(mat)
  dat[["species"]] <- rownames(dat)

  dat2 <- dat
  dat2[1, "species"] <- NA

  expect_error(
    check_species_traits(dat2),
    "The column 'species' of species x traits cannot contain missing values",
    fixed = TRUE
  )

  dat2[1, "species"] <- dat2[2, "species"]

  expect_error(
    check_species_traits(dat2),
    "The column 'species' of species x traits cannot contain duplicated values",
    fixed = TRUE
  )

  # Correct Input ----

  mat <- matrix(1:10, ncol = 2)
  rownames(mat) <- paste0("species_", seq_len(nrow(mat)))
  colnames(mat) <- paste0("trait_", LETTERS[seq_len(ncol(mat))])

  dat <- as.data.frame(mat)
  dat[["species"]] <- rownames(mat)

  expect_silent(check_species_traits(dat))

  expect_null(check_species_traits(dat))

  dat$"trait_3" <- LETTERS[seq_len(nrow(dat))]

  expect_silent(check_species_traits(dat))
})


# Tests for check site x locations ---------------------------------------------

test_that("check_site_locations() works", {
  data("woodiv_locations")

  sites_sf <- woodiv_locations

  # Wrong input ----

  expect_error(
    check_site_locations(),
    "Argument 'sites_locations' (spatial sites 'sf' object) is required",
    fixed = TRUE
  )

  expect_error(
    check_site_locations(data.frame("a")),
    "The site x locations object must be an 'sf' object",
    fixed = TRUE
  )

  expect_error(
    check_site_locations(as.list(data.frame("a"))),
    "The site x locations object must be an 'sf' object",
    fixed = TRUE
  )

  expect_error(
    check_site_locations("a"),
    paste0("The site x locations object must be an 'sf' object"),
    fixed = TRUE
  )

  expect_error(
    check_site_locations(sites_sf[-c(seq_len(nrow(sites_sf))), ]),
    "The site x locations object should have at least one row",
    fixed = TRUE
  )

  expect_error(
    check_site_locations(sites_sf[, -1]),
    "The site x locations object must contain the 'site' column",
    fixed = TRUE
  )

  sites <- sites_sf
  sites[1, "site"] <- NA

  expect_error(
    check_site_locations(sites),
    "The column 'site' of site x locations cannot contain missing values",
    fixed = TRUE
  )

  sites[1, "site"] <- sites[2, "site", drop = TRUE]

  expect_error(
    check_site_locations(sites),
    "The column 'site' of site x locations cannot contain duplicated values",
    fixed = TRUE
  )

  expect_silent(check_site_locations(sites_sf))

  expect_null(check_site_locations(sites_sf))
})


# Tests for check species-categories -------------------------------------------

test_that("check_species_categories() works", {
  ## Wrong inputs
  # Not good type of object
  expect_error(
    check_species_categories("a"),
    "The species x categories object must be a data.frame",
    fixed = TRUE
  )

  # Not enough rows
  expect_error(
    check_species_categories(data.frame("a")[-1, , drop = FALSE]),
    "The species x categories object should have at least one row",
    fixed = TRUE
  )

  # Not enough columns
  expect_error(
    check_species_categories(data.frame("a")),
    paste0(
      "The species x categories object must have two columns (species ",
      "name and one category)"
    ),
    fixed = TRUE
  )

  # Not well named columns
  expect_error(
    check_species_categories(data.frame("a", "b")),
    "The species x categories object must contain the 'species' column",
    fixed = TRUE
  )

  dat <- data.frame(
    species = c("a", NA),
    category = c("plant", "plant")
  )

  expect_error(
    check_species_categories(dat),
    paste0(
      "The column 'species' of species x categories cannot contain ",
      "missing values"
    ),
    fixed = TRUE
  )

  dat[2, "species"] <- dat[1, "species"]

  expect_error(
    check_species_categories(dat),
    paste0(
      "The column 'species' of species x categories cannot contain ",
      "duplicated values"
    ),
    fixed = TRUE
  )

  ## Working input
  expect_silent(
    check_species_categories(data.frame(species = "a", category = "plant"))
  )
})


# Tests for check threshold proportion -----------------------------------------

test_that("check_threshold_proportion() works", {
  ## Wrong inputs
  # Missing threshold
  expect_error(
    check_threshold_proportion(),
    "Argument '' (trait coverage) is required",
    fixed = TRUE
  )

  # Wrong 'type'
  expect_error(
    check_threshold_proportion(0, type = "bla"),
    "'arg' should be one of \"trait\", \"site\", \"species\"",
    fixed = TRUE
  )

  # Threshold below 0
  expect_error(
    check_threshold_proportion(-1),
    paste0(
      "Argument '-1' (trait coverage proportion) should be ",
      "a numeric value >= 0 and <= 1"
    ),
    fixed = TRUE
  )

  # Threshold above 1
  expect_error(
    check_threshold_proportion(2),
    paste0(
      "Argument '2' (trait coverage proportion) should be ",
      "a numeric value >= 0 and <= 1"
    ),
    fixed = TRUE
  )

  # Correct inputs
  expect_silent(check_threshold_proportion(0))
  expect_silent(check_threshold_proportion(0.5))
  expect_silent(check_threshold_proportion(1))
})

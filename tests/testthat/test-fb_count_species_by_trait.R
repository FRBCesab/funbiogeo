# Fake dataset ----

species_traits <- data.frame(
  species = paste0("sp", 1:4),
  t1      = c(NA, 2.5, 100, 400),
  t2      = c(2.2, 5.0, 200, 200)
)

species_traits2 <- data.frame(
  species = paste0("sp", 1:4),
  t1      = c(NA, NA, NA, NA),
  t2      = c(2.2, 5.0, 200, 200)
)

species_traits3 <- data.frame(
  species = paste0("sp", 1:4),
  t1      = c(NA, 2.5, 100, 400),
  t2      = c(NA, 5.0, 200, 200)
)


test_that("fb_count_species_by_trait() errors with wrong inputs", {
  
  # Wrong inputs ----
  
  expect_error(
    fb_count_species_by_trait(),
    "Argument 'species_traits' (species x traits data frame) is required",
    fixed = TRUE
  )
  
  expect_error(
    {
      st2 <- species_traits
      colnames(st2) <- NULL
      fb_count_species_by_trait(st2)
    },
    "The species x traits object must have column names (trait names)",
    fixed = TRUE
  )
  
  expect_error(
    fb_count_species_by_trait(species_traits[ , -1, drop = FALSE]),
    "The species x traits object must contain the 'species' column",
    fixed = TRUE
  )
})



test_that("fb_count_species_by_trait() successfully works", {
    
  # Success ----
  
  expect_silent({
    st3 <- species_traits
    st3[1, 2] <- NA
    test_coverage <- fb_count_species_by_trait(st3)
  })
  
  
  # Output format ----
  
  expect_s3_class(test_coverage, "data.frame")
  expect_named(test_coverage, c("trait", "n_species", "coverage"))
  expect_type(test_coverage$"trait", "character")
  expect_type(test_coverage$"n_species", "integer")
  expect_type(test_coverage$"coverage", "double")
  expect_equal(test_coverage$"trait"[1], "t2")
  expect_equal(test_coverage$"n_species"[1], 4)
  expect_equal(test_coverage$"coverage"[1], 1.00)
  
  
  # Test for a trait with all NA ----
  
  expect_silent({
    test_coverage <- fb_count_species_by_trait(species_traits2)
  })
  
  expect_equal(nrow(test_coverage), (ncol(species_traits2) - 1))
  expect_equal(ncol(test_coverage), 3)
  
  expect_equal(test_coverage$"trait"[2], "t1")
  expect_equal(test_coverage$"n_species"[2], 0)
  expect_equal(test_coverage$"coverage"[2], 0)
  
  # Test for a species without any trait ----
  
  expect_silent({
    test_coverage <- fb_count_species_by_trait(species_traits3)
  })
  
  expect_equal(nrow(test_coverage), (ncol(species_traits2) - 1))
  expect_equal(ncol(test_coverage), 3)
  
  expect_equal(test_coverage$"trait"[1], "t1")
  expect_equal(test_coverage$"n_species"[1], 3)
  expect_equal(test_coverage$"coverage"[1], 0.75)
})

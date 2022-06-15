# Fake dataset ----

species_traits <- data.frame(
  species = paste0("sp", 1:4),
  t1      = c(NA, 2.5, 100, 400),
  t2      = c(2.2, 5.0, 200, 200)
)


test_that("fb_count_traits_by_species() errors with wrong inputs", {
  
  # Wrong inputs ----
  
  expect_error(
    fb_count_traits_by_species(),
    "Argument 'species_traits' (species x traits data frame) is required",
    fixed = TRUE
  )
  
  expect_error(
    {
      st2 <- species_traits
      colnames(st2) <- NULL
      fb_count_traits_by_species(st2)
    },
    "The species x traits object must have column names (trait names)",
    fixed = TRUE
  )
  
  expect_error(
    fb_count_traits_by_species(species_traits[ , -1, drop = FALSE]),
    "The species x traits object must contain the 'species' column",
    fixed = TRUE
  )
  
  
  # Success ----
  
  expect_silent({
    st3 <- species_traits
    st3[1, 2] <- NA
    test_coverage <- fb_count_traits_by_species(st3)
  })
  
  
  # Output format ----
  
  expect_s3_class(test_coverage, "data.frame")
  expect_named(test_coverage, c("species", "n_traits", "coverage"))
  expect_type(test_coverage$"species", "character")
  expect_type(test_coverage$"n_traits", "integer")
  expect_type(test_coverage$"coverage", "double")
  expect_equal(test_coverage$"species"[1], "sp2")
  expect_equal(test_coverage$"n_traits"[1], 2)
  expect_equal(test_coverage$"coverage"[1], 1.0)
  
})

# Initial data -----------------------------------------------------------------
sp_trait <- data.frame(
  species = letters[1:5],
  tr1     = 1:5,
  tr2     = c(0.5:3.5, NA),
  tr3     = c(letters[1:4], NA),
  tr4     = factor(c(letters[1:4], NA)),
  tr5     = factor(c(letters[1:4], NA), ordered = TRUE)
)

# Actual Tests -----------------------------------------------------------------

test_that("fb_table_trait_summary() work ", {
  
  ## Wrong Input
  
  expect_error(
    fb_table_trait_summary(sp_trait, kable = "A"),
    "Argument 'kable' should be a logical",
    fixed = TRUE
  )
  
  
  ## Good Input
  # Non-kable
  
  expect_silent(
    res <- fb_table_trait_summary(sp_trait, kable = FALSE)
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(5, 8))
  expect_equal(
    colnames(res),
    c("trait_name", "trait_type", "number_non_missing",
      "proportion_non_missing", "trait_range", "trait_mean_sd",
      "number_distinct", "list_distinct")
  )
  expect_equal(res[["trait_name"]], colnames(sp_trait)[-1])
  expect_equal(
    res[["trait_type"]],
    c("numeric", "numeric", "categorical", "categorical", "ordered")
  )
  expect_equal(res[1, 3], 5)
  expect_equal(res[1, 4], "100 %")
  expect_equal(res[2, 3], 4)
  expect_equal(res[2, 4], "80 %")
  expect_equal(res[1, 8], NA_character_)
  expect_equal(res[3, 7], 4)
  
  # kable
  expect_silent(
    res <- fb_table_trait_summary(sp_trait, kable = TRUE)
  )
  
  expect_s3_class(res, "knitr_kable")
})

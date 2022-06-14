test_that("count_species_per_trait works", {
  sp_tr = data.frame(
    species =    paste0("sp", rep(1:10, 2)),
    trait_name = paste0("tr", rep(1:2, each = 10)),
    trait_value = c(rep(1, 10), rep(1, 8), NA_real_, NA_real_) 
  )
  
  expect_silent(res <- count_species_per_trait(sp_tr))
  
  expect_true(is(res, "data.frame"))
  expect_equal(dim(res), c(2L, 4L))
  expect_equal(res[1, 1], 10)
  expect_equal(res[1, 2], 1.0)
  expect_equal(res[1, 3], "tr1")
})

test_that("count_trait_per_species works", {
  sp_tr = data.frame(
    species =    paste0("sp", rep(1:10, 2)),
    trait_name = paste0("tr", rep(1:2, each = 10)),
    trait_value = c(rep(1, 10), rep(1, 8), NA_real_, NA_real_) 
  )
  
  expect_silent(res <- count_trait_per_species(sp_tr))
  
  expect_true(is(res, "data.frame"))
  expect_equal(dim(res), c(10L, 3L))
  expect_equal(res[1, 1], 2)
  expect_equal(res[1, 2], 1.0)
  expect_equal(res[1, 3], "sp1")
})
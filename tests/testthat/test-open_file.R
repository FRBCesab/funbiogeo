test_that("open_file() works", {
  
  temp = tempfile("test", fileext = "txt")
  
  file.create(temp)
  
  expect_silent(open_file(temp))
  
  unlink(temp)
})

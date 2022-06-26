# Create temporary folders ----

path  <- tempdir()

path1 <- file.path(path, "path1")
dir.create(path1, showWarnings = FALSE)

path2 <- file.path(path, "path2")
dir.create(path2, showWarnings = FALSE)

# Default file name ----

filename <- file.path(path2, "funbiogeo_report.Rmd")


# Test for errors ----

test_that("fb_make_report() errors", {
  
  # Wrong path ----
  
  expect_error(
    fb_make_report(path = file.path(path1, "reports")),
    paste0("The path '", file.path(path1, "reports"), "' does not exist"),
    fixed = TRUE)

  # File already exists and overwrite is FALSE ----
  
  invisible(file.create(filename))
  
  expect_error(
    fb_make_report(path = path2),
    paste0("The file '", file.path(path2, "funbiogeo_report.Rmd"), "' already ",
           "exists. If you want to replace it, use 'overwrite = TRUE'."),
    fixed = TRUE) 
  
})



# Test option overwrite ----

test_that("fb_make_report() overwrite option", {
  
  # File already exists and overwrite is TRUE ----
  
  invisible(file.remove(filename))
  invisible(file.create(filename)) # Create empty file
  
  expect_silent(
    fb_make_report(path = path2, overwrite = TRUE)
  )
  
  content <- readLines(filename)
  
  expect_equal(length(grep("^title: ", content)), 1)
})



# Test for filenames and titles ----

test_that("fb_make_report() filename and title", {
  
  # Default ----
  
  invisible(file.remove(filename))
  
  expect_silent(
    fb_make_report(path = path2)
  )
  
  expect_true(file.exists(filename))
  
  content <- readLines(filename)
  
  expect_equal(length(grep("funbiogeo Report", content)), 1)
  
  invisible(file.remove(filename))
  
  
  # Filename provided (with extension) ----
  
  file_name <- "my_report.Rmd"
  
  expect_silent(
    fb_make_report(path = path2, filename = file_name)
  )
  
  expect_true(file.exists(file.path(path2, file_name)))
  
  content <- readLines(file.path(path2, file_name))
  
  expect_equal(length(grep("my_report", content)), 1)
  
  invisible(file.remove(file.path(path2, file_name)))
  
  
  # Filename provided (with lowercase extension) ----
  
  file_name <- "my_report.Rmd"
  
  expect_silent(
    fb_make_report(path = path2, filename = tolower(file_name))
  )
  
  expect_true(file.exists(file.path(path2, file_name)))
  
  content <- readLines(file.path(path2, file_name))
  
  expect_equal(length(grep("my_report", content)), 1)
  
  invisible(file.remove(file.path(path2, file_name)))
  
  
  # Filename provided (without extension) ----
  
  file_name <- "my_report"
  
  expect_silent(
    fb_make_report(path = path2, filename = file_name)
  )
  
  expect_true(file.exists(file.path(path2, paste0(file_name, ".Rmd"))))
  
  content <- readLines(file.path(path2, paste0(file_name, ".Rmd")))
  
  expect_equal(length(grep("my_report", content)), 1)
  
  invisible(file.remove(file.path(path2, paste0(file_name, ".Rmd"))))
  
  
  # Title provided (without punctuation)
  
  title <- "My Beautiful Title"
  expected_filename <- file.path(path2, "my_beautiful_title.Rmd")
    
  expect_silent(
    fb_make_report(path = path2, title = title)
  )
  
  expect_true(file.exists(expected_filename))
  
  content <- readLines(expected_filename)
  
  expect_equal(length(grep(title, content)), 1)
  
  invisible(file.remove(expected_filename))
  
  
  # Title provided (with punctuations)
  
  title <- "Report: My      Beautiful Title"
  expected_filename <- file.path(path2, "report_my_beautiful_title.Rmd")
  
  expect_silent(
    fb_make_report(path = path2, title = title)
  )
  
  expect_true(file.exists(expected_filename))
  
  content <- readLines(expected_filename)
  
  expect_equal(length(grep(title, content)), 1)
  
  invisible(file.remove(expected_filename))
  
  
  # Both Title and Filename are provided
  
  title    <- "My Beautiful Report"
  file_name <- "report_made_by_funbiogeo.Rmd"
  
  expect_silent(
    fb_make_report(path = path2, title = title, filename = file_name)
  )
  
  expect_true(file.exists(file.path(path2, file_name)))
  
  content <- readLines(file.path(path2, file_name))
  
  expect_equal(length(grep(title, content)), 1)
  
  invisible(file.remove(file.path(path2, file_name)))
  
})


# Test for author ----

test_that("fb_make_report() authorship", {
  
  # No author provided ----
  
  expect_silent(
    fb_make_report(path = path2, overwrite = TRUE)
  )
  
  expect_true(file.exists(filename))
  
  content <- readLines(filename)
  
  expect_equal(length(grep("^author: ", content)), 0)
  
  invisible(file.remove(filename))
  
  
  # Single author provided ----
  
  expect_silent(
    fb_make_report(path = path2, overwrite = TRUE, author = "Jane Doe")
  )
  
  expect_true(file.exists(filename))
  
  content <- readLines(filename)
  
  expect_equal(length(grep("^author: \"Jane Doe\"$", content)), 1)
  
  
  # Multiple authors provided ----
  
  expect_silent(
    fb_make_report(path = path2, overwrite = TRUE, author = "Doe J. and Doe J.")
  )
  
  expect_true(file.exists(filename))
  
  content <- readLines(filename)
  
  expect_equal(length(grep("^author: \"Doe J. and Doe J.\"$", content)), 1)
  
  
  # Multiple authors provided ----
  
  expect_silent(
    fb_make_report(path = path2, overwrite = TRUE, 
                   author = c("Doe J.", "Doe J."))
  )
  
  expect_true(file.exists(filename))
  
  content <- readLines(filename)
  
  expect_equal(length(grep("^author: \"Doe J., Doe J.\"$", content)), 1)
})



# Test for outputs ----

test_that("fb_make_report() output", {
  
  expect_invisible(
    fb_make_report(path = path2, overwrite = TRUE)
  )
  
  expect_silent(
    res <- fb_make_report(path = path2, overwrite = TRUE)
  )
  
  expect_null(res)
})
  
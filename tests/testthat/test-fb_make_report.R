# Default file name ----

filename <- file.path("funbiogeo", "funbiogeo_report.Rmd")

# Fake datasets ----

sp_tr <- data.frame(
  species = paste0("sp", 1:4),
  t1      = c(0.01, 0.05, 0.05, 0.10),
  t2      = c( 100,  100,  200,  400),
  t3      = c( "A",  "A",  "B",  "C")
)

st_sp <- data.frame(
  site = 1:4,
  sp1  = c( 0, 1, 0, 1),
  sp2  = c( 1, 1, 0, 0)
)

sp_cat <- data.frame(
  species = paste0("sp", 1:4),
  fam     = c("A", "A", "B", "B")
)

data("woodiv_locations")
site_locations <- woodiv_locations


st_loc <- site_locations[1:4, ]


# Create fake directory ----

temp_dir <- create_tempdir()
dir.create(file.path(temp_dir, "funbiogeo"))


# Test for errors ----

test_that("fb_make_report() errors", {

  # Wrong path -----------------------------------------------------------------
  
  expect_error(
    fb_make_report(
      path = file.path(temp_dir, "reports"), interactive = FALSE
    ),
    paste0("The path '", file.path(temp_dir, "reports"), "' does not exist"),
    fixed = TRUE)

  
  # File already exists and overwrite is FALSE ---------------------------------
    
  invisible(file.create(file.path(temp_dir, filename)))
  
  expect_error(
    fb_make_report(
      path = temp_dir, interactive = FALSE
    ),
    paste0("The file '", file.path(temp_dir, filename), "' already ",
           "exists. If you want to replace it, use 'overwrite = TRUE'."),
    fixed = TRUE) 
  
  
  # Clean directory ----

  invisible(file.remove(file.path(temp_dir, filename)))
  invisible(unlink(file.path(temp_dir, "funbiogeo"), recursive = TRUE))


  # Wrong sites x species ------------------------------------------------------
  
  expect_error(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, interactive = FALSE
    ),
    "Argument 'site_species' (site x species data frame) is required",
    fixed = TRUE
  )

  expect_error(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, site_species = NULL, 
      interactive = FALSE
    ),
    "The site x species object must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, site_species = st_sp$"site", 
      interactive = FALSE
    ),
    "The site x species object must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, site_species = "site_species", 
      interactive = FALSE
    ),
    "The site x species object must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, site_species = st_sp[NULL, ], 
      interactive = FALSE
    ),
    "The site x species object should have at least one row and one column",
    fixed = TRUE
  )

  expect_error(
    {
      st_sp2 <- st_sp
      colnames(st_sp2) <- NULL
      fb_make_report(
        path = temp_dir, overwrite = TRUE, site_species = st_sp2, 
        interactive = FALSE
      )
    },
    "The site x species object must have column names (species names)",
    fixed = TRUE
  )

  expect_error(
    {
      st_sp2 <- st_sp
      colnames(st_sp2)[1] <- "location"
      fb_make_report(
        path = temp_dir, overwrite = TRUE, site_species = st_sp2, 
        interactive = FALSE
      )
    },
    "The site x species object must contain the 'site' column",
    fixed = TRUE
  )

  expect_error(
    {
      st_sp2 <- st_sp
      st_sp2[1, 2] <- -1
      fb_make_report(
        path = temp_dir, overwrite = TRUE, site_species = st_sp2, 
        interactive = FALSE
      )
    },
    "The site x species object cannot contain negative values",
    fixed = TRUE
  )
  

  # Wrong sites x locations ----------------------------------------------------
  
  expect_error(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, site_species = st_sp, 
      interactive = FALSE
    ),
    "Argument 'sites_locations' (spatial sites 'sf' object) is required",
    fixed = TRUE
  )

  expect_error(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, site_species = st_sp, 
      site_locations = NULL, interactive = FALSE
    ),
    "The site x locations object must be an 'sf' object",
    fixed = TRUE
  )

  expect_error(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, site_species = st_sp, 
      site_locations = sf::st_drop_geometry(st_loc), interactive = FALSE
    ),
    "The site x locations object must be an 'sf' object",
    fixed = TRUE
  )

  expect_error(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, site_species = st_sp, 
      site_locations = st_loc[NULL, ], interactive = FALSE
    ),
    "The site x locations object should have at least one row",
    fixed = TRUE
  )


  # Wrong sites x locations ----------------------------------------------------
  
  expect_error(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, site_species = st_sp, 
      site_locations = st_loc, interactive = FALSE
    ),
    "Argument 'species_traits' (species x traits data frame) is required",
    fixed = TRUE
  )

  expect_error(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, site_species = st_sp, 
      site_locations = st_loc, species_traits = NULL, interactive = FALSE
    ),
    "The species x traits object must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, site_species = st_sp, 
      site_locations = st_loc, species_traits = sp_tr[NULL, ], 
      interactive = FALSE
    ),
    "The species x traits object should have at least one row and one column",
    fixed = TRUE
  )

  expect_error(
    {
      sp_tr2 <- sp_tr
      colnames(sp_tr2) <- NULL
      fb_make_report(
        path = temp_dir, overwrite = TRUE, site_species = st_sp, 
        site_locations = st_loc, species_traits = sp_tr2, interactive = FALSE
      )
    },
    "The species x traits object must have column names (trait names)",
    fixed = TRUE
  )

  expect_error(
    {
      sp_tr2 <- sp_tr
      colnames(sp_tr2)[1] <- "espece"
      fb_make_report(
        path = temp_dir, overwrite = TRUE, site_species = st_sp, 
        site_locations = st_loc, species_traits = sp_tr2, interactive = FALSE
      )
    },
    "The species x traits object must contain the 'species' column",
    fixed = TRUE
  )


  # Wrong species x categories -------------------------------------------------

  # ...


  # Datasets already exists ----------------------------------------------------

  dir.create(file.path(temp_dir, "funbiogeo", "data"), showWarnings = FALSE)

  filename <- file.path("funbiogeo", "data", "fb_site_species.rds")
  invisible(file.create(file.path(temp_dir, filename)))
  
  expect_error(
    fb_make_report(
      path = temp_dir, site_species = st_sp, site_locations = st_loc, 
      species_traits = sp_tr, interactive = FALSE
    ),
    paste0("The file '", file.path(temp_dir, filename), "' already ",
           "exists. If you want to replace it, use 'overwrite = TRUE'."),
    fixed = TRUE
  )
  
  invisible(file.remove(file.path(temp_dir, filename)))


  filename <- file.path("funbiogeo", "data", "fb_site_locations.rds")
  invisible(file.create(file.path(temp_dir, filename)))
  
  expect_error(
    fb_make_report(
      path = temp_dir, site_species = st_sp, site_locations = st_loc, 
      species_traits = sp_tr, interactive = FALSE
    ),
    paste0("The file '", file.path(temp_dir, filename), "' already ",
           "exists. If you want to replace it, use 'overwrite = TRUE'."),
    fixed = TRUE
  )
  
  filename <- file.path("funbiogeo", "data", "fb_site_locations.rds")
  invisible(file.remove(file.path(temp_dir, filename)))

  filename <- file.path("funbiogeo", "data", "fb_site_species.rds")
  invisible(file.remove(file.path(temp_dir, filename)))


  filename <- file.path("funbiogeo", "data", "fb_species_traits.rds")
  invisible(file.create(file.path(temp_dir, filename)))
  
  expect_error(
    fb_make_report(
      path = temp_dir, site_species = st_sp, site_locations = st_loc, 
      species_traits = sp_tr, interactive = FALSE
    ),
    paste0("The file '", file.path(temp_dir, filename), "' already ",
           "exists. If you want to replace it, use 'overwrite = TRUE'."),
    fixed = TRUE
  )
  
  filename <- file.path("funbiogeo", "data", "fb_site_locations.rds")
  invisible(file.remove(file.path(temp_dir, filename)))
  
  filename <- file.path("funbiogeo", "data", "fb_site_species.rds")
  invisible(file.remove(file.path(temp_dir, filename)))

  filename <- file.path("funbiogeo", "data", "fb_species_traits.rds")
  invisible(file.remove(file.path(temp_dir, filename)))
})



# Test option overwrite --------------------------------------------------------

test_that("fb_make_report() overwrite option", {
  
  # File already exists and overwrite is TRUE ----
  
  filename <- file.path("funbiogeo", "funbiogeo_report.Rmd")

  invisible(file.create(file.path(temp_dir, filename))) # Create empty file
  
  expect_message(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, species_traits = sp_tr, 
      site_species = st_sp,site_locations = st_loc, open = FALSE, 
      interactive = FALSE
    )
  )
  
  content <- readLines(file.path(temp_dir, filename))
  
  expect_equal(length(grep("^title: ", content)), 1)

  invisible(file.remove(file.path(temp_dir, filename)))
})



# Test for filenames and titles ------------------------------------------------

test_that("fb_make_report() filename and title", {
  
  # Default ----
  
  filename <- file.path("funbiogeo", "funbiogeo_report.Rmd")

  expect_message(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, species_traits = sp_tr, 
      site_species = st_sp,site_locations = st_loc, open = FALSE, 
      interactive = FALSE
    )
  )
  
  expect_true(file.exists(file.path(temp_dir, filename)))
  
  content <- readLines(file.path(temp_dir, filename))
  
  expect_equal(length(grep("funbiogeo Report", content)), 1)
  
  invisible(file.remove(file.path(temp_dir, filename)))
  
  
  # Filename provided (with extension) ----
  
  file_name <- "my_report.Rmd"
  filename  <- file.path("funbiogeo", file_name)

  expect_message(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, species_traits = sp_tr, 
      site_species = st_sp,site_locations = st_loc, open = FALSE,
      filename = file_name, interactive = FALSE
    )
  )
  
  expect_true(file.exists(file.path(temp_dir, filename)))
  
  content <- readLines(file.path(temp_dir, filename))
  expect_equal(length(grep("My Report", content)), 1)
  
  invisible(file.remove(file.path(temp_dir, filename)))
  
  
  # Filename provided (without extension) ----
  
  file_name <- "my_report"
  filename  <- file.path("funbiogeo", file_name)

  expect_message(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, species_traits = sp_tr, 
      site_species = st_sp,site_locations = st_loc, open = FALSE,
      filename = file_name, interactive = FALSE
    )
  )
  
  expect_true(file.exists(file.path(temp_dir, paste0(filename, ".Rmd"))))
  
  content <- readLines(file.path(temp_dir, paste0(filename, ".Rmd")))
  expect_equal(length(grep("My Report", content)), 1)
  
  invisible(file.remove(file.path(temp_dir, paste0(filename, ".Rmd"))))
  
  
  # Title provided (and not filename) ----
  
  title             <- "My Beautiful Title"
  expected_filename <- file.path("funbiogeo", "my_beautiful_title.Rmd")
    
  expect_message(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, species_traits = sp_tr, 
      site_species = st_sp,site_locations = st_loc, open = FALSE,
      title = title, interactive = FALSE
    )
  )
  
  expect_true(file.exists(file.path(temp_dir, expected_filename)))
  
  content <- readLines(file.path(temp_dir, expected_filename))
  expect_equal(length(grep(title, content)), 1)
  
  invisible(file.remove(file.path(temp_dir, expected_filename)))
  
  
  # Title provided (with punctuations) ----
  
  title             <- "Report: My      Beautiful Title"
  expected_filename <- file.path("funbiogeo", "report_my_beautiful_title.Rmd")
  
  expect_message(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, species_traits = sp_tr, 
      site_species = st_sp,site_locations = st_loc, open = FALSE,
      title = title, interactive = FALSE
    )
  )
  
  expect_true(file.exists(file.path(temp_dir, expected_filename)))
  
  content <- readLines(file.path(temp_dir, expected_filename))
  expect_equal(length(grep(title, content)), 1)
  
  invisible(file.remove(file.path(temp_dir, expected_filename)))
  
  
  # Both Title and Filename are provided ----
  
  title     <- "My Beautiful Report"
  file_name <- "report_made_by_funbiogeo.Rmd"
  filename  <- file.path("funbiogeo", file_name)
  
  expect_message(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, species_traits = sp_tr, 
      site_species = st_sp,site_locations = st_loc, open = FALSE,
      title = title, filename = file_name, interactive = FALSE
    )
  )
  
  expect_true(file.exists(file.path(temp_dir, filename)))
  
  content <- readLines(file.path(temp_dir, filename))
  expect_equal(length(grep(title, content)), 1)
  
  invisible(file.remove(file.path(temp_dir, filename)))
})


# Test for author ----

test_that("fb_make_report() authorship", {
  
  filename <- file.path("funbiogeo", "funbiogeo_report.Rmd")

  # No author provided ----
  
  expect_message(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, species_traits = sp_tr, 
      site_species = st_sp, site_locations = st_loc, open = FALSE, 
      interactive = FALSE
    )
  )
  
  expect_true(file.exists(file.path(temp_dir, filename)))
  
  content <- readLines(file.path(temp_dir, filename))
  expect_equal(length(grep("^author: ", content)), 0)
  
  
  # Single author provided ----
  
  expect_message(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, species_traits = sp_tr, 
      site_species = st_sp, site_locations = st_loc,
      species_categories = sp_cat, open = FALSE, author = "Jane Doe",
      interactive = FALSE
    )
  )
  
  expect_true(file.exists(file.path(temp_dir, filename)))
  
  content <- readLines(file.path(temp_dir, filename))
  expect_equal(length(grep("^author: \"Jane Doe\"$", content)), 1)
  
  
  # Multiple authors provided ----
  
  expect_message(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, species_traits = sp_tr, 
      site_species = st_sp, site_locations = st_loc, open = FALSE,
      author = "Doe J. and Doe J.", interactive = FALSE
    )
  )
  
  expect_true(file.exists(file.path(temp_dir, filename)))
  
  content <- readLines(file.path(temp_dir, filename))
  expect_equal(length(grep("^author: \"Doe J. and Doe J.\"$", content)), 1)
  
  
  # Multiple authors provided ----
  
  expect_message(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, species_traits = sp_tr, 
      site_species = st_sp, site_locations = st_loc, open = FALSE,
      author = c("Doe J.", "Doe J."), interactive = FALSE
    )
  )

  expect_true(file.exists(file.path(temp_dir, filename)))
  
  content <- readLines(file.path(temp_dir, filename))
  expect_equal(length(grep("^author: \"Doe J., Doe J.\"$", content)), 1)
  
  # Test open file -------------------------------------------------------------
  expect_message(
    fb_make_report(
      path = temp_dir, overwrite = TRUE, species_traits = sp_tr, 
      site_species = st_sp, site_locations = st_loc, open = TRUE,
      author = c("Doe J.", "Doe J."), interactive = FALSE
    )
  )
})

withr::deferred_clear()

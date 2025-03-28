#' Create an Rmarkdown Report to Explore User Data
#'
#' Creates an R Markdown (`.Rmd`) report from a template to explore and 
#' summarize user data. User can modify this report and use the function
#' [rmarkdown::render()] (or click the _Render_ of the RStudio IDE) to convert 
#' this `.Rmd` in different formats:
#'   - HTML document (`output_format = "bookdown::html_document2"`);
#'   - PDF document (`output_format = "bookdown::pdf_document2"`);
#'   - Word document (`output_format = "bookdown::word_document2"`);
#'   - HTML, PDF and Word documents (`output_format = "all"`).
#' 
#' Note that a copy of user data will be saved as `.rds` files in 
#' `path/funbiogeo/data/` (where `path` is the directory defined by the user).
#' 
#' @param path a `character` of length 1. The directory in which the `.Rmd` and
#'   `.rds` files will be created. This directory must exist. Note that 
#'   subdirectories `funbiogeo/` and `funbiogeo/data/` will be created. Default 
#'   is the current directory.
#' 
#' @param filename a `character` of length 1. The name of the `.Rmd` file to be
#'   created. If `NULL` (default) the `.Rmd` file will be named from the `title`
#'   (if provided) or `funbiogeo_report.Rmd` otherwise.
#'   
#' @param title a `character` of length 1. The title of the report.
#'   If `NULL` (default) the title will be named from the `title`
#'   (if provided) or `funbiogeo Report` otherwise.
#'   
#' @param author a `character` of length 1. The author(s) of the report. 
#'   If `NULL` (default) no author will be added.
#' 
#' @param overwrite a logical. If the `.Rmd` file (or any `.rds` dataset) is 
#'   already present and `overwrite = TRUE`, the `.Rmd` file (and all `.rds` 
#'   files) will be replaced. Default is `FALSE`.
#' 
#' @param open a logical. If `TRUE` (default), the `.Rmd` file will be opened 
#'   in the text editor.
#' 
#' @param interactive a logical. If `TRUE` (default), the function will ask user
#'   to accept the copy of datasets.
#' 
#' @inheritParams fb_get_environment
#' @inheritParams fb_get_trait_coverage_by_site
#' @inheritParams fb_plot_species_traits_completeness
#' 
#' @return No return value.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create temporary folder (optional) ----
#' temp_path <- tempdir()
#' 
#' # Create report ----
#' fb_make_report(
#'   path           = temp_path, 
#'   author         = "Casajus N. and Grenié M.",
#'   species_traits = woodiv_traits,
#'   site_species   = woodiv_site_species,
#'   site_locations = woodiv_locations,
#'   open           = FALSE
#' )
#' 
#' # Open Rmd file ----
#' utils::file.edit(file.path(temp_path, "funbiogeo", "funbiogeo_report.Rmd"))
#' 
#' # Render Rmd file ----
#' rmarkdown::render(
#'   input         = file.path(temp_path, "funbiogeo", "funbiogeo_report.Rmd"),
#'   output_format = "all"
#' )
#' }

fb_make_report <- function(path = ".", filename = NULL, title = NULL, 
                           author = NULL, species_traits, site_species, 
                           site_locations, species_categories = NULL,
                           overwrite = FALSE, open = TRUE, interactive = TRUE) {
  
  open <- open && rlang::is_interactive()
  
  if (interactive) {

    prompt <- paste(
      "funbiogeo will create a copy of your datasets in 'path/'. Do you want", 
      "to proceed? [Y/n] "
    )

    answer <- readline(prompt)

    if (answer == "") {
      answer <- "yes"
    }

    answer <- tolower(answer)
    answer <- substr(answer, 1, 1)

    if (!(answer %in% c("y", "n"))) {
      stop("Please answer 'yes' or 'no'", call. = FALSE)
    }

    if (answer == "n") {
      stop(
        "You must agree to copy your data to generate the Rmd report", 
        call. = FALSE
      )
    }
  }


  # Check path -----------------------------------------------------------------
  
  if (!dir.exists(path)) {
    stop("The path '", path, "' does not exist", call. = FALSE)
  }

  # Create subdirectories ------------------------------------------------------

  path      <- file.path(path, "funbiogeo")
  path_data <- file.path(path, "data")

  dir.create(path_data, showWarnings = FALSE, recursive = TRUE)
  
  
  # Create file name and title -------------------------------------------------
  
  if (is.null(title) && !is.null(filename)) {

    title <- gsub("\\.Rmd$", "", filename, ignore.case = TRUE)
    title <- gsub("[[:punct:]]", " ", title)
    title <- trimws(title)
    title <- tools::toTitleCase(title)
  }
  
  if (!is.null(title) && is.null(filename)) {
    
    filename <- gsub("[[:punct:]]|\\s", "_", title)
    filename <- gsub("_{1,}", "_", filename)
    filename <- tolower(filename)
  }
  
  if (is.null(filename) && is.null(title)) {

    filename <- "funbiogeo_report.Rmd"
    title    <- "funbiogeo Report"
  }

  filename <- gsub("\\.Rmd$", "", filename, ignore.case = TRUE)
  filename <- paste0(filename, ".Rmd")
  
  path_rmd <- file.path(path, filename)
  
  
  # If file exists -------------------------------------------------------------
  
  if (file.exists(path_rmd) && !overwrite) {
    stop(
      "The file '", path_rmd, "' already exists. If you want to replace it, ", 
      "use 'overwrite = TRUE'.", call. = FALSE
    )
  }
  
  
  # Check datasets -------------------------------------------------------------
  
  check_site_species(site_species)
  check_site_locations(site_locations)
  check_species_traits(species_traits)
  check_species_categories(species_categories)
  

  # Copy datasets in path/funbiogeo/data/ --------------------------------------

  filename <- file.path(path_data, "fb_site_species.rds")

  if (file.exists(filename) && !overwrite) {
    stop("The file '", filename, "' already exists. If you want to replace ", 
         "it, use 'overwrite = TRUE'.", call. = FALSE)
  }

  saveRDS(
    object = site_species, 
    file   = filename
  )


  filename <- file.path(path_data, "fb_site_locations.rds")

  if (file.exists(filename) && !overwrite) {
    stop("The file '", filename, "' already exists. If you want to replace ", 
         "it, use 'overwrite = TRUE'.", call. = FALSE)
  }

  saveRDS(
    object = site_locations, 
    file   = filename
  )


  filename <- file.path(path_data, "fb_species_traits.rds")

  if (file.exists(filename) && !overwrite) {
    stop("The file '", filename, "' already exists. If you want to replace ", 
         "it, use 'overwrite = TRUE'.", call. = FALSE)
  }

  saveRDS(
    object = species_traits, 
    file   = filename
  )

  if (!is.null(species_categories)) {

    filename <- file.path(path_data, "fb_species_categories.rds")

    if (file.exists(filename) && !overwrite) {
      stop("The file '", filename, "' already exists. If you want to replace ", 
      "it, use 'overwrite = TRUE'.", call. = FALSE)
    }

    saveRDS(
      object = species_categories, 
      file   = filename
    )
  }

  
  # Copy report template in path/funbiogeo/ ------------------------------------
  
  invisible(
    file.copy(
      system.file(
        file.path("templates", "template_report.Rmd"), package = "funbiogeo"),
      path_rmd, overwrite = TRUE
    )
  )
  
  message("The file '", path_rmd, "' has been created!")
  
  
  # Replace default values (mustaches) -----------------------------------------
  
  if (!is.null(author)) {
    
    author <- paste0(author, collapse = ", ")
    
    xfun::gsub_file(path_rmd, "\"{{title}}\"", 
                    "\"{{title}}\"\nauthor: \"{{author}}\"", 
                    fixed = TRUE)
    xfun::gsub_file(path_rmd, "\"{{author}}\"", paste0("\"", author, "\""), 
                    fixed = TRUE)
  }
  
  xfun::gsub_file(path_rmd, "\"{{title}}\"", paste0("\"", title, "\""), 
                  fixed = TRUE)
  
  
  # Open file in text editor ---------------------------------------------------
  
  if (open) {
    open_file(path_rmd)
  }
  
  invisible(NULL)
}

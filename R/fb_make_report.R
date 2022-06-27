#' Create an R Markdown report
#'
#' @description 
#' Creates an R Markdown (`.Rmd`) report from a template to explore and 
#' summarize users data. User can modify this report and use the function
#' [rmarkdown::render()] to convert this `.Rmd` in different formats:
#' - HTML document (`output_format = "bookdown::html_document2"`);
#' - PDF document (`output_format = "bookdown::pdf_document2"`);
#' - WORD document (`output_format = "bookdown::word_document2"`);
#' - HTML, PDF and WORD documents (`output_format = "all"`).
#' 
#' @param path a `character` of length 1. The directory in which the `.Rmd` 
#'   file will be created. This directory must exist.
#' 
#' @param filename a `character` of length 1. The name of the `.Rmd` file to be
#'   created. If `NULL` (default) the `.Rmd` file will be named 
#'   `funbiogeo_report.Rmd`.
#'   
#' @param title a `character` of length 1. The title of the report.
#'   If `NULL` (default) the title will be `funbiogeo Report`.
#'   
#' @param author a `character` of length 1. The author(s) of the report. 
#'   If `NULL` (default) no author will be added.
#'   
#' @param species_traits_name a `character` of length 1. The **name** of the 
#'   species x traits dataset (not the object). Note that before rendering the 
#'   report this dataset must be loaded.
#'   
#' @param site_species_name a `character` of length 1. The **name** of the 
#'   sites x species dataset (not the object). Note that before rendering the 
#'   report this dataset must be loaded.
#'   
#' @param site_locations_name a `character` of length 1. The **name** of the 
#'   sites x locations dataset (not the object). Note that before rendering the 
#'   report this dataset must be loaded.
#' 
#' @param overwrite a logical. If this file is already present and 
#'   `overwrite = TRUE`, it will be erased and replaced by the template.
#'   Default is `FALSE`.
#' 
#' @return No return value.
#'
#' @export
#'
#' @examples
#' # Create temporary folder ----
#' temp_path <- tempdir()
#' 
#' # Load data ----
#' data("species_traits")
#' data("site_species")
#' data("site_locations")
#' 
#' # Create report ----
#' fb_make_report(path                = temp_path, 
#'                author              = "Casajus N. and Grenie M.",
#'                species_traits_name = "species_traits",
#'                site_species_name   = "site_species",
#'                site_locations_name = "site_locations")
#' 
#' \dontrun{
#' # Open Rmd file ----
#' utils::file.edit(file.path(temp_path, "funbiogeo_report.Rmd"))
#' 
#' # Render Rmd file ----
#' rmarkdown::render(file.path(temp_path, "funbiogeo_report.Rmd"), 
#'                   output_format = "all")
#' }

fb_make_report <- function(path = ".", filename = NULL, title = NULL, 
                           author = NULL, species_traits_name, 
                           site_species_name, site_locations_name,
                           overwrite = FALSE) {
  
  
  # Check path -----------------------------------------------------------------
  
  if (!dir.exists(path)) {
    stop("The path '", path, "' does not exist", call. = FALSE)
  }
  
  
  # Create file name and title ---------------------------------------------
  
  if (is.null(title) && !is.null(filename)) {
    title <- gsub("\\.Rmd$", "", filename, ignore.case = TRUE)
  }
  
  if (!is.null(title) && is.null(filename)) {
    
    filename <- gsub("[[:punct:]]|\\s", "_", title)
    filename <- gsub("_{1,}", "_", filename)
    filename <- tolower(filename)
  }
  
  if (is.null(filename)) {
    filename <- "funbiogeo_report"
  } else {
    filename <- gsub("\\.Rmd$", "", filename, ignore.case = TRUE)
  }
  
  filename <- paste0(filename, ".Rmd")
  path     <- file.path(path, filename)
  
  
  # If file exists -------------------------------------------------------------
  
  if (file.exists(path) && !overwrite) {
    stop("The file '", path, "' already exists. If you want to replace it, ", 
         "use 'overwrite = TRUE'.", call. = FALSE)
  }
  
  
  # Check data names -----------------------------------------------------------
  
  check_object_name(species_traits_name)
  check_object_name(site_species_name)
  check_object_name(site_locations_name)
  
  
  # Copy template --------------------------------------------------------------
  
  invisible(
    file.copy(
      system.file(
        file.path("templates", "template_report.Rmd"), package = "funbiogeo"),
      path, overwrite = TRUE
    )
  )
  
  message("The file ", path, "was created!\nOpen it then use knitr::knit() or rmarkdown::render() to render it.")
  # Replace default values (mustaches) -----------------------------------------
  
  if (is.null(title)) {
    title <- "funbiogeo Report"
  }
  
  if (!is.null(author)) {
    
    author <- paste0(author, collapse = ", ")
    
    xfun::gsub_file(path, "\"{{title}}\"", 
                    "\"{{title}}\"\nauthor: \"{{author}}\"", 
                    fixed = TRUE)
    xfun::gsub_file(path, "\"{{author}}\"", paste0("\"", author, "\""), 
                    fixed = TRUE)
  }
  
  xfun::gsub_file(path, "\"{{title}}\"", paste0("\"", title, "\""), 
                  fixed = TRUE)
  
  
  # Replace data names ---------------------------------------------------------
  
  xfun::gsub_file(path, "{{species_traits}}", species_traits_name, 
                  fixed = TRUE)
  
  xfun::gsub_file(path, "{{site_species}}", site_species_name, 
                  fixed = TRUE)
  
  xfun::gsub_file(path, "{{site_locations}}", site_locations_name, 
                  fixed = TRUE)
  
  
  invisible(NULL)
}

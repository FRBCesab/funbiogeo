#' Create an R Markdown report
#'
#' @description 
#' Creates an R Markdown (`.Rmd`) report from a template to explore and 
#' summarize users data. User can modify this report and use the function
#' [rmarkdown::render()] to convert this `.Rmd` in different formats:
#' - HTML document (`output_format = "html_document"`);
#' - PDF document (`output_format = "pdf_document"`);
#' - WORD document (`output_format = "word_document"`);
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
#' # Create report ----
#' fb_make_report(path = temp_path, author = "Casajus N. and Grenie M.")
#' 
#' \dontrun{
#' # Open Rmd file ----
#' utils::file.edit(file.path(temp_path, "funbiogeo_report.Rmd"))
#' }

fb_make_report <- function(path = ".", filename = NULL, title = NULL, 
                           author = NULL, overwrite = FALSE) {
  
  
  # Check path ----
  
  if (!dir.exists(path)) {
    stop("The path '", path, "' does not exist", call. = FALSE)
  }
  
  
  # Create file name and title ----
  
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
    filename <- gsub("\\.Rmd$", "", filename)
  }
  
  filename <- paste0(filename, ".Rmd")
  path     <- file.path(path, filename)
  
  
  # If file exists ----
  
  if (file.exists(path) && !overwrite) {
    stop("The file '", path, "' already exists. If you want to replace it, ", 
         "use 'overwrite = TRUE'.", call. = FALSE)
  }
  
  
  # Copy template ----
  
  invisible(
    file.copy(system.file(file.path("templates", "template_report.Rmd"), 
                          package = "funbiogeo"), path, overwrite = TRUE
  ))
  
  
  # Replace default values (mustaches) ----
  
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
  
  invisible(NULL)
}

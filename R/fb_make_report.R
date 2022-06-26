fb_make_report <- function(path = ".", filename = NULL, title = NULL, 
                           author = NULL, overwrite = FALSE) {
  
  
  # Check path ----
  
  if (!dir.exists(path)) {
    stop("The path '", path, "' does not exist", call. = FALSE)
  }
  
  
  # Create file name and title ----
  
  if (is.null(title) && !is.null(filename)) {
    title <- gsub("\\.Rmd$", "", filename)
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

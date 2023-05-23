#' Extract species x categories information from long format data
#' 
#' Convert a flat `data.frame` with species names and species (supra-)category
#' (e.g. family, order, endemism status, etc.) into a proper `data.frame` object
#' that can then be used by other functions.
#' The final output contains species in rows and two columns (species name and
#' species category).
#'
#' @param data a `data.frame` in a long format (see example).
#' 
#' @param species a `character` of length 1. Name of the column with species 
#'   names.
#' 
#' @param category a `character` of length 1. Name of the column with species 
#'   category.
#'
#' @return A `data.frame` with species in rows and two columns (species name and
#' species category).
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' filename <- system.file("extdata", "raw_mammals_data.csv", 
#'                         package = "funbiogeo")
#' all_data <- read.csv(filename)
#' 
#' head(all_data)
#' 
#' species_categories <- fb_format_species_categories(all_data, "species", 
#'                                                    "order")
#' species_categories[1:6, ]

fb_format_species_categories <- function(data, species, category) {
  
  ## Check 'data' argument -----------------------------------------------------
  
  if (missing(data)) {
    stop("Argument 'data' is required", call. = FALSE)
  }
  
  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a data.frame", call. = FALSE)
  }
  
  if (ncol(data) < 2) {
    stop("Argument 'data' must be a data.frame with at least two columns",
         call. = FALSE)
  }
  
  if (nrow(data) == 0) {
    stop("Argument 'data' must be a data.frame with at least one row",
         call. = FALSE)
  }
  
  
  ## Check 'species' column ----------------------------------------------------
  
  if (missing(species)) {
    stop("Argument 'species' is required", call. = FALSE)
  }
  
  if (!is.character(species)) {
    stop("Argument 'species' must be a character of length 1 (column name)", 
         call. = FALSE)
  }
  
  if (length(species) != 1) {
    stop("Argument 'species' must be a character of length 1 (column name)", 
         call. = FALSE)
  }
  
  if (!(species %in% colnames(data))) {
    stop(paste0("The column '", species, "' is absent from 'data'"), 
         call. = FALSE)
  }
  
  
  ## Check 'category' column ---------------------------------------------------
  
  if (missing(category)) {
    stop("Argument 'category' is required", call. = FALSE)
  }
  
  if (!is.character(category)) {
    stop("Argument 'category' must be a character of length 1 (column name)", 
         call. = FALSE)
  }
  
  if (length(category) != 1) {
    stop("Argument 'category' must be a character of length 1 (column name)", 
         call. = FALSE)
  }
  
  if (!(category %in% colnames(data))) {
    stop(paste0("The column '", category, "' is absent from 'data'"), 
         call. = FALSE)
  }
  
  
  ## Select columns ------------------------------------------------------------
  
  data <- data[ , c(species, category)]
  
  
  ## Replace non-alphanumeric characters ---------------------------------------
  
  data[ , species] <- gsub("\\s|[[:punct:]]", "_", data[ , species])
  data[ , species] <- gsub("_{1,}", "_", data[ , species])
  data[ , species] <- gsub("^_|_$", "", data[ , species])
  
  data[ , category] <- gsub("\\s|[[:punct:]]", "_", data[ , category])
  data[ , category] <- gsub("_{1,}", "_", data[ , category])
  data[ , category] <- gsub("^_|_$", "", data[ , category])
  
  
  ## Remove duplicated rows ----------------------------------------------------
  
  n_cat_per_sp <- tapply(data[ , category], data[ , species], function(x) 
    length(unique(x)))
  
  if (any(n_cat_per_sp > 1)) {
    stop("Some species have multiple categories", call. = FALSE)
  }
  
  data <- data[which(!duplicated(data[ , species])), ]
  
  
  data
}

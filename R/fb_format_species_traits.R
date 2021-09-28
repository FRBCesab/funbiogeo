#' Format species x traits object
#' 
#' @description
#' ...
#'
#' @param data a `data.frame` in a long format (see example).
#' 
#' @param species a character of length 1. Name of the column with species 
#'   names.
#' 
#' @param traits a character of length >= 1. Name(s) of trait column(s).
#'
#' @return A `data.frame` with species in rows and traits in columns.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' filename <- system.file("extdata", "raw_trees_data.csv", 
#'                         package = "funbiogeo")
#' all_data <- read.csv2(filename)
#' 
#' head(all_data)
#' 
#' species_traits <- fb_format_species_traits(all_data, "species", 
#'                                            c("sla", "n_mass", "wood_dens"))
#' species_traits

fb_format_species_traits <- function(data, species, traits) {
  
  ## Check inputs ----
  
  if (missing(data)) {
    stop("Argument 'data' is required", call. = FALSE)
  }
  
  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a data.frame", call. = FALSE)
  }
  
  if (ncol(data) == 0) {
    stop("Argument 'data' must be a data.frame with at least one column",
         call. = FALSE)
  }
  
  if (nrow(data) == 0) {
    stop("Argument 'data' must be a data.frame with at least one row",
         call. = FALSE)
  }
  
  
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
  
  
  if (missing(traits)) {
    stop("Argument 'traits' is required", call. = FALSE)
  }
  
  if (!is.character(traits)) {
    stop("Argument 'traits' must be a character of length >= 1 (column names)", 
         call. = FALSE)
  }
  
  if (any(!(traits %in% colnames(data)))) {
    stop("Some traits columns are absent from 'data'", call. = FALSE)
  }

  
  ## Select columns ----
  
  data <- data[ , c(species, traits)]
  
  
  ## Replace non-alphanumeric characters ----

  data[ , species] <- gsub("\\s|[[:punct:]]", "_", data[ , species])
  data[ , species] <- gsub("_{1,}", "_", data[ , species])
  data[ , species] <- gsub("^_|_$", "", data[ , species])
  
  
  ## Get unique traits values per species ----
  
  traits_values <- list()
  
  for (trait in traits) {
    
    traits_values[[trait]] <- tapply(data[ , trait], data[ , species], 
                                     function(x) unique(x))
    
    if (length(unique(unlist(lapply(traits_values[[trait]], length)))) > 1) {
      stop("Some species have non-unique trait values", call. = FALSE)
    }
  }
  
  data.frame(traits_values)
}

#' Format species x traits Object from Long Data
#' 
#' @description
#' ...
#'
#' @param data_long a `data.frame` in a long format (see example).
#' 
#' @param species a character of length 1. Name of the column with species 
#'   names.
#' 
#' @param traits a character of length >= 1. Name(s) of trait column(s).
#'
#' @return A `data.frame` with species in rows and traits in columns, with the
#'   first column names `"species"` containing the species names
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
#' traits <- c("adult_body_mass", "gestation_length", "litter_size", 
#'             "max_longevity", "sexual_maturity_age", "diet_breadth")
#' 
#' species_traits <- fb_format_species_traits(all_data, "species", traits)
#' head(species_traits)

fb_format_species_traits <- function(data_long, species, traits) {
  
  ## Check inputs ----
  
  if (missing(data_long)) {
    stop("Argument 'data_long' is required", call. = FALSE)
  }
  
  if (!is.data.frame(data_long)) {
    stop("Argument 'data_long' must be a data.frame", call. = FALSE)
  }
  
  if (ncol(data_long) == 0) {
    stop("Argument 'data_long' must be a data.frame with at least one column",
         call. = FALSE)
  }
  
  if (nrow(data_long) == 0) {
    stop("Argument 'data_long' must be a data.frame with at least one row",
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
  
  if (!(species %in% colnames(data_long))) {
    stop(paste0("The column '", species, "' is absent from 'data_long'"), 
         call. = FALSE)
  }
  
  
  if (missing(traits)) {
    stop("Argument 'traits' is required", call. = FALSE)
  }
  
  if (!is.character(traits)) {
    stop("Argument 'traits' must be a character of length >= 1 (column names)", 
         call. = FALSE)
  }
  
  if (any(!(traits %in% colnames(data_long)))) {
    stop("Some traits columns are absent from 'data_long'", call. = FALSE)
  }

  
  ## Select columns ----
  
  data_long <- data_long[ , c(species, traits)]
  
  
  ## Replace non-alphanumeric characters ----

  data_long[ , species] <- gsub("\\s|[[:punct:]]", "_", data_long[ , species])
  data_long[ , species] <- gsub("_{1,}", "_", data_long[ , species])
  data_long[ , species] <- gsub("^_|_$", "", data_long[ , species])
  
  
  ## Get unique traits values per species ----
  
  trait_values <- vector("list", length(traits))
  names(trait_values) <- traits
  
  for (trait in traits) {
    
    trait_values[[trait]] <- tapply(data_long[ , trait], data_long[ , species], 
                                     function(x) unique(x))
    
    if (length(unique(unlist(lapply(trait_values[[trait]], length)))) > 1) {
      stop("Some species have non-unique trait values", call. = FALSE)
    }
  }
  
  trait_values_df <- data.frame(trait_values)
  
  # Add the 'species' column
  trait_values_df[["species"]] <- row.names(trait_values_df)
  row.names(trait_values_df) <- NULL
  
  # Reorder columns to have species column first
  trait_values_df[, c(ncol(trait_values_df), seq(ncol(trait_values_df) - 1))]
}

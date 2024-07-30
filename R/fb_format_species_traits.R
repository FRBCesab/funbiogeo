#' Extract species x traits information from long format data
#' 
#' Convert a flat `data.frame` with traits values for different species
#' into a proper `data.frame` object that can then be used by other functions.
#' The final output contains species in rows and traits in columns.
#'
#' @param data a `data.frame` in a long format (see example).
#' 
#' @param species a `character` of length 1. Name of the column with species 
#'   names.
#' 
#' @param traits a `character` of length >= 1. Name(s) of trait column(s).
#'
#' @return A `data.frame` with species in rows and traits in columns, with the
#'   first column names `"species"` containing the species names.
#' 
#' @export
#'
#' @examples
#' filename <- system.file(
#'   "extdata", "funbiogeo_raw_data.csv", package = "funbiogeo"
#' )
#' all_data <- read.csv(filename)
#' 
#' head(all_data)
#' 
#' traits <- c("plant_height", "seed_mass", "sla", "wood_density")
#' 
#' species_traits <- fb_format_species_traits(all_data, "species", traits)
#' head(species_traits)

fb_format_species_traits <- function(data, species, traits) {
  
  ## Check 'data' argument -----------------------------------------------------
  
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
  
  
  ## Check 'traits' argument ---------------------------------------------------
  
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

  
  ## Select columns ------------------------------------------------------------
  
  data <- data[ , c(species, traits)]
  
  
  ## Replace non-alphanumeric characters ---------------------------------------

  data[ , species] <- gsub("\\s|[[:punct:]]", "_", data[ , species])
  data[ , species] <- gsub("_{1,}", "_", data[ , species])
  data[ , species] <- gsub("^_|_$", "", data[ , species])
  
  
  ## Get unique traits values per species --------------------------------------
  
  trait_values <- vector("list", length(traits))
  names(trait_values) <- traits
  
  for (trait in traits) {
    
    trait_values[[trait]] <- tapply(data[ , trait], data[ , species], 
                                    function(x) unique(x))
    
    if (length(unique(unlist(lapply(trait_values[[trait]], length)))) > 1) {
      stop("Some species have non-unique trait values", call. = FALSE)
    }
  }
  
  trait_values_df <- data.frame(trait_values)
  
  
  ## Add the 'species' column --------------------------------------------------
  
  trait_values_df[["species"]] <- row.names(trait_values_df)
  row.names(trait_values_df) <- NULL
  
  
  ## Reorder columns to have species column first ------------------------------
  
  trait_values_df[ , c(ncol(trait_values_df), seq(ncol(trait_values_df) - 1))]
}

#' Extract site x species information from long format data
#' 
#' Convert a flat `data.frame` with species occurrence/abundance at site level 
#' into a proper `data.frame` object that can then be used by other functions.
#' The final output contains sites in rows and species in columns.
#'
#' @param data a `data.frame` in a long format (see example).
#' 
#' @param site a `character` of length 1. Name of the column with site labels.
#' 
#' @param species a `character` of length 1. Name of the column with species 
#'   names.
#' 
#' @param value a `character` of length 1. Name of the column with species 
#'   occurrence/abundance.
#'   
#' @param na_to_zero a logical value. If `TRUE` (default) `NA` are replaced by 
#'   `0`.
#'
#' @return A `data.frame` with sites in rows and species in columns. The first
#'   column is named `"site"` and contains the name of the sites.
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
#' site_species <- fb_format_site_species(all_data, "site", "species", "count")
#' site_species[1:3, 1:4]

fb_format_site_species <- function(data, site, species, value, 
                                    na_to_zero = TRUE) {
  
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
  
  
  ## Check 'site' column -------------------------------------------------------
  
  if (missing(site)) {
    stop("Argument 'site' is required", call. = FALSE)
  }
  
  if (!is.character(site)) {
    stop("Argument 'site' must be a character of length 1 (column name)", 
         call. = FALSE)
  }
  
  if (length(site) != 1) {
    stop("Argument 'site' must be a character of length 1 (column name)", 
         call. = FALSE)
  }

  if (!(site %in% colnames(data))) {
    stop(paste0("The column '", site, "' is absent from 'data'"), 
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
  
  
  ## Check 'value' column ------------------------------------------------------
  
  if (missing(value)) {
    stop("Argument 'value' is required", call. = FALSE)
  }
  
  if (!is.character(value)) {
    stop("Argument 'value' must be a character of length 1 (column name)", 
         call. = FALSE)
  }
  
  if (length(value) != 1) {
    stop("Argument 'value' must be a character of length 1 (column name)", 
         call. = FALSE)
  }
  
  if (!(value %in% colnames(data))) {
    stop(paste0("The column '", value, "' is absent from 'data'"), 
         call. = FALSE)
  }
  
  if (!is.numeric(data[ , value])) {
    stop(paste0("The column '", value, "' is must be a numeric"), 
         call. = FALSE)
  }
  
  
  ## Check 'na_to_zero' column -------------------------------------------------
  
  if (!is.logical(na_to_zero)) {
    stop("Argument 'na_to_zero' must be TRUE or FALSE", call. = FALSE)
  }

  
  ## Select columns ------------------------------------------------------------
  
  data <- data[ , c(site, species, value)]
  
  
  ## Replace non-alphanumeric characters ---------------------------------------
  
  data[ , site] <- gsub("\\s|[[:punct:]]", "_", data[ , site])
  data[ , site] <- gsub("_{1,}", "_", data[ , site])
  data[ , site] <- gsub("^_|_$", "", data[ , site])
  
  data[ , species] <- gsub("\\s|[[:punct:]]", "_", data[ , species])
  data[ , species] <- gsub("_{1,}", "_", data[ , species])
  data[ , species] <- gsub("^_|_$", "", data[ , species])
  
  
  ## From long to wider format -------------------------------------------------
  
  data_wider <- tidyr::pivot_wider(data, 
                                   names_from  = tidyselect::all_of(species), 
                                   values_from = tidyselect::all_of(value))
  
  data_wider <- as.data.frame(data_wider)
  
  
  ## Replace NA by 0 -----------------------------------------------------------
  
  if (na_to_zero) {
    
    # Make a list of columns where to replace NA values by 0
    
    col_list   <- colnames(data_wider)[-1]
    vect_value <- as.list(rep_len(0, length(col_list)))
    
    names(vect_value) <- col_list
    
    data_wider <- tidyr::replace_na(data_wider, vect_value)
  }
  
  data_wider
}

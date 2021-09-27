#' Format sites x species object
#' 
#' @description
#' ...
#'
#' @param data a `data.frame` in a long format (see example).
#' 
#' @param site a character of length 1. Name of the column with site labels.
#' 
#' @param species a character of length 1. Name of the column with species 
#'   names.
#' 
#' @param value a character of length 1. Name of the column with species 
#'   occurrence/abundance.
#'   
#' @param na_to_zero a logical value. If `TRUE` (default) NA are replaced by 
#'   `0`.
#'
#' @return A matrix with sites in rows and species in columns.
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
#' sites_species <- fb_format_sites_species(all_data, "site", "species", 
#'                                          "count")
#' head(sites_species)

fb_format_sites_species <- function(data, site, species, value, 
                                    na_to_zero = TRUE) {
  
  ## Check inputs ----
  
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
  
  if (is.na(site)) {
    stop("Argument 'site' must be a character of length 1 (column name)", 
         call. = FALSE)
  }

  if (!(site %in% colnames(data))) {
    stop(paste0("The column '", site, "' is absent from 'data'"), 
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
  
  if (is.na(species)) {
    stop("Argument 'species' must be a character of length 1 (column name)", 
         call. = FALSE)
  }
  
  if (!(species %in% colnames(data))) {
    stop(paste0("The column '", species, "' is absent from 'data'"), 
         call. = FALSE)
  }
  
  
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
  
  if (is.na(value)) {
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
  
  
  if (!is.logical(na_to_zero)) {
    stop("Argument 'na_to_zero' must be TRUE or FALSE", call. = FALSE)
  }
  
  if (length(na_to_zero) != 1) {
    stop("Argument 'na_to_zero' must be TRUE or FALSE", call. = FALSE)
  }

  
  ## Select columns ----
  
  data <- data[ , c(site, species, value)]
  
  
  ## Replace non-alphanumeric characters ----
  
  data[ , site] <- gsub("\\s|[[:punct:]]", "_", data[ , site])
  data[ , site] <- gsub("_{1,}", "_", data[ , site])
  data[ , site] <- gsub("^_|_$", "", data[ , site])
  
  data[ , species] <- gsub("\\s|[[:punct:]]", "_", data[ , species])
  data[ , species] <- gsub("_{1,}", "_", data[ , species])
  data[ , species] <- gsub("^_|_$", "", data[ , species])
  
  
  ## From long to wider format ----
  
  data_wider <- tidyr::pivot_wider(data, 
                                   names_from  = tidyselect::all_of(species), 
                                   values_from = tidyselect::all_of(value))
  
  
  ## Cleanup ----
  
  data_wider <- as.data.frame(data_wider)
  rownames(data_wider) <- data_wider[ , site]
  
  data_wider <- data_wider[ , -1]
  
  
  ## Convert to matrix ----
  
  data_wider <- data.matrix(data_wider, rownames.force = TRUE)
  
  
  ## Replace NA by 0 ----
  
  if (na_to_zero) {
    data_wider <- ifelse(is.na(data_wider), 0, data_wider)
  }
  
  data_wider
}

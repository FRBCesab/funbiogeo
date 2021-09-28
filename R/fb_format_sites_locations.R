#' Format sites x locations object
#' 
#' @description
#' ...
#'
#' @param data a `data.frame` in a long format (see example).
#' 
#' @param site a character of length 1. Name of the column with site labels.
#' 
#' @param longitude a character of length 1. Name of the column with longitude. 
#' 
#' @param latitude a character of length 1. Name of the column with latitude. 
#'   
#' @param na_rm a logical value. If `TRUE` remove sites with incomplete 
#'   coordinates. Default is `FALSE`.
#'
#' @return A matrix with sites in rows and longitude and latitude in columns.
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
#' sites_locations <- fb_format_sites_locations(all_data, "site", "longitude", 
#'                                              "latitude")
#' head(sites_locations)

fb_format_sites_locations <- function(data, site, longitude, latitude, 
                                      na_rm = FALSE) {
  
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
  
  if (!(site %in% colnames(data))) {
    stop(paste0("The column '", site, "' is absent from 'data'"), 
         call. = FALSE)
  }
  
  
  if (missing(longitude)) {
    stop("Argument 'longitude' is required", call. = FALSE)
  }
  
  if (!is.character(longitude)) {
    stop("Argument 'longitude' must be a character of length 1 (column name)", 
         call. = FALSE)
  }
  
  if (length(longitude) != 1) {
    stop("Argument 'longitude' must be a character of length 1 (column name)", 
         call. = FALSE)
  }
  
  if (!(longitude %in% colnames(data))) {
    stop(paste0("The column '", longitude, "' is absent from 'data'"), 
         call. = FALSE)
  }
  
  if (!is.numeric(data[ , longitude])) {
    stop(paste0("The column '", longitude, "' is must be a numeric"), 
         call. = FALSE)
  }
  
  
  if (missing(latitude)) {
    stop("Argument 'latitude' is required", call. = FALSE)
  }
  
  if (!is.character(latitude)) {
    stop("Argument 'latitude' must be a character of length 1 (column name)", 
         call. = FALSE)
  }
  
  if (length(latitude) != 1) {
    stop("Argument 'latitude' must be a character of length 1 (column name)", 
         call. = FALSE)
  }
  
  if (!(latitude %in% colnames(data))) {
    stop(paste0("The column '", latitude, "' is absent from 'data'"), 
         call. = FALSE)
  }
  
  if (!is.numeric(data[ , latitude])) {
    stop(paste0("The column '", latitude, "' is must be a numeric"), 
         call. = FALSE)
  }
  
  
  if (!is.logical(na_rm)) {
    stop("Argument 'na_rm' must be TRUE or FALSE", call. = FALSE)
  }
  
  
  ## Select columns ----
  
  data <- data[ , c(site, longitude, latitude)]
  
  
  ## Replace non-alphanumeric characters ----
  
  data[ , site] <- gsub("\\s|[[:punct:]]", "_", data[ , site])
  data[ , site] <- gsub("_{1,}", "_", data[ , site])
  data[ , site] <- gsub("^_|_$", "", data[ , site])
  
  
  ## Get unique coordinates per site ----
  
  x_coord <- tapply(data[ , longitude], data[ , site], function(x) unique(x))
  
  if (length(unique(unlist(lapply(x_coord, length)))) > 1) {
    stop("Some sites have non-unique longitude", call. = FALSE)
  }
  
  y_coord <- tapply(data[ , latitude], data[ , site], function(x) unique(x))
  if (length(unique(unlist(lapply(y_coord, length)))) > 1) {
    stop("Some sites have non-unique latitude", call. = FALSE)
  }
  
  data <- data.frame("longitude" = x_coord, "latitude" = y_coord)
  
  rownames(data) <- names(x_coord)
  
  
  ## Remove sites with NA ----
  
  if (na_rm) {
    data <- data[!is.na(data[ , longitude]), ]
    data <- data[!is.na(data[ , latitude]), ]
  }
  
  
  ## Convert to matrix ----
  
  data.matrix(data, rownames.force = TRUE)
}

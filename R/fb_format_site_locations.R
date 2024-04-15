#' Extract site x locations information from long format data
#' 
#' Convert a flat `data.frame` with site coordinates into a proper `sf` object
#' that can then be used by other functions. This function assumes that
#' the coordinates are given in WGS84 (longitude vs. latitude). The function
#' automatically removes repeated coordinates from the input dataset.
#'
#' @param data a `data.frame` in a long format (see example).
#' 
#' @param site a `character` of length 1. Name of the column with site labels.
#' 
#' @param longitude a `character` of length 1. Name of the column with
#'   longitude. The function assumes coordinates are WGS84 (EPSG:4326). 
#' 
#' @param latitude a `character` of length 1. Name of the column with latitude.
#'   The function assumes coordinates are WGS84 (EPSG:4326).
#' 
#' @param crs a `character` of length 1 or an object of class `crs`.
#'   Coordinate Reference System (CRS) of the specified coordinates.
#'   The CRS should be a [valid CRS in R](
#'   https://geocompr.robinlovelace.net/spatial-class.html?q=CRS#crs-in-r).
#'   It can either be a character like `"+proj=longlat +datum=WGS84 +no_defs"`
#'   or as specified using `sf::st_crs()` like `sf::st_crs(4326)`
#'   the default value.
#'   
#' @param na_rm a logical value. If `TRUE` remove sites with incomplete 
#'   coordinates. Default is `FALSE`.
#'
#' @return An `sf` object with a `site` column specifying site coordinates.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' filename <- system.file("extdata", "funbiogeo_raw_data.csv", 
#'                         package = "funbiogeo")
#' all_data <- read.csv(filename)
#' 
#' head(all_data)
#' 
#' site_locations <- fb_format_site_locations(all_data, "site", "longitude", 
#'                                            "latitude")
#' head(site_locations)

fb_format_site_locations <- function(
  data, site, longitude, latitude,
  crs = sf::st_crs(4326), na_rm = FALSE
) {
  
  ## Check 'data' --------------------------------------------------------
  
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
  
  
  ## Check coordinates columns -------------------------------------------------
  
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
  
  
  ## Check 'na_rm' argument ----------------------------------------------------
  
  if (!is.logical(na_rm)) {
    stop("Argument 'na_rm' must be TRUE or FALSE", call. = FALSE)
  }
  
  
  ## Check provided CRS --------------------------------------------------------
  
  if (is.character(crs) | !inherits(crs, "crs")) {
    # Try to coerce provided argument into a CRS, specific error otherwise
    crs <- tryCatch(
      suppressWarnings(sf::st_crs(crs)),
      error = function(e) stop(
        "Argument 'crs' should be valid CRS or coercible to one ",
        "with sf::st_crs()"
      )
    )
    
    # Catch other potentially invalid CRSs
    if (is.na(crs$input)) {
      stop(
        "Argument 'crs' should be valid CRS or coercible to one ",
        "with sf::st_crs()"
      )
    }
  }
  
  
  ## Select columns ------------------------------------------------------------
  
  data <- data[ , c(site, longitude, latitude)]
  
  
  ## Replace non-alphanumeric characters ---------------------------------------
  
  data[ , site] <- gsub("\\s|[[:punct:]]", "_", data[ , site])
  data[ , site] <- gsub("_{1,}", "_",           data[ , site])
  data[ , site] <- gsub("^_|_$", "",            data[ , site])
  
  
  ## Remove sites with NA ------------------------------------------------------
  
  if (na_rm) {
    data <- data[!is.na(data[ , longitude]), ]
    data <- data[!is.na(data[ , latitude]), ]
  }
  
  
  ## Keep unique sites only ----------------------------------------------------
  
  data <- data[!duplicated(data), ]
  
  
  ## Convert to 'sf' object ----------------------------------------------------
  
  sf::st_as_sf(data, coords = c(latitude, longitude), crs = crs)
}

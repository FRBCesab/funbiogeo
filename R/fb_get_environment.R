#' Extract average raster values at sites locations
#'
#' @param site_locations an `sf` object with each sites defined as points
#' @param environment_raster a `raster` object of environmental values
#'
#' @return a `data.frame` with average environmental values per site
#' @export
fb_get_environment = function(site_locations, environment_raster) {
  
  if (!requireNamespace("sf") | !requireNamespace("terra")) {
    stop("Packages 'sf' and 'terra' should be installed to use this function",
         call. = FALSE)
  }
  
  if (missing(site_locations)) {
    stop("Argument 'site_locations' (sites x location object) is required",
         call. = FALSE)
  }
  
  if (missing(environment_raster)) {
    stop("Argument 'environment_raster' (environmental raster) is required",
         call. = FALSE)
  }
  
  check_site_locations(site_locations)
  
  terra::extract(
    environment_raster, terra::vect(site_locations), fun = mean, na.rm = TRUE,
    df = TRUE
  )
}
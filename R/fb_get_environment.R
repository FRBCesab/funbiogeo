#' Extract average raster values at sites locations
#'
#' @param sites_locations an `sf` object with each sites defined as points
#' 
#' @param environment_raster a `raster` object of environmental values
#'
#' @return A `data.frame` with average environmental values (columns) per site
#' (rows).
#' 
#' @export
#' 
#' @examples
#' ## Add an example ----

fb_get_environment <- function(sites_locations, environment_raster) {
  
  if (!requireNamespace("sf") | !requireNamespace("terra")) {
    stop("Packages 'sf' and 'terra' should be installed to use this function",
         call. = FALSE)
  }
  
  if (missing(sites_locations)) {
    stop("Argument 'sites_locations' (sites x locations object) is required",
         call. = FALSE)
  }
  
  if (missing(environment_raster)) {
    stop("Argument 'environment_raster' (environmental raster) is required",
         call. = FALSE)
  }
  
  check_sites_locations(sites_locations)
  
  terra::extract(
    environment_raster, terra::vect(sites_locations), fun = mean, na.rm = TRUE,
    df = TRUE
  )
}

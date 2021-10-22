#' Extract average raster values at sites locations
#'
#' @param site_locations an `sf` object with all sites.
#' 
#' @param environment_raster a `SpatRaster` object (package `terra`).
#'   A single or multi-layers environmental raster.
#'
#' @return A `data.frame` with average environmental values (columns) per site
#' (rows).
#' 
#' @export
#' 
#' @examples
#' library("funbiogeo")
#' 
#' data("site_locs")
#' 
#' ## Import climate rasters ----
#' prec <- system.file("extdata", "annual_tot_prec.tif", package = "funbiogeo")
#' tavg <- system.file("extdata", "annual_mean_temp.tif", package = "funbiogeo")
#' 
#' layers <- terra::rast(c(tavg, prec))
#' 
# FIXME (finish example)

fb_get_environment <- function(site_locations, environment_raster) {
  
  
  if (missing(site_locations)) {
    stop("Argument 'site_locations' (site x locations 'sf' object) ",
         "is required", call. = FALSE)
  }
  
  if (missing(environment_raster)) {
    stop("Argument 'environment_raster' (environmental raster) is required",
         call. = FALSE)
  }
  
  check_site_locations(site_locations)
  
  if (!inherits(environment_raster, "SpatRaster")) {
    stop("The raster layer must be a 'SpatRaster' object (package `terra`)", 
         call. = FALSE)
  }
  
  ## Reproject sites if needed -------------------------------------------------
  
  if (sf::st_crs(site_locations) !=
      terra::crs(environment_raster, proj = TRUE)) {
    
    site_locations <- sf::st_transform(
      site_locations,
      terra::crs(environment_raster, proj = TRUE)
    )
  }
  
  
  ## Extract values ------------------------------------------------------------
  
  env_values <- terra::extract(x = environment_raster, 
                               y = terra::vect(site_locations), 
                               fun = mean, na.rm = TRUE, df = TRUE)
  
  
  ## Add sites ID --------------------------------------------------------------
  
  colnames(env_values)[1] <- "site"
  
  env_values[["site"]] <- site_locations[["site"]]
  
  return(env_values)
}

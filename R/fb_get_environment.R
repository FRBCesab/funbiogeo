#' Extract Raster Values at Location of Sites
#'
#' @param site_locations an `sf` object with the spatial geometries of sites.
#'                       **NOTE**: the first column should be named **`"site"`**
#'                       and indicate site names.
#' 
#' @param environment_raster a `SpatRaster` object (package `terra`).
#'   A single or multi-layers environmental raster.
#'
#' @return A `data.frame` with average environmental values (columns) per site
#' (rows), with the first column being `"site"` indicating site names.
#' 
#' @export
#' 
#' @examples
#' library("funbiogeo")
#' 
#' data("site_locations")
#' 
#' ## Import climate rasters ----
#' prec <- system.file("extdata", "annual_tot_prec.tif", package = "funbiogeo")
#' tavg <- system.file("extdata", "annual_mean_temp.tif", package = "funbiogeo")
#' 
#' layers <- terra::rast(c(tavg, prec))
#' 
#' fb_get_environment(head(site_locations), layers)

fb_get_environment <- function(site_locations, environment_raster) {
  

  # Check inputs  
  check_site_locations(site_locations)
  
  if (missing(environment_raster)) {
    stop("Argument 'environment_raster' (environmental raster) is required",
         call. = FALSE)
  }
  
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

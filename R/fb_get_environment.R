#' Extract average raster values at sites locations
#'
#' @param sites_locations a `matrix` or `data.frame` with sites in rows and two
#'   columns: longitude and latitude. The first column must be the longitude.
#' 
#' @param environment_raster a `SpatRaster` object (package `terra`). A raster
#'   of one or several environmental layers.
#'   
#' @param crs a character of length 1 specifying the Coordinate Reference 
#'   System in the PROJ4 standard.
#'   Default is: `'+proj=longlat +datum=WGS84 +no_defs'`.
#'
#' @return A `data.frame` with average environmental values (columns) per site
#' (rows).
#' 
#' @export
#' 
#' @examples
#' library("funbiogeo")
#' 
#' data("sites_locs")
#' 
#' ## Import climate rasters ----
#' prec <- system.file("extdata", "annual_tot_prec.tif", package = "funbiogeo")
#' tavg <- system.file("extdata", "annual_mean_temp.tif", package = "funbiogeo")
#' 
#' layers <- terra::rast(c(tavg, prec))
#' 
#' ## Extract environment at sites ----
#' sites_env <- fb_get_environment(sites_locs, layers)
#' head(sites_env)

fb_get_environment <- function(sites_locations, environment_raster,
                               crs = "+proj=longlat +datum=WGS84 +no_defs") {
  
  
  if (missing(sites_locations)) {
    stop("Argument 'sites_locations' (sites x locations matrix) is required")
  }
  
  if (missing(environment_raster)) {
    stop("Argument 'environment_raster' (environmental rasters) is required",
         call. = FALSE)
  }
  
  check_sites_locations(sites_locations)
  
  if (!inherits(environment_raster, "SpatRaster")) {
    stop("The raster layer must be a 'SpatRaster' object (package terra)", 
         call. = FALSE)
  }
  
  if (!is.character(crs)) {
    stop("Argument 'crs' (coordinate system) must a character of length 1")
  }
  
  
  ## Convert sites x locations to sf ----
  
  sites_locations_sf <- sf::st_as_sf(sites_locations, coords = 1:2)
  sites_locations_sf <- sf::st_set_crs(sites_locations_sf, crs)
  
  
  ## Project if required ----
  
  if (crs != terra::crs(environment_raster, proj = TRUE)) {
    
    sites_locations_sf <- sf::st_transform(sites_locations_sf, 
                                           terra::crs(environment_raster, 
                                                      proj = TRUE))
  }
  
  
  ## Extract values ----
  
  env_values <- terra::extract(x = environment_raster, 
                               y = terra::vect(sites_locations_sf), 
                               fun = mean, na.rm = TRUE, df = TRUE)
  
  
  ## Add sites ID ----
  
  colnames(env_values)[1] <- "site"
  
  env_values$"site" <- rownames(sites_locations)
  
  env_values
}

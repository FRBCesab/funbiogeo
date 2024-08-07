#' Aggregate Site Data Along Coarser Spatial Grid
#'
#' This function helps aggregating site data along a coarser grid.
#' The idea is that you have any type of data at the site scale
#' (diversity metrics, environmental data, etc.) but you would like to work
#' on it at a coarser scale, or you want to visualize it at that scale.
#' This function helps you do exactly that.
#' 
#' @inheritParams fb_get_environment

#' @param site_data a `matrix` or `data.frame` containing values per sites to 
#'   aggregate along the provided grid. Can have one or several columns
#'   (variables to aggregate). The first column must contain sites names as
#'   provided in the example dataset `site_locations`.
#'
#' @param agg_grid a `SpatRaster` object (package `terra`).
#'   A raster of one single layer, that defines the grid along which
#'   to aggregate.
#'   
#' @param fun the function used to aggregate points values when there are 
#'   multiple points in one cell. Default is `mean`.
#'   
#' @return A `SpatRaster` object with as many layers as columns in `site_data`.
#' 
#' @import sf
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("site_locations")
#' data("site_species")
#' 
#' ## Import grid ----
#' tavg <- system.file("extdata", "annual_mean_temp.tif", package = "funbiogeo")
#' tavg <- terra::rast(tavg)
#' 
#' ## Rasterize 3 first species counts ----
#' fb_aggregate_site_data(site_locations, site_species[, 1:4], tavg, fun = sum)

fb_aggregate_site_data <- function(site_locations, site_data, agg_grid,
                                   fun = mean) {
  
  # Check inputs ---------------------------------------------------------------
  
  check_site_locations(site_locations)
  
  if (missing(site_data)) {
    stop("Argument 'site_data' is required",
         call. = FALSE)
  }
  
  if (!is.data.frame(site_data) & !is.matrix(site_data)) {
    stop("Argument 'site_data' must be a matrix or a data.frame", 
         call. = FALSE)
  }
  
  if (0 %in% dim(site_data)) {
    stop("Argument 'site_data' should have at least one row and one column", 
         call. = FALSE)
  }
  
  if (missing(agg_grid)) {
    stop("Argument 'agg_grid' is required", call. = FALSE)
  }
  
  if (!inherits(agg_grid, "SpatRaster")) {
    stop("The 'agg_grid' raster must be a 'SpatRaster' object ",
         "(package `terra`)", call. = FALSE)
  }
  
  if (is.na(terra::crs(agg_grid, proj = TRUE)) | 
      terra::crs(agg_grid, proj = TRUE) == "") {
    stop("The 'agg_grid' raster must have a CRS (coordinate system)", 
         call. = FALSE)
  }
  
  
  # Get proper aggregation grid ------------------------------------------------
  
  agg_grid <- terra::subset(agg_grid, 1)
  
  
  # Merge sites info -----------------------------------------------------------
  
  site_locations <- merge(site_locations, site_data, by = "site")
  
  
  # Reproject sites if required ------------------------------------------------
  
  if (sf::st_crs(site_locations) !=
      sf::st_crs(terra::crs(agg_grid, proj = TRUE))) {
    
    site_locations <- sf::st_transform(
      site_locations,
      sf::st_crs(terra::crs(agg_grid, proj = TRUE))
    )
  }
  
  
  # Rasterize data -------------------------------------------------------------
  
  fields <- colnames(sf::st_drop_geometry(site_locations))[-1]
  
  rasters <- lapply(seq_along(fields), function(x) {
    terra::rasterize(terra::vect(site_locations), agg_grid,
                     field = fields[x], 
                     fun = fun)
  })
  
  rasters <- terra::rast(rasters)
  names(rasters) <- fields
  
  rasters
}

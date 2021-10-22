#' Aggregate site data along coarser grid
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
#'   (variables to aggregate). Row names must  contain sites names as provided
#'   as `site_locations`.
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
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("site_locs")
#' data("species_occs")
#' 
#' ## Import grid ----
#' tavg <- system.file("extdata", "annual_mean_temp.tif", package = "funbiogeo")
#' tavg <- terra::rast(tavg)
#' 
#' ## Rasterize 3 first species counts ----
# FIXME (finish examples)
fb_aggregate_site_data <- function(site_locations, site_data, agg_grid,
                                   fun = mean) {
  
  # Check inputs ---------------------------------------------------------------
  
  if (missing(site_locations)) {
    stop("Argument 'site_locations' (site x locations matrix) is required",
         call. = FALSE)
  }
  
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
  
  if (is.matrix(site_data)) {
    
    if (is.null(rownames(site_data))) {
      stop("Argument 'site_data' must have row names (sites names)", 
           call. = FALSE)
    }
  }
  
  if (is.data.frame(site_data)) {
    
    if (any(rownames(site_data) %in% seq_len(nrow(site_data)))) {
      stop("Argument 'site_data' must have row names (sites names)", 
           call. = FALSE)
    }
  }
  
  if (!is.numeric(as.matrix(site_data))) {
    stop("Argument 'site_data' must contain only numeric values. Sites ", 
         "names must be provided as row names", call. = FALSE)
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
  
  site_locations <- merge(site_locations, site_data, by.x = "site",
                           by.y = "row.names")
  
  
  # Reproject sites if required ------------------------------------------------
  
  if (sf::st_crs(site_locations) != terra::crs(agg_grid, proj = TRUE)) {
    
    site_locations <- sf::st_transform(site_locations, 
                                           terra::crs(agg_grid, proj = TRUE))
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

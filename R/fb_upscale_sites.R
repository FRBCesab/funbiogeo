#' Upscale sites
#'
#' @param data a `matrix` or `data.frame` containing values to rasterize. Can
#'   have one or several columns (variables to rasterize). Row names must 
#'   contain sites names as in the argument `sites_locations`.
#'
#' @param grid a `SpatRaster` object (package `terra`). A raster of one single 
#'   layer.
#'   
#' @param fun the function used to aggregate points values when there are 
#'   multiple points in one cell. Default is `mean`.
#'   
#' @inheritParams fb_get_environment
#'
#' @return A `SpatRaster` object with `n` layers where `n` represents the number
#'   of columns in `data`.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("sites_locs")
#' data("species_occs")
#' 
#' ## Import grid ----
#' tavg <- system.file("extdata", "annual_mean_temp.tif", package = "funbiogeo")
#' tavg <- terra::rast(tavg)
#' 
#' ## Rasterize 3 first species counts ----
#' rasters <- fb_upscale_sites(sites_locations = sites_locs, 
#'                             data            = species_occs[ , 1:3], 
#'                             grid            = tavg)
#'
#' ## Subset first layer ----
#' a_neg <- terra::subset(rasters, "acer_negundo")
#' 
#' ## Species maps ----
#' fb_map_raster(a_neg) + 
#'   ggplot2::scale_fill_distiller("Counts", palette = "Blues", direction = 1) +
#'   ggplot2::ggtitle("Acer negundo in Pennsylvania")

fb_upscale_sites <- function(sites_locations, data, grid, fun = mean,
                             crs = "+proj=longlat +datum=WGS84 +no_defs") {
  
  ## Check inputs ----
  
  if (missing(sites_locations)) {
    stop("Argument 'sites_locations' (sites x locations matrix) is required")
  }
  
  check_sites_locations(sites_locations)
  
  if (missing(data)) {
    stop("Argument 'data' is required")
  }
  
  if (!is.data.frame(data) & !is.matrix(data)) {
    stop("Argument 'data' must be a matrix or a data.frame", 
         call. = FALSE)
  }
  
  if (0 %in% dim(data)) {
    stop("Argument 'data' should have at least one row and one column", 
         call. = FALSE)
  }
  
  if (is.matrix(data)) {
    
    if (is.null(rownames(data))) {
      stop("Argument 'data' must have row names (sites names)", 
           call. = FALSE)
    }
  }
  
  if (is.data.frame(data)) {
    
    if (any(rownames(data) %in% seq_len(nrow(data)))) {
      stop("Argument 'data' must have row names (sites names)", 
           call. = FALSE)
    }
  }
  
  if (!is.numeric(as.matrix(data))) {
    stop("Argument 'data' must contain only numeric values. Sites ", 
         "names must be provided as row names", call. = FALSE)
  }
  
  if (missing(grid)) {
    stop("Argument 'grid' is required", call. = FALSE)
  }
  
  if (!inherits(grid, "SpatRaster")) {
    stop("The 'grid' raster must be a 'SpatRaster' object (package terra)", 
         call. = FALSE)
  }
  
  if (is.na(terra::crs(grid, proj = TRUE)) | 
      terra::crs(grid, proj = TRUE) == "") {
    stop("The 'grid' raster must have a CRS (coordinate system)", 
         call. = FALSE)
  }
  
  if (!is.character(crs)) {
    stop("Argument 'crs' (coordinate system) must be a character of length 1")
  }
  
  
  ## Subset 1st layer ----
  
  grid <- terra::subset(grid, 1)
  
  
  ## Merge sites info ----
  
  sites_locations <- merge(sites_locations, data, by = "row.names")
  sites_locations <- sites_locations[ , -1]
  
  
  ## Convert to sf ----
  
  sites_locations_sf <- sf::st_as_sf(sites_locations, coords = 1:2)
  sites_locations_sf <- sf::st_set_crs(sites_locations_sf, crs)
  
  
  ## Project if required ----
  
  if (crs != terra::crs(grid, proj = TRUE)) {
    
    sites_locations_sf <- sf::st_transform(sites_locations_sf, 
                                           terra::crs(grid, proj = TRUE))
  }
  
  
  ## Rasterize data ----
  
  fields <- colnames(sf::st_drop_geometry(sites_locations_sf))
  
  rasters <- lapply(seq_along(fields), function(x) {
    terra::rasterize(terra::vect(sites_locations_sf), grid, field = fields[x], 
                     fun = fun)
  })
  
  rasters <- terra::rast(rasters)
  names(rasters) <- fields
  
  rasters
}

#' Upscale sites: aggregates them at higher scales
#'
#' This function helps upscaling site data. In our sense upscaling is when
#' you want to work with a version of your data aggregated at a coarser spatial
#' scale. This function aggregates the site data at a given scale.
#'
#' @inheritParams fb_get_environment

#' @param site_data a `matrix` or `data.frame` containing values per sites to 
#'   aggregate along the provided grid. Can have one or several columns
#'   (variables to aggregate). Row names must  contain sites names as provided
#'   as `sites_locations`.
#'
#' @param grid a `SpatRaster` object (package `terra`). A raster of one single 
#'   layer, that defines the grid along which to aggregate.
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

fb_upscale_sites <- function(sites_locations, site_data, grid, fun = mean,
                             crs = "+proj=longlat +datum=WGS84 +no_defs") {
  
  ## Check inputs ----
  
  if (missing(sites_locations)) {
    stop("Argument 'sites_locations' (sites x locations matrix) is required",
         call. = FALSE)
  }
  
  check_sites_locations(sites_locations)
  
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
  
  sites_locations <- merge(sites_locations, site_data, by = "row.names")
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

#' Map a raster layer
#' 
#' @param x a `SpatRaster` object (package `terra`). A raster of one single 
#' layer
#' 
#' @param ... other parameters passed to [`theme()`]
#'
#' @return A `ggplot` object.
#' 
#' @export
#'
#' @examples
#' library(ggplot2)
#' 
#' ## Load raster ----
#' tavg <- system.file("extdata", "annual_mean_temp.tif", package = "funbiogeo")
#' tavg <- terra::rast(tavg)
#' 
#' ## Default map ----
#' fb_map_raster(tavg)
#' 
#' ## Map with custom theme ----
#' fb_map_raster(tavg, legend.position = "bottom")
#' 
#' ## Advanced customization ----
#' my_map <- fb_map_raster(tavg) + 
#'   scale_fill_distiller("Temperature", palette = "Spectral") +
#'   theme(legend.position = "bottom") + 
#'   ggtitle("Mean annual temperature in Pennsylvania")
#' 
#' my_map
#' 
#' ## Map w/o annotation ----
#' fb_map_raster(tavg) + 
#'   theme_void() + 
#'   theme(legend.position = "none")

fb_map_raster <- function(x, ...) {
  
  
  ## Check inputs ----
  
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  
  if (!inherits(x, "SpatRaster")) {
    stop("Argument 'x' must be a 'SpatRaster' object (package terra)", 
         call. = FALSE)
  }
  
  if (terra::nlyr(x) > 1) {
    stop("Argument 'x' must be a single layer 'SpatRaster' object", 
         call. = FALSE)
  }
  
  
  ## Fortify raster ----
  
  x <- terra::as.data.frame(x, xy = TRUE)
  
  
  ## Plot ----
  
  ggplot2::ggplot(x) + 
    
    ggplot2::geom_raster(ggplot2::aes_string(x = "x", y = "y", 
                                             fill = colnames(x)[3])) + 
    
    ggplot2::coord_equal() + ggplot2::labs(x = "Longitude", y = "Latitude") + 
    
    ggplot2::theme_bw() + ggplot2::theme(...)
}

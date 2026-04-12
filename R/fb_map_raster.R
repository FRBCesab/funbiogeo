#' Map a Single Raster Layer
#'
#' @param x a `SpatRaster` object (package `terra`). A raster of one single
#' layer
#'
#' @param background a `logical`. If `TRUE` adds a layer of country boundaries
#'   from Natural Earth.
#'
#' @param ... other parameters passed to `ggplot2::theme()`
#'
#' @return A `ggplot` object.
#'
#' @importFrom rlang .data
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
#' ## Map with a background ----
#' fb_map_raster(tavg, background = TRUE)
#'
#' ## Map with custom theme ----
#' fb_map_raster(tavg, legend.position = "bottom")
#'
#' ## Advanced customization ----
#' my_map <- fb_map_raster(tavg) +
#'   scale_fill_distiller("Temperature", palette = "Spectral") +
#'   theme(legend.position = "bottom") +
#'   ggtitle("Mean annual temperature in Western Europe")
#'
#' my_map
#'
#' ## Map w/o annotation ----
#' fb_map_raster(tavg) +
#'   theme_void() +
#'   theme(legend.position = "none")

fb_map_raster <- function(x, background = FALSE, ...) {
  # Checks
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }

  if (!inherits(x, "SpatRaster")) {
    stop(
      "Argument 'x' must be a 'SpatRaster' object (package terra)",
      call. = FALSE
    )
  }

  if (terra::nlyr(x) > 1) {
    stop(
      "Argument 'x' must be a single layer 'SpatRaster' object",
      call. = FALSE
    )
  }

  if (!is.logical(background) & !is.na(background)) {  
    stop(
      "The 'background' argument should either be TRUE or FALSE", 
      call. = FALSE
    )
  } 

  # Fortify raster
  x <- terra::as.data.frame(x, xy = TRUE)

  # Define map extent
  map_extent <- c(range(x$x), range(x$y))

  basemap <- NULL  

  if (background) {  

    # Import World baseline (Natural Earth)
    basemap <- rnaturalearth::ne_countries()
    basemap <-  ggplot2::geom_sf(data = basemap, fill = NA, color = "white")
  }

  # Plot
  ggplot2::ggplot(x) +
    ggplot2::geom_raster(
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data[[colnames(x)[3]]])
    ) +
    basemap + 
    ggplot2::coord_sf(  
      xlim = c(map_extent[1], map_extent[2]),  
      ylim = c(map_extent[3], map_extent[4]),  
      expand = TRUE  
    ) +   
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::theme_bw() +
    ggplot2::theme(...)
}

#' Plot Sites' Position in Environmental Space
#' 
#' Plot a figure showing the average environmental space of given sites compared
#' to a full environmental vector. For the sake of simplicity only represents
#' the figure along two environmental axes. The average environmental value are
#' extracted for each site.
#' 
#' @inheritParams fb_get_environment
#' @param first_layer `character(1)` the name of the first layer to use,
#'   by default uses the first layer of `environment_raster`
#' @param second_layer `character(1)` the name of the second layer to use,
#'   by default uses the second layer of `environment_raster`
#'
#' @return a `ggplot` object
#' 
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' data("site_locations")
#' 
#' # Import climate rasters
#' prec <- system.file("extdata", "annual_tot_prec.tif", package = "funbiogeo")
#' tavg <- system.file("extdata", "annual_mean_temp.tif", package = "funbiogeo")
#' 
#' layers <- terra::rast(c(tavg, prec))
#' 
#' # Make plot (show environmental position of 6 first sites)
#' fb_plot_site_environment(head(site_locations), layers)
fb_plot_site_environment = function(
    site_locations, environment_raster,
    first_layer = names(environment_raster)[1],
    second_layer = names(environment_raster)[2]
  )  {
  
  
  # Checks
  check_site_locations(site_locations)
  
  if (missing(environment_raster)) {
    stop("Argument 'environment_raster' (environmental raster) is required",
         call. = FALSE)
  }
  
  if (!inherits(environment_raster, "SpatRaster")) {
    stop("The raster layer must be a 'SpatRaster' object (package `terra`)", 
         call. = FALSE)
  }
  
  if (!(first_layer %in% names(environment_raster))) {
    stop(
      "Argument 'first_layer' should be the name of a layer in provided ",
      "'environment_raster'"
    )
  }
  
  if (!(second_layer %in% names(environment_raster))) {
    stop(
      "Argument 'second_layer' should be the name of a layer in provided ",
      "'environment_raster'"
    )
  }
  
  
  # Select layers
  environment_raster <- environment_raster[[c(first_layer, second_layer)]]
  
  
  # Get sites average environments
  site_environment <- fb_get_environment(site_locations, environment_raster)
  
  
  # Actual plot
  ggplot2::ggplot(
    site_environment, ggplot2::aes(.data[[first_layer]], .data[[second_layer]])
  ) +
    ggplot2::geom_point(
      data = as.data.frame(terra::values(environment_raster)), shape = ".",
      color = "gray75"
    ) + 
    ggplot2::geom_point(color = "darkblue") +
    ggplot2::theme_bw() +
    ggplot2::theme(aspect.ratio = 1)
}
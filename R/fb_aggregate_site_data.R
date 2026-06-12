#' Aggregate Site Data Along Coarser Spatial Grid
#'
#' This function helps aggregating site data along a coarser grid.
#' The idea is that you have any type of data at the site scale
#' (diversity metrics, environmental data, etc.) but you would like to work
#' on it at a coarser scale, or you want to visualize it at that scale.
#' This function helps you do exactly that.
#'
#' @inheritParams fb_get_environment

#' @param site_data a `matrix` or `data.frame` containing values per site to
#'   aggregate along the provided grid. It can contain one or several columns
#'   (variables to aggregate). The first column must contain site names as
#'   provided in the first argument `site_locations`.
#'
#' @param agg_geom a `terra::SpatRaster` or an `sf` object. This defines the
#'   geometry along which to aggregate the initial data. See more in the Details
#'   section.
#'
#' @param fun the function used to aggregate point values when there are
#'   multiple points in one cell. Default is `mean`.
#'
#' @param ... additional argument(s) passed to the provided function `fun`
#'
#' @details
#' The `agg_geom` object will condition the type of object output by the
#' function. It can be of any sort of a `SpatRaster` or a `sf` object. Depending
#' on the need, it could be a regular square grid or hexagonal grid; it could
#' also be irregular polygons like biomes or ecoregions, or points, and even
#' lines (such as when aggregating across transects or trajectories).
#'
#' @return An object of the same type as the `agg_geom` input with as many
#'   layers (if `SpatRaster`) or columns (if `sf`) as columns provided in the
#'   input `site_data`.
#'
#' @import sf
#' @export
#'
#' @examples
#' library("sf")
#' ## Raster grid
#' tavg <- system.file(
#'   "extdata", "annual_mean_temp.tif", package = "funbiogeo"
#' )
#' tavg <- terra::rast(tavg)
#'
#' # Rasterize 3 first species counts
#' fb_aggregate_site_data(
#'     head(woodiv_locations), woodiv_site_species[, 1:4], tavg, fun = sum
#' )
#'
#' ## Irregular polygons
#' countries <- readRDS(system.file(
#'   "extdata", "countries_sf.rds", package = "funbiogeo"
#' ))
#' # Aggregate occurrence per country
#' fb_aggregate_site_data(
#'     head(woodiv_locations, n = 20), woodiv_site_species[, 1:4], countries,
#'     fun = sum
#' )

fb_aggregate_site_data <- function(
  site_locations,
  site_data,
  agg_geom,
  fun = mean,
  ...
) {
  # Check inputs ---------------------------------------------------------------

  check_site_locations(site_locations)

  if (missing(site_data)) {
    stop("Argument 'site_data' is required", call. = FALSE)
  }

  if (!is.data.frame(site_data) && !is.matrix(site_data)) {
    stop("Argument 'site_data' must be a matrix or a data.frame", call. = FALSE)
  }

  if (0 %in% dim(site_data)) {
    stop(
      "Argument 'site_data' should have at least one row and one column",
      call. = FALSE
    )
  }

  if (missing(agg_geom)) {
    stop("Argument 'agg_geom' is required", call. = FALSE)
  }

  if (!inherits(agg_geom, "SpatRaster") && !inherits(agg_geom, "sf")) {
    stop(
      "The 'agg_geom' raster must be a 'SpatRaster' (package `terra`)",
      " or an 'sf' object",
      call. = FALSE
    )
  }

  if (
    is.na(terra::crs(agg_geom, proj = TRUE)) ||
      !nzchar(terra::crs(agg_geom, proj = TRUE))
  ) {
    stop(
      "The 'agg_geom' raster must have a CRS (coordinate system)",
      call. = FALSE
    )
  }

  # Simplify site-locations object ---------------------------------------------
  # To avoid importing the entire 'sf' package we temporarily remove the
  # geometry column
  site_geom <- sf::st_geometry(site_locations)
  sf::st_geometry(site_locations) <- NULL

  # Keep only site column
  site_locations <- site_locations[, "site", drop = FALSE]

  # Re-add the geometry column
  sf::st_geometry(site_locations) <- site_geom

  # Merge sites info -----------------------------------------------------------
  site_locations <- merge(site_locations, site_data, by = "site")

  # Aggregate based on grid type -----------------------------------------------

  if (inherits(agg_geom, "SpatRaster")) {
    fb_aggregate_site_data_raster(
      site_locations,
      site_data,
      agg_geom,
      fun,
      ...
    )
  } else if (inherits(agg_geom, "sf")) {
    fb_aggregate_site_data_sf(site_locations, site_data, agg_geom, fun, ...)
  }
}

# Function when grid is a raster
fb_aggregate_site_data_raster <- function(
  site_locations,
  site_data,
  agg_geom,
  fun,
  ...
) {
  # Get proper aggregation grid ------------------------------------------------

  agg_geom <- terra::subset(agg_geom, 1)

  # Reproject sites if required ------------------------------------------------

  if (
    sf::st_crs(site_locations) != sf::st_crs(terra::crs(agg_geom, proj = TRUE))
  ) {
    site_locations <- sf::st_transform(
      site_locations,
      sf::st_crs(terra::crs(agg_geom, proj = TRUE))
    )
  }

  # Rasterize data -------------------------------------------------------------

  fields <- colnames(sf::st_drop_geometry(site_locations))[-1]

  rasters <- lapply(seq_along(fields), function(x) {
    terra::rasterize(
      terra::vect(site_locations),
      agg_geom,
      field = fields[x],
      fun = fun,
      ...
    )
  })

  rasters <- terra::rast(rasters)
  names(rasters) <- fields

  rasters
}

#' Function when agg_geom is an sf object
#' @importFrom stats aggregate
#' @noRd
fb_aggregate_site_data_sf <- function(
  site_locations,
  site_data,
  agg_geom,
  fun,
  ...
) {
  # Reproject sites if required ------------------------------------------------

  if (sf::st_crs(site_locations) != sf::st_crs(agg_geom)) {
    site_locations <- sf::st_transform(
      site_locations,
      sf::st_crs(agg_geom)
    )
  }

  # Aggregate data -------------------------------------------------------------
  # Select columns on which to perform aggregation
  data_columns <- setdiff(colnames(site_data), "site")

  # Perform aggregation
  aggregated_sf <- aggregate(site_locations[, data_columns], agg_geom, fun, ...)
}

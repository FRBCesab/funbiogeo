#' Map Arbitrary Site Data
#'
#' From the site-locations data and a dataset organized by site, plot a map of
#' this information.
#' The returned plot is as little customized as possible to let the user do
#' the customization.
#'
#' @inheritParams fb_get_environment 
#' @param site_data `data.frame()` of additional site information containing
#'   the column `"site"` to merge with the `site_locations` argument
#' @param selected_col `character(1)` name of the column to plot 
#'
#' @return a `ggplot` object.
#' 
#' @importFrom rlang .data
#' @import sf
#' @export
#'
#' @examples
#' site_rich <- fb_count_species_by_site(woodiv_site_species)
#' 
#' # Map of Species Richness
#' rich_map <- fb_map_site_data(woodiv_locations, site_rich, "n_species")
#' rich_map
#' 
#' # Customize the map
#' rich_map +
#'   ggplot2::scale_fill_viridis_c("Species Richness")
fb_map_site_data <- function(site_locations, site_data, selected_col) {
  
  # Checks
  check_site_locations(site_locations)
  
  # Check 'site_data'
  if (missing(site_data)) {
    stop("Argument 'site_data' (site info data.frame) is required")
  }
  
  if (!is.data.frame(site_data)) {
    stop("Argument 'site_data' must be a data.frame")
  }
  
  if (!("site" %in% colnames(site_data))) {
    stop("\"site\" column should be in provided 'site_data'")
  }
  
  # Check 'selected_col'
  if (missing(selected_col)) {
    stop("Argument 'selected_col' (name of selected column) is required")  
  }
  
  if (!is.character(selected_col)) {
    stop("Argument 'selected_col' must be a character")
  }
  
  if (!(selected_col %in% colnames(site_data))) {
    stop("Provided 'selected_col' should be in 'site_data'")
  }
  
  # Merge data
  full_data <- merge(site_locations, site_data, by = "site")
  
  # Clean environment
  rm(site_data, site_locations)
  
  # Make plot
  if (inherits(sf::st_geometry(full_data), "sfc_POLYGON") |
      inherits(sf::st_geometry(full_data), "sfc_MULTIPOLYGON")) {
    
    ggplot2::ggplot(
      full_data, ggplot2::aes(fill = .data[[selected_col]])
    ) +
      ggplot2::geom_sf(color = NA) +
      ggplot2::theme_bw()

  } else {
    
    ggplot2::ggplot(full_data, ggplot2::aes(color = .data[[selected_col]])) +
      ggplot2::geom_sf() +
      ggplot2::theme_bw()
    
  }
}
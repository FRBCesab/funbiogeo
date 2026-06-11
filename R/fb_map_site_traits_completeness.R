#' Map Trait Coverage Per Site
#'
#' Returns a `ggplot2` map of sites colored by trait coverage (proportion
#' of species having a known trait value). By default shows one plot for each
#' trait and add an additional facet named `"all_traits"` considering the
#' trait coverage with all provided traits taken together.
#'
#' @inheritParams fb_get_environment
#' @inheritParams fb_get_all_trait_coverages_by_site
#' @inheritParams fb_map_raster
#' @return a 'ggplot2' object
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' # Map without a background
#' fb_map_site_traits_completeness(
#'   woodiv_locations, woodiv_site_species, woodiv_traits
#' )
#'
#' # Map with a background
#' fb_map_site_traits_completeness(
#'   woodiv_locations, woodiv_site_species, woodiv_traits, background = TRUE
#' )

fb_map_site_traits_completeness <- function(
  site_locations,
  site_species,
  species_traits,
  all_traits = TRUE,
  background = FALSE
) {
  # Checks
  check_site_locations(site_locations)
  check_site_species(site_species)
  check_species_traits(species_traits)

  if (!is.logical(background) && !is.na(background)) {
    stop(
      "The 'background' argument should either be TRUE or FALSE",
      call. = FALSE
    )
  }

  all_coverages <- fb_get_all_trait_coverages_by_site(
    site_species,
    species_traits,
    all_traits = all_traits
  )

  # Make coverage df long
  all_coverages_long <- tidyr::pivot_longer(
    all_coverages,
    -"site",
    names_to = "coverage_name",
    values_to = "coverage_value"
  )

  if (all_traits) {
    levels_order <- c("all_traits", colnames(all_coverages)[-c(1, 2)])
  } else {
    levels_order <- c(colnames(all_coverages)[-c(1, 2)])
  }

  all_coverages_long[["coverage_name"]] <-
    factor(all_coverages_long[["coverage_name"]], levels = levels_order)

  # Combine Trait Coverage with Location
  site_locations_cov <- merge(site_locations, all_coverages_long, by = "site")

  # Clean environment
  rm(
    all_coverages,
    all_coverages_long,
    all_traits,
    levels_order,
    site_locations,
    site_species,
    species_traits
  )

  # Define map extent
  map_extent <- sf::st_bbox(site_locations_cov)

  basemap <- NULL

  if (background) {
    # Import World baseline (Natural Earth)
    basemap <- rnaturalearth::ne_countries()
    basemap <- ggplot2::geom_sf(data = basemap, fill = NA)
  }

  # Make the Map
  if (
    inherits(sf::st_geometry(site_locations_cov), "sfc_POLYGON") ||
      inherits(sf::st_geometry(site_locations_cov), "sfc_MULTIPOLYGON")
  ) {
    # If sites are (multi-)polygons
    base_plot <- ggplot2::ggplot(
      site_locations_cov,
      ggplot2::aes(fill = .data$coverage_value)
    ) +
      ggplot2::geom_sf(colour = NA) +
      basemap +
      ggplot2::coord_sf(
        xlim = c(map_extent[1], map_extent[3]),
        ylim = c(map_extent[2], map_extent[4]),
        expand = TRUE
      ) +
      ggplot2::scale_fill_viridis_c(
        "Trait Coverage",
        labels = scales::label_percent()
      )
  } else {
    # If sites are points or (multi-)linestrings or geometrycollection
    base_plot <- ggplot2::ggplot(site_locations_cov) +
      basemap +
      ggplot2::geom_sf(ggplot2::aes(colour = .data$coverage_value)) +
      ggplot2::scale_colour_viridis_c(
        "Trait Coverage",
        labels = scales::label_percent()
      ) +
      ggplot2::coord_sf(
        xlim = c(map_extent[1], map_extent[3]),
        ylim = c(map_extent[2], map_extent[4]),
        expand = TRUE
      )
  }

  base_plot +
    ggplot2::facet_wrap(ggplot2::vars(.data$coverage_name)) +
    ggplot2::theme_bw()
}

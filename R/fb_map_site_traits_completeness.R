#' Map Trait Coverage Per Site
#'
#' @inheritParams fb_get_environment 
#' @inheritParams fb_get_trait_coverage_by_site
#'
#' @return a 'ggplot2' object
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' fb_map_site_traits_completeness(site_locations, site_species, species_traits)
fb_map_site_traits_completeness <- function(
    site_locations, site_species, species_traits
) {
  
  # Checks
  check_site_locations(site_locations)
  check_site_species(site_species)
  check_species_traits(species_traits)
  
  all_coverages <- fb_get_all_trait_coverages_by_site(
    site_species, species_traits
  )
  
  # Make coverage df long
  all_coverages_long <- tidyr::pivot_longer(
    all_coverages, -"site", names_to = "coverage_name",
    values_to = "coverage_value"
  )
  
  all_coverages_long[["coverage_name"]] <- 
    factor(all_coverages_long[["coverage_name"]],
           levels = c("all_traits", colnames(all_coverages)[-c(1, 2)]))
  
  # Combine Trait Coverage with Location
  site_locations_cov <- merge(site_locations, all_coverages_long, by = "site")
  
  # Make the Map
  if(inherits(site_locations_cov$geom, "sfc_POLYGON") |
     inherits(site_locations_cov$geom, "sfc_MULTIPOLYGON")) {
    
    # If sites are (multi-)polygons
    base_plot <- ggplot2::ggplot(
      site_locations_cov, ggplot2::aes(fill = .data$coverage_value)
    ) +
      ggplot2::geom_sf() +
      ggplot2::scale_fill_viridis_c(
        "Trait Coverage", labels = scales::label_percent()
      )
    
  } else {
    
    # If sites are points or (multi-)linestrings or geometrycollection
    base_plot <- ggplot2::ggplot(
      site_locations_cov, ggplot2::aes(colour = .data$coverage_value)
    ) +
      ggplot2::geom_sf() +
      ggplot2::scale_colour_viridis_c(
        "Trait Coverage", labels = scales::label_percent()
      )
    
  }
  
  base_plot +
    ggplot2::facet_wrap(ggplot2::vars(.data$coverage_name)) +
    ggplot2::theme_bw()
}
#' Show Species by Trait Completeness
#'
#' @inheritParams fb_get_coverage
#'
#' @return a `ggplot2` object
#'
#' @examples
#' data(species_traits)
#' \dontrun{fb_plot_species_traits_completeness(species_traits)}
#' 
#' @import ggplot2
#' @export
fb_plot_species_traits_completeness = function(species_traits) {
  
  # Make dataset long
  species_traits_long = tidyr::pivot_longer(
      species_traits, -"species", names_to = "trait_name",
      values_to = "trait_value"
    )
  
  # Count Number of Species per Trait
  number_species_per_trait = count_species_per_trait(species_traits_long)
  
  # Count Number of Trait per Species
  number_trait_per_species = count_trait_per_species(species_traits_long)
  
  species_traits_long$has_trait = ifelse(
    !is.na(species_traits_long$trait_value), TRUE, FALSE
  )
  
  # Plot Species x Trait completeness
  ggplot(
    species_traits_long,
      aes_q(
        ~factor(trait_name, levels = number_species_per_trait$trait_name),
        ~factor(species,    levels = number_trait_per_species$species)
      )
    ) +
    geom_tile(aes_q(fill = ~has_trait)) +
    scale_x_discrete("Trait", labels = number_species_per_trait$trait_label) +
    scale_y_discrete("Species", labels = NULL) +
    scale_fill_brewer(
      "Known Trait?", palette = "Set1",
      labels = c(`FALSE` = "No", `TRUE` = "Yes")
    ) +
    coord_cartesian(expand = FALSE) +
    theme_bw() +
    theme(axis.ticks.y = element_blank())
}
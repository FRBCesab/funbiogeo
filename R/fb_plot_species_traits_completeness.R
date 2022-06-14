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
  ggplot2::ggplot(
    species_traits_long,
    ggplot2::aes_q(
        ~factor(trait_name, levels = number_species_per_trait$trait_name),
        ~factor(species,    levels = number_trait_per_species$species)
      )
    ) +
    ggplot2::geom_tile(
      ggplot2::aes_q(fill = ~has_trait)) +
    ggplot2::scale_x_discrete(
      "Trait", labels = number_species_per_trait$trait_label
    ) +
    ggplot2::scale_y_discrete("Species", labels = NULL) +
    ggplot2::scale_fill_brewer(
      "Known Trait?", palette = "Set1",
      labels = c(`FALSE` = "No", `TRUE` = "Yes")
    ) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
}
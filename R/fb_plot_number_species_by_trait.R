#' Show Number of Species per Trait
#'
#' @inheritParams fb_filter_traits_by_species_coverage
#'
#' @return a ggplot2 object
#'
#' @examples
#' data(species_traits)
#' 
#' fb_plot_number_species_by_trait(species_traits)
#' 
#' # Add a vertical cutoff line (12.5% of species)
#' fb_plot_number_species_by_trait(species_traits, 1/8)
#' 
#' @importFrom rlang .data
#' @export
fb_plot_number_species_by_trait = function(
    species_traits, threshold_species_proportion = NULL
) {
  
  # Make dataset long
  species_traits_long = tidyr::pivot_longer(
    species_traits, -"species", names_to = "trait_name",
    values_to = "trait_value"
  )
  
  number_species_per_trait = fb_count_species_by_traits(species_traits)
  
  number_species_per_trait$trait = factor(
    number_species_per_trait$trait, levels = rev(number_species_per_trait$trait)
  )
  
  number_species_per_trait[["prop_species"]] =
    number_species_per_trait[["n_species"]]/nrow(species_traits)
  
  given_plot = ggplot2::ggplot(
    number_species_per_trait, ggplot2::aes(.data$n_species, .data$trait)
  ) +
    ggplot2::geom_point(color = "darkblue") +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(prettyNum(.data$prop_species * 100, digits = 3), "%")
      ),
      hjust = -0.15, size = 3.5
    ) +
    ggplot2::scale_x_continuous(
      "Number of Species",
      sec.axis = ggplot2::sec_axis(
        trans = ~./nrow(species_traits), "Proportion of Species",
        labels = scales::label_percent()
      ),
      # Add a tiny bit of space so that proportion can be shown
      limits = c(NA_real_, max(number_species_per_trait$n_species)*1.015)
    ) +
    ggplot2::labs(y = "Trait Name") +
    ggplot2::theme_bw()
  
  if (!is.null(threshold_species_proportion)) {
    given_plot = given_plot +
      ggplot2::geom_vline(
        xintercept = threshold_species_proportion * nrow(species_traits),
        linetype = 2, size = 1.2, color = "darkred"
      ) +
      ggplot2::annotate(
        "text", x = threshold_species_proportion * nrow(species_traits),
        y = 0.95, hjust = 1.1, color = "darkred",
        label = paste0(
          "(n = ", threshold_species_proportion * nrow(species_traits), ")\n",
          "(p = ",
          prettyNum(threshold_species_proportion, digits = 3), "%)"
        )
      )
  }  
  
  return(given_plot)
}
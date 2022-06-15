#' Show Number of Traits per Species
#'
#' @inheritParams fb_filter_traits_by_species_coverage
#'
#' @return a ggplot2 object
#' 
#'
#' @examples
#' data(species_traits)
#' 
#' \dontrun{
#' fb_plot_number_traits_by_species(species_traits)
#' 
#' # Add a vertical cutoff line (30% of the species)
#' fb_plot_number_traits_by_species(species_traits, 1/3)
#' }
#' 
#' @export
fb_plot_number_traits_by_species = function(
    species_traits, threshold_species_proportion = NULL
) {
  
  # Make dataset long
  species_traits_long = tidyr::pivot_longer(
    species_traits, -"species", names_to = "trait_name",
    values_to = "trait_value"
  )
  
  number_trait_per_species = fb_count_traits_by_species(species_traits)
  
  # Count number of species per number of trait (XX species has YY traits)
  number_trait_per_species = by(
    number_trait_per_species, number_trait_per_species$n_traits,
    function(x) c(n = nrow(x))
  )
  number_trait_per_species = utils::stack(number_trait_per_species)
  number_trait_per_species$n_traits = 
    as.numeric(as.character(number_trait_per_species$ind))
  
  given_plot = ggplot2::ggplot(
    number_trait_per_species, ggplot2::aes_q(~values, ~n_traits)
  ) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::geom_segment(
      ggplot2::aes_q(y = ~n_traits, yend = ~n_traits, x =~0, xend = ~values)
    ) +
    ggplot2::labs(x = "Number of Species", y = "Number of Traits") +
    ggplot2::scale_x_continuous(
      sec.axis = ggplot2::sec_axis(
        ~./nrow(species_traits), "Proportion of Species",
        labels = scales::label_percent()
      )
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, to = max(number_trait_per_species$n_trait), by = 1)
    ) +
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
          prettyNum(threshold_species_proportion * 100,  digits = 3),
          "%)"
        )
      )
  }
  
  return(given_plot)
}
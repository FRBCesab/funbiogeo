#' Show Number of Traits per Species
#'
#' @inheritParams fb_get_coverage
#' @param threshold_species_number `NULL` or `numeric(1)` \[default = `NULL`\]\cr{}
#'        If `NULL` doesn't show a line otherwise adds a vertical line
#'        at the specified number of species
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' data(species_traits)
#' 
#' fb_plot_number_traits_per_species(species_traits)
#' 
#' # Add a vertical cutoff line
#' fb_plot_number_traits_per_species(species_traits, 30)
fb_plot_number_traits_per_species = function(
    species_traits, threshold_species_number
) {
  
  # Make dataset long
  species_traits_long = tidyr::pivot_longer(
    species_traits, -species, names_to = "trait_name",
    values_to = "trait_value"
  )
  
  number_trait_per_species = count_trait_per_species(species_traits_long)
  
  given_plot = number_trait_per_species %>%
    count(n_trait) %>%
    ggplot(aes(n, n_trait)) +
    geom_point(size = 1.5) +
    geom_segment(aes(y = n_trait, yend = n_trait, x = 0, xend = n)) +
    labs(x = "Number of Species", y = "Number of Traits") +
    scale_x_continuous(
      sec.axis = sec_axis(~./nrow(species_traits), "Proportion of Species",
                          labels = scales::label_percent())
    ) +
    scale_y_continuous(
      breaks = seq(0, to = max(number_trait_per_species$n_trait), by = 1)
    ) +
    theme_bw()
  
  if (!is.null(threshold_species_number)) {
    given_plot = given_plot +
      geom_vline(xintercept = threshold_species_number, linetype = 2,
                 size = 1.2, color = "darkred") +
      annotate(
        "text", x = threshold_species_number, y = 0.95, hjust = 1.1,
        color = "darkred",
        label = paste0(
          "(n = ", threshold_species_number, ")\n",
          "(p = ",
          prettyNum(threshold_species_number/nrow(species_traits) * 100,
                    digits = 3),
          "%)")
      )
  }
  
  return(given_plot)
}
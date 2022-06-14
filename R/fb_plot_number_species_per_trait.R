#' Show Number of Species per Trait
#'
#' @inheritParams fb_get_coverage
#'
#' @param threshold_species_number `NULL` or `numeric(1)` \[default = `NULL`\]\cr{}
#'        If `NULL` doesn't show a line otherwise adds a vertical line
#'        at the specified number of species
#'
#' @return a ggplot2 object
#'
#' @examples
#' data(species_traits)
#' 
#' fb_plot_number_species_per_trait(species_traits)
#' 
#' # Add a vertical cutoff line
#' fb_plot_number_species_per_trait(species_traits, 100)
#' @export
fb_plot_number_species_per_trait = function(
    species_traits, threshold_species_number = NULL
) {
  
  # Make dataset long
  species_traits_long = tidyr::pivot_longer(
    species_traits, -species, names_to = "trait_name",
    values_to = "trait_value"
  )
  
  number_species_per_trait = count_species_per_trait(species_traits_long)
  
  number_species_per_trait$trait_name = factor(
    number_species_per_trait$trait_name,
    levels = rev(number_species_per_trait$trait_name)
  )
  
  given_plot = ggplot(
    number_species_per_trait, aes(n_species, trait_name)
  ) +
    geom_point(color = "darkblue") +
    geom_text(
      aes(label = paste0(prettyNum(prop_species * 100, digits = 3), "%")),
      hjust = -0.15, size = 3.5
    ) +
    scale_x_continuous(
      "Number of Species",
      sec.axis = sec_axis(
        trans = ~./nrow(species_traits), "Proportion of Species",
        labels = scales::label_percent()
      ),
      # Add a tiny bit of space so that proportion can be shown
      limits = c(NA_real_, max(number_species_per_trait$n_species)*1.015)
    ) +
    labs(y = "Trait Name") +
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
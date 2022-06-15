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
#' \dontrun{%
#' fb_plot_number_species_by_trait(species_traits)
#' 
#' # Add a vertical cutoff line
#' fb_plot_number_species_by_trait(species_traits, 100)
#' }
#' 
#' @export
fb_plot_number_species_by_trait = function(
    species_traits, threshold_species_number = NULL
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
    number_species_per_trait, ggplot2::aes_string("n_species", "trait")
  ) +
    ggplot2::geom_point(color = "darkblue") +
    ggplot2::geom_text(
      ggplot2::aes_q(
        label = ~paste0(prettyNum(prop_species * 100, digits = 3), "%")
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
  
    if (!is.null(threshold_species_number)) {
      given_plot = given_plot +
        ggplot2::geom_vline(
          xintercept = threshold_species_number, linetype = 2, size = 1.2,
          color = "darkred"
        ) +
        ggplot2::annotate(
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
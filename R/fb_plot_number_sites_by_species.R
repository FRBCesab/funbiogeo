#' Plot Number of Sites by Species
#'
#' @inheritParams fb_filter_species_by_site_coverage
#'
#' @return
#' @export
#'
#' @examples
#' 
#' fb_plot_number_sites_by_species(site_species, 0.4)
fb_plot_number_sites_by_species <- function(
    site_species, threshold_sites_proportion = NULL
) {
  
  # Check ----------------------------------------------------------------------
  check_site_species(site_species)
  
  if (!is.null(threshold_sites_proportion)) {
    check_threshold_proportion(threshold_sites_proportion, "site")
  }
  
  # Get the numbers
  number_sites_by_species <- fb_count_sites_by_species(site_species)
  
  # Actual plot
  given_plot <- ggplot2::ggplot(
    number_sites_by_species, ggplot2::aes(.data$n_sites)
  ) +
    ggplot2::geom_density(
      ggplot2::aes(y = ggplot2::after_stat(count))
    ) +
    ggplot2::scale_x_continuous(
      "Number of Sites",
      sec.axis = ggplot2::sec_axis(
        trans = ~./nrow(site_species), "Proportion of Sites",
        labels = scales::label_percent()
      )
    ) +
    ggplot2::scale_y_continuous(
      "Proportion of Species", labels = scales::label_percent(),
      sec.axis = ggplot2::sec_axis(
        trans = ~.*ncol(site_species), "Number of Species"
      )
    ) +
    ggplot2::theme_bw()
  
  if (!is.null(threshold_sites_proportion)) {
    given_plot <- given_plot +
      ggplot2::geom_vline(
        xintercept = threshold_sites_proportion * nrow(site_species),
        linetype = 2, size = 1.2, color = "darkred"
      ) +
      ggplot2::annotate(
        "text", x = threshold_sites_proportion * nrow(site_species),
        y = max(number_sites_by_species[["n_site"]]), hjust = 1.1,
        color = "darkred",
        label = paste0(
          "(n = ", round(threshold_sites_proportion * nrow(site_species)),
          ")\n(p = ",
          round(threshold_sites_proportion, 1), "%)"
        )
      )
  }  
  
  return(given_plot)
}
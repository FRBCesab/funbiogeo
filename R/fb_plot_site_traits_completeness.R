#' Plot Trait Coverage per Site for each Trait
#'
#' Display a binary heatmap visualizing the site x traits matrix with colors
#' displaying the proportion of occurring species with known trait values.
#' Traits are ordered from the most to the least known (left to right).
#' Sites are ordered from the ones with highest to lowest overall trait coverage
#' (bottom to top). The site average proportion of species with known trait
#' for each trait (across all sites) is shown in the x-axis labels.
#' An additional column at the very right of the plot named `"all_traits"`
#' shows a summary considering traits together.
#' 
#' @inheritParams fb_get_all_trait_coverages_by_site
#'
#' @return a ggplot2 object
#'
#' @examples
#' fb_plot_site_traits_completeness(site_species, species_traits)
#' 
#' @importFrom rlang .data
#' @export
fb_plot_site_traits_completeness <- function(
    site_species, species_traits, all_traits = TRUE
) {
  
  # Checks
  check_site_species(site_species)
  check_species_traits(species_traits)
  
  all_coverage <- fb_get_all_trait_coverages_by_site(
    site_species, species_traits, all_traits = all_traits
  )
  
  all_coverage <- tidyr::pivot_longer(
    all_coverage, -"site", names_to = "coverage_name",
    values_to = "coverage_value"
  )
  
  site_order <- by(
    all_coverage, all_coverage$site, function(x) mean(x$coverage_value)
  )
  site_order <- utils::stack(site_order)
  
  coverage_order <- by(
    all_coverage, all_coverage$coverage_name, function(x) mean(x$coverage_value)
  )
  coverage_order <- utils::stack(coverage_order)
  
  # Reorder sites and traits by average coverage
  all_coverage$site <- factor(
    all_coverage$site,
    levels = site_order[["ind"]][
      order(site_order[["values"]], decreasing = TRUE)
    ]
  )
  
  all_coverage$coverage_name <- factor(
    all_coverage$coverage_name,
    levels = coverage_order[["ind"]][
      order(coverage_order[["values"]], decreasing = TRUE)
    ]
  )
  
  
  # Get averaging coverage
  avg_coverage <- by(
    all_coverage, all_coverage$coverage_name,
    function(x) mean(x$coverage_value)
  )
  
  avg_coverage <- utils::stack(avg_coverage)
  colnames(avg_coverage) <- c("avg_coverage", "coverage_name")
  
  avg_coverage[["cov_label"]] <- 
    with(
      avg_coverage,
      paste0(
        coverage_name, "\n(", round(avg_coverage * 100, 1), "%)"
      )
    )
  
  avg_coverage <- avg_coverage[, c("cov_label", "coverage_name")]
  avg_coverage <- t(utils::unstack(avg_coverage))
  
  
  ggplot2::ggplot(
    all_coverage,
    ggplot2::aes(.data$coverage_name, .data$site, fill = .data$coverage_value)
  ) +
    ggplot2::geom_tile() +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::labs(x = "Trait Name", y = "Sites") +
    ggplot2::scale_x_discrete(
      labels = avg_coverage, guide = ggplot2::guide_axis(n.dodge = 2)
    ) +
    ggplot2::scale_fill_viridis_b(
      n.breaks = 10,
      "Trait Coverage\n(Prop. of species)",
      labels = scales::label_percent(),
      show.limits = TRUE
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y  = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "top",
      legend.key.width = grid::unit(2, "cm")
    )
}
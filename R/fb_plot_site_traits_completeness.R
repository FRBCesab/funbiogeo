#' Plot Trait Coverage per Site for each Trait
#'
#' @inheritParams fb_get_trait_coverage_by_site
#'
#' @return a ggplot2 object
#'
#' @examples
#' fb_plot_site_traits_completeness(site_species, species_traits)
#' 
#' @importFrom rlang .data
#' @export
fb_plot_site_traits_completeness <- function(site_species, species_traits) {
  
  # Checks
  check_site_species(site_species)
  check_species_traits(species_traits)
  
  all_coverage <- fb_get_all_trait_coverages_by_site(
    site_species, species_traits
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
        coverage_name, "\n(", prettyNum(avg_coverage * 100, digits = 3), "%)"
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
    ggplot2::scale_x_discrete(labels = avg_coverage) +
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
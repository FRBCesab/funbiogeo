#' Plot Distribution of Trait Coverages across all Sites
#' 
#' Plots the distributions of trait coverage across site, i.e. the proportion of
#' species weighted by abundance with known trait values, for each trait
#' separately and all traits taken together.
#' The trait distributions are ordered from the lowest to the highest average
#' trait coverage (top to bottom). The top always displays a distribution named
#' `"all_traits"` that contains the distribution of coverage all traits taken
#' together.
#' 
#' @inheritParams fb_get_all_trait_coverages_by_site
#'
#' @return a 'ggplot2' object
#'
#' @examples
#' fb_plot_distribution_site_trait_coverage(site_species, species_traits)
#' 
#' @importFrom rlang .data
#' @export
fb_plot_distribution_site_trait_coverage <- function(
    site_species, species_traits, all_traits = TRUE
) {
  
  # Checks
  check_site_species(site_species)
  check_species_traits(species_traits)
  
  full_coverage <- data.frame(site = rownames(site_species))
  
  # Computing Trait Coverage per Site
  if (all_traits) {
    full_coverage <- fb_get_trait_coverage_by_site(
      site_species, species_traits
    )
    colnames(full_coverage)[2] <- "all_traits"
  }
  
  trait_coverage <- lapply(
    colnames(species_traits)[-1],
    function(x) {
      
      trait_cov2 <- fb_get_trait_coverage_by_site(
        site_species, species_traits[, c("species", x)]
      )
      
      colnames(trait_cov2)[2] <- x
      
      return(trait_cov2)
    })
  
  # Combine Trait Coverages
  trait_coverage <- Reduce(
    function(...) merge(..., by = "site", all.x = TRUE), trait_coverage
  )
  
  all_coverage <- merge(full_coverage, trait_coverage, by = "site")
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
  
  
  # Get average coverage per trait
  avg_coverage <- by(
    all_coverage, all_coverage$coverage_name,
    function(x) mean(x$coverage_value)
  )
  
  avg_coverage <- utils::stack(avg_coverage)
  colnames(avg_coverage) <- c("avg_coverage", "coverage_name")
  
  # Produce label per trait with average coverage
  avg_coverage[["cov_label"]] <- 
    with(
      avg_coverage,
      paste0(coverage_name, "\n(", round(avg_coverage * 100, digits = 1), "%)")
    )
  
  avg_coverage <- avg_coverage[, c("cov_label", "coverage_name")]
  avg_coverage <- t(utils::unstack(avg_coverage))
  
  
  if (!is_ggridges_installed()) {
    stop("This function requires 'ggridges' to work\n",
         "Please run \"install.packages('ggridges')\"", call. = FALSE)
  }
  
  # Figure
  ggplot2::ggplot(
    all_coverage,
    ggplot2::aes(.data$coverage_value, .data$coverage_name)
  ) +
    ggridges::stat_density_ridges(scale = 0.98) +
    ggplot2::scale_x_continuous(
      "Average Trait Coverage per Site", labels = scales::label_percent()
    ) +
    ggplot2::scale_y_discrete("Trait Name", labels = avg_coverage) +
    ggplot2::theme_bw()
}
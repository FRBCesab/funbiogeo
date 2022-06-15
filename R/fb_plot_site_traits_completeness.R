#' Plot Sites per Traits Completeness
#'
#' @inheritParams fb_get_coverage
#'
#' @return a ggplot2 object
#'
#' @examples
#' \dontrun{fb_plot_site_traits_completeness(site_species, species_traits)}
#' @export
fb_plot_site_traits_completeness = function(site_species, species_traits) {
  
  # Checks
  check_site_species(site_species)
  check_species_traits(species_traits)
  
  # Computing Trait Coverage per Site
  full_coverage = fb_get_coverage(site_species, species_traits)
  colnames(full_coverage)[2] = "all_traits"
  
  trait_coverage = lapply(colnames(species_traits)[-1], function(x) {
    
    trait_cov2 = fb_get_coverage(
      site_species, species_traits[, c("species", x)]
    )
    
    colnames(trait_cov2)[2] = x
    
    return(trait_cov2)
  })
  
  # Combine Trait Coverages
  trait_coverage = Reduce(
    function(...) merge(..., by = "site", all.x = TRUE), trait_coverage
  )
  
  all_coverage = merge(full_coverage, trait_coverage, by = "site")
  all_coverage = tidyr::pivot_longer(
    all_coverage, -"site", names_to = "coverage_name",
    values_to = "coverage_value"
  )
  
  # Reorder sites and traits by average coverage
  all_coverage$site = with(
    all_coverage, forcats::fct_reorder(
      factor(site), coverage_value, median, .desc = TRUE
    )
  )
  
  all_coverage$coverage_name = with(
    all_coverage, forcats::fct_reorder(
      factor(coverage_name), coverage_value, median, .desc = TRUE)
  )
  
  
  # Get averaging coverage
  avg_coverage = by(
    all_coverage, all_coverage$coverage_name,
    function(x) mean(x$coverage_value)
  )
  
  avg_coverage = stack(avg_coverage)
  colnames(avg_coverage) = c("avg_coverage", "coverage_name")
  
  avg_coverage[["cov_label"]] =
    with(
      avg_coverage,
      paste0(
        coverage_name, "\n(", prettyNum(avg_coverage * 100, digits = 3), "%)"
      )
    )
  
  avg_coverage = avg_coverage[, c("cov_label", "coverage_name")]
  avg_coverage = t(unstack(avg_coverage))
  
  
  ggplot(
    all_trait_coverage, aes(coverage_name, site, fill = coverage_value)
  ) +
    geom_tile() +
    coord_cartesian(expand = FALSE) +
    labs(x = "Trait Name", y = "Sites") +
    scale_x_discrete(labels = avg_coverage) +
    scale_fill_viridis_b(
      n.breaks = 10,
      "Trait Coverage\n(Prop. of species)",
      labels = scales::label_percent(),
      show.limits = TRUE
    ) +
    theme_bw() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "top",
      legend.key.width = unit(2, "cm")
    )
}
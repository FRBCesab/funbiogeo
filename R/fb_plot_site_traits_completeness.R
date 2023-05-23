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
#' @inheritParams fb_plot_species_traits_completeness
#'
#' @return a ggplot2 object
#'
#' @examples
#' fb_plot_site_traits_completeness(site_species, species_traits)
#' 
#' @importFrom rlang .data
#' @export
fb_plot_site_traits_completeness <- function(
    site_species, species_traits, species_categories = NULL, all_traits = TRUE
) {
  
  # Checks
  check_site_species(site_species)
  check_species_traits(species_traits)
  check_species_categories(species_categories)
  
  
  # Splitting species by category
  species_split <- list(single_cat = species_traits[["species"]])
  category_name <- "single_cat"
  
  if (!is.null(species_categories)) {
    
    category_name <- colnames(species_categories)[2]
    
    species_split <- split(
      species_categories[, 1], species_categories[, 2]
    )
    
  }
  
  
  # Split sites according to species categories
  site_species_categories <- lapply(
    species_split,
    function(x) site_species[, c("site", x), drop = FALSE]
  )
  
  all_coverage <- lapply(
    site_species_categories,
    function(x) fb_get_all_trait_coverages_by_site(
      x, species_traits, all_traits = all_traits
    )
  )
  
  all_coverage <- lapply(
    all_coverage,
    function(single_coverage) {
      
      single_coverage <- tidyr::pivot_longer(
        single_coverage, -"site", names_to = "coverage_name",
        values_to = "coverage_value"
      )
      
      site_order <- by(
        single_coverage, single_coverage$site,
        function(x) mean(x$coverage_value)
      )
      site_order <- utils::stack(site_order)
      
      coverage_order <- by(
        single_coverage, single_coverage$coverage_name,
        function(x) mean(x$coverage_value)
      )
      coverage_order <- utils::stack(coverage_order)
      
      # Reorder sites and traits by average coverage
      single_coverage$site <- factor(
        single_coverage$site,
        levels = site_order[["ind"]][
          order(site_order[["values"]], decreasing = TRUE)
        ]
      )
      
      single_coverage$coverage_name <- factor(
        single_coverage$coverage_name,
        levels = coverage_order[["ind"]][
          order(coverage_order[["values"]], decreasing = TRUE)
        ]
      )
      
      return(single_coverage)
    }
  )
  
  
  # Get average coverage
  avg_coverage <- lapply(
    all_coverage,
    function(x) {
      avg_coverage <- by(
        x, x$coverage_name,
        function(y) mean(y$coverage_value, na.rm = TRUE)
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
    }
  )
  
  
  # Add categories back into data.frame
  all_coverage <- lapply(
    names(all_coverage),
    function(x) {
      
      given_coverage <- all_coverage[[x]]
      
      given_coverage[category_name] <- x
        
      return(given_coverage)
    }
  )
  
  all_coverage <- do.call(rbind, all_coverage)
  
  avg_coverage <- lapply(
    names(avg_coverage),
    function(x) {
      
      given_coverage <- data.frame(
        coverage_name = colnames(avg_coverage[[x]]),
        avg_coverage = as.character(avg_coverage[[x]])
      )
      
      given_coverage[category_name] <- x
      
      return(given_coverage)
    }
  )
  
  avg_coverage <- do.call(rbind, avg_coverage)
  
  
  # Manage conditional faceting
  if (is.null(species_categories)) {
    
    category_facet <- NULL
    
  } else {
    
    category_facet <- ggplot2::facet_wrap(
      ggplot2::vars(
        !!rlang::sym(category_name)), scales = "free"
    )
    
  }
  
  all_coverage <- merge(
    all_coverage, avg_coverage, by = c("coverage_name", category_name)
  )
  
  # Clean environment
  rm(all_traits, avg_coverage, site_species, site_species_categories,
     species_categories, species_split, species_traits)
  
  # Actual Plot
  ggplot2::ggplot(
    all_coverage,
    ggplot2::aes(
      interaction(.data$avg_coverage, .data[[category_name]], sep = "__"), .data$site,
      fill = .data$coverage_value
    )
  ) +
    ggplot2::geom_tile() +
    category_facet +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::labs(x = "Trait Name", y = "Sites") +
    ggplot2::scale_x_discrete(
      labels = function(x) unlist(strsplit(x, "__.+$")),
      guide = ggplot2::guide_axis(n.dodge = 2)
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
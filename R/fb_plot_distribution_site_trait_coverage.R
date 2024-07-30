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
#' @inheritParams fb_plot_species_traits_completeness
#'
#' @return a 'ggplot2' object
#'
#' @examples
#' fb_plot_distribution_site_trait_coverage(woodiv_site_species, woodiv_traits)
#' 
#' @importFrom rlang .data
#' @export
fb_plot_distribution_site_trait_coverage <- function(
    site_species, species_traits, species_categories = NULL, all_traits = TRUE
) {
  
  # Checks
  check_site_species(site_species)
  check_species_traits(species_traits)
  check_species_categories(species_categories)
  
  full_coverage <- data.frame(site = rownames(site_species))
  
  
  # Split species by category
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
  
  # Computing Trait Coverage per Site per category
  site_categories_coverage <- lapply(
    site_species_categories,
    function(x) {
      site_cat_coverage <- fb_get_all_trait_coverages_by_site(
        x, species_traits, all_traits = all_traits
      )
      
      tidyr::pivot_longer(
        site_cat_coverage, -"site", names_to = "coverage_name",
        values_to = "coverage_value"
      )
    }
  )
  
  # Get average coverage per trait across all sites per category
  avg_coverage <- lapply(
    site_categories_coverage,
    function(single_category) {
      site_avg_coverage <- by(
      single_category, list(coverage_name = single_category[["coverage_name"]]),
      \(y) mean(y$coverage_value, na.rm = TRUE)
    )
      site_avg_coverage <- utils::stack(site_avg_coverage)
      colnames(site_avg_coverage) <- c("avg_coverage", "coverage_name")
      
      return(site_avg_coverage)
      
    }
  )
  
  # Get categories as a specific column
  avg_coverage <- lapply(
    names(avg_coverage),
    function(single_category_name) {
      
      single_category <- avg_coverage[[single_category_name]]
      single_category[[category_name]] <- single_category_name
      
      single_category[, c(3, 2, 1)]
      
    }
  )
  avg_coverage <- do.call(rbind, avg_coverage)
  
  # Get order of coverage overall
  grand_avg_coverage <- utils::stack(
    by(avg_coverage, avg_coverage$coverage_name,
       \(x) mean(x$avg_coverage, na.rm =TRUE))
  )
  
  # Order coverage categories per decreasing coverage
  avg_coverage$coverage_name <- factor(
    avg_coverage$coverage_name,
    levels = grand_avg_coverage$ind[
      order(grand_avg_coverage$values, decreasing = TRUE)
    ]
  )
  
  # Simplify categories
  site_categories_coverage <- lapply(
    names(site_categories_coverage),
    function(x) {
      
      given_coverage <- site_categories_coverage[[x]]
      
      given_coverage[category_name] <- x
      
      return(given_coverage)
    }
  )
  
  site_categories_coverage <- do.call(rbind, site_categories_coverage)
  
  site_categories_coverage$coverage_name <- factor(
    site_categories_coverage$coverage_name,
    levels = levels(avg_coverage$coverage_name)
  )
  
  if (!is_ggridges_installed()) {
    stop("This function requires 'ggridges' to work\n",
         "Please run \"install.packages('ggridges')\"", call. = FALSE)
  }
  
  # Clean environment
  rm(all_traits, site_species, species_traits, grand_avg_coverage)
  
  
  if (is.null(species_categories)) {
    
    category_facet <- NULL
    
  } else {
    
    category_facet <- ggplot2::facet_wrap(
      ggplot2::vars(
        !!rlang::sym(category_name))
    )
    
  }
  
  
  # Figure
  ggplot2::ggplot(
    site_categories_coverage,
    ggplot2::aes(.data$coverage_value, .data$coverage_name)
  ) +
    ggridges::stat_density_ridges(scale = 0.98) +
    category_facet + 
    ggplot2::scale_x_continuous(
      "Average Trait Coverage per Site", labels = scales::label_percent()
    ) +
    ggplot2::scale_y_discrete("Trait Name") +
    ggplot2::theme_bw()
}
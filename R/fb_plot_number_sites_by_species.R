#' Plot Number of Sites by Species
#' 
#' Represent all species in each function of the number of sites they occupy.
#' The species are ordered from the ones that occupy the least number of sites
#' from the ones that occupy the most.
#' The number of site is indicated at the bottom x-axis, while the top x-axis
#' represents the proportion of occupied sites.
#' The left y-axis label species names and their rank by increasing prevalence.
#' The user can supplied a threshold of sites to see how many species occupy
#' more or less than the given proportion of sites.
#'
#' @inheritParams fb_filter_species_by_site_coverage
#'
#' @return a `ggplot2` object
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' fb_plot_number_sites_by_species(site_species)
#' 
#' # Add a vertical cutoff line (40% of sites)
#' fb_plot_number_sites_by_species(site_species, threshold = 0.4)
fb_plot_number_sites_by_species <- function(
  site_species, species_categories = NULL, threshold_sites_proportion = NULL
) {
  
  # Check ----------------------------------------------------------------------
  check_site_species(site_species)
  check_species_categories(species_categories)
  
  
  if (!is.null(threshold_sites_proportion)) {
    check_threshold_proportion(threshold_sites_proportion, "site")
  }
  
  
  # Splitting species by category
  species_split <- list(single_cat = species_traits[["species"]])
  category_name <- "single_cat"
  
  if (!is.null(species_categories)) {
    
    category_name <- colnames(species_categories)[2]
    
    species_split <- split(
      species_categories[, 1], species_categories[, 2]
    )
    
  }
  
  
  # Split sites according to species' categories
  site_species_categories <- lapply(
    species_split,
    function(x) site_species[, c("site", x), drop = FALSE]
  )
  
  # Get the numbers
  number_sites_by_species <- lapply(
    site_species_categories, fb_count_sites_by_species
  )
  n_species <- lapply(site_species_categories, nrow)
  
  
  
  # Construct y-axis breaks
  # Under 25 observation, label everyone of them
  # Otherwise label 30 of them
  
  if (nrow(number_sites_by_species) <= 30) {
    
    species_y_breaks  <- number_sites_by_species$species
    species_y_numbers <- seq(1, nrow(number_sites_by_species))
    species_y_prop    <- species_y_numbers/nrow(number_sites_by_species) * 100
    
  } else {
    
    message(
      "There are more than 30 species, the y-axis will label the position ",
      "of 30 evenly spaced species (along their prevalence)"
    )
    
    species_y_breaks  <- number_sites_by_species$species[
      seq(1, nrow(number_sites_by_species), length.out = 30)
    ]
    species_y_numbers <- match(
      species_y_breaks, rev(number_sites_by_species$species)
    )
    species_y_prop    <- round(
      species_y_numbers/nrow(number_sites_by_species) * 100, 1
    )
  }
  
  number_sites_by_species$species <- factor(
    number_sites_by_species$species,
    levels = rev(number_sites_by_species$species)
  )
  
  # Manage conditional faceting
  if (is.null(species_categories)) {
    
    category_facet <- NULL
    
  } else {
    
    category_facet <- ggplot2::facet_wrap(
      ggplot2::vars(
        !!rlang::sym(category_name)), scales = "free"
    )
    
  }
  
  # Clean environment
  rm(site_species)
  
  # Actual plot
  given_plot <- ggplot2::ggplot(
    number_sites_by_species, ggplot2::aes(.data$n_sites, .data$species)
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(
      "Number of Occupied Sites",
      sec.axis = ggplot2::sec_axis(
        trans = ~./n_species, "Proportion of Occupied Sites",
        labels = scales::label_percent()
      )
    ) +
    ggplot2::scale_y_discrete(
      "Species Rank (Least to Most prevalent) and Identity",
      breaks = species_y_breaks,
      labels = paste0(species_y_numbers, " (", species_y_breaks, ")")
    ) +
    ggplot2::theme_bw()
  
  # Add threshold line underneath other layers
  if (!is.null(threshold_sites_proportion)) {
    given_plot$layers <- append(
      list(
        ggplot2::geom_vline(
          xintercept = threshold_sites_proportion * nrow(site_species),
          linetype = 2, linewidth = 1.2, color = "darkred"
        ),
        ggplot2::annotate(
          "text", x = threshold_sites_proportion * nrow(site_species),
          y = Inf, hjust = 1.1, vjust = 1.5,
          color = "darkred",
          label = paste0(
            "(n = ", round(threshold_sites_proportion * nrow(site_species)),
            ")\n(p = ",
            round(threshold_sites_proportion * 100, 1), "%)"
          )
        )
      ),
      given_plot$layers,
      after = 1
    )
  }  
  
  return(given_plot)
}
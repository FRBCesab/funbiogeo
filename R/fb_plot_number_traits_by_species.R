#' Plot Number of Traits per Species
#' 
#' Display a graph showing the number (and proportion) of species having
#' at least 0, 1, etc. number of traits. It provides a lollipop graph to examine
#' which number of trait cover a certain proportion of the species.
#' This plot doesn't show which traits are concerned.
#'
#' @inheritParams fb_filter_traits_by_species_coverage
#' @inheritParams fb_plot_species_traits_completeness
#'
#' @return a `ggplot2` object
#' 
#' @examples
#' data(species_traits)
#' 
#' fb_plot_number_traits_by_species(species_traits)
#' 
#' # Add a vertical cutoff line (33% of the species)
#' fb_plot_number_traits_by_species(
#'   species_traits, threshold_species_proportion = 1/3
#'  )
#' 
#' @importFrom rlang .data
#' @export
fb_plot_number_traits_by_species <- function(
  species_traits, species_categories = NULL, threshold_species_proportion = NULL
) {
  
  check_species_categories(species_categories)
  
  n_species <- nrow(species_traits)
  
  # Split trait along list
  species_traits_categories <- split_species_categories(
    species_traits, species_categories
  )
  
  number_trait_per_species <- lapply(
    species_traits_categories, fb_count_traits_by_species
  )
  
  # Count number of species per number of trait (XX species has YY traits)
  number_trait_per_species <- lapply(
    number_trait_per_species,
    function(x) {
      number_category <- by(x, x$n_traits, function(y) c(n = nrow(y)))
      
      number_category <- utils::stack(number_category)
      number_category$n_traits <- as.numeric(as.character(number_category$ind))
      
      # Compute number to show at least 0 or more, etc.
      number_category[["at_least"]] <- rev(
        cumsum(rev(number_category$values))
      )
      
      return(number_category)
    }
  )
  
  # Conditional faceting
  if (is.null(species_categories)) {
    
    category_facet <- NULL
    
  } else {
    
    number_trait_per_species <- lapply(
      names(number_trait_per_species),
      function(x) {
        
        single_category <- number_trait_per_species[[x]]
        
        single_category[[colnames(species_categories)[2]]] <- x
        
        return(single_category)
      }
    )
    
    category_facet <- ggplot2::facet_wrap(
      ggplot2::vars(
        !!rlang::sym(colnames(species_categories)[[2]])), scales = "free"
    )
  }
  
  number_trait_per_species <- do.call(rbind, number_trait_per_species)
  max_n_trait <- max(number_trait_per_species[["n_traits"]])
  
  
  # Clean environment
  rm(species_traits, species_traits_categories)
  
  
  # Actual plot
  given_plot <- ggplot2::ggplot(
    number_trait_per_species, ggplot2::aes(.data$at_least, .data$n_traits)
  ) +
    ggplot2::geom_point(size = 1.7) +
    ggplot2::geom_segment(
      ggplot2::aes(
        y = .data$n_traits, yend = .data$n_traits, x = 0, xend = .data$at_least
      )
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(
          round((.data$at_least/n_species) * 100, 1), "%"
        ),
        x = .data$at_least, y = .data$n_traits
      ), hjust = 0.5, vjust = -0.6, size = 3
    ) +
    category_facet +
    ggplot2::labs(x = "Number of Species", y = "Number of Traits") +
    ggplot2::scale_x_continuous(
      sec.axis = ggplot2::sec_axis(
        ~./n_species, "Proportion of Species",
        labels = scales::label_percent()
      )
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, to = max_n_trait, by = 1),
      labels = function(x) ifelse(
        x <= 1, paste0("\u2265", x, " trait"),
        ifelse(x < ncol(species_traits) - 1, paste0("\u2265", x, " traits"),
               paste0(x, " traits"))
      )
    ) +
    ggplot2::theme_bw()
  
  
  # Add threshold line underneath other layers
  if (!is.null(threshold_species_proportion)) {
    
    given_plot$layers <- append(
      list(
        ggplot2::geom_vline(
          xintercept = threshold_species_proportion * nrow(species_traits),
          linetype = 2, linewidth = 1.2, color = "darkred"
        ),
        ggplot2::annotate(
          "text", x = threshold_species_proportion * nrow(species_traits),
          y = 0.95, hjust = 1.1, color = "darkred",
          label = paste0(
            "(n = ", round(threshold_species_proportion * nrow(species_traits)),
            ")\n(p = ", round(threshold_species_proportion * 100, digits = 1),
            "%)"
          )
        )
      ),
      given_plot$layers,
      after = 1
    )
    
  }
  
  return(given_plot)
}
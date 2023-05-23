#' Plot Number of Species per Trait
#'
#' Display a lollipop graph showing the number and proportion of species
#' with non-NA trait for each trait ranked in decreasing order.
#' 
#' @inheritParams fb_filter_traits_by_species_coverage
#' @inheritParams fb_plot_species_traits_completeness
#'
#' @return a ggplot2 object
#'
#' @examples
#' data(species_traits)
#' 
#' fb_plot_number_species_by_trait(species_traits)
#' 
#' # Add a vertical cutoff line (12.5% of species)
#' fb_plot_number_species_by_trait(species_traits, NULL, 1/8)
#' 
#' @importFrom rlang .data
#' @export
fb_plot_number_species_by_trait <- function(
  species_traits, species_categories = NULL, threshold_species_proportion = NULL
) {
  
  # Checks
  check_species_categories(species_categories)
  
  species_traits_categories <- split_species_categories(
    species_traits, species_categories
  )
  
  n_species <- nrow(species_traits)
  
  number_species_per_trait <- lapply(
    species_traits_categories,
    function(x) {
      trait_coverage <- fb_count_species_by_trait(x)
      
      trait_coverage$trait <- factor(
        trait_coverage$trait, levels = rev(trait_coverage$trait)
      )
      
      trait_coverage[["prop_species"]] <- trait_coverage[["n_species"]]/nrow(x)
      
      return(trait_coverage)
    }
  )
  
  
  # Manage conditional faceting
  if (is.null(species_categories)) {
    
    category_facet <- NULL
    
  } else {
    
    number_species_per_trait <- lapply(
      names(number_species_per_trait),
      function(x) {
        
        single_category <- number_species_per_trait[[x]]
        
        single_category[[colnames(species_categories)[2]]] <- x
        
        return(single_category)
      }
    )
    
    category_facet <- ggplot2::facet_wrap(
      ggplot2::vars(
        !!rlang::sym(colnames(species_categories)[[2]])), scales = "free"
    )
    
  }
  
  number_species_per_trait <- do.call(rbind, number_species_per_trait)
  
  
  # Clean environment
  rm(species_traits, species_categories)
  
  # Add threshold line underneath other layers
  if (!is.null(threshold_species_proportion)) {
    
    vline_annotation <- list(
        ggplot2::geom_vline(
          xintercept = threshold_species_proportion * n_species,
          linetype = 2, linewidth = 1.2, color = "darkred"
        ),
        ggplot2::annotate(
          "text", x = threshold_species_proportion * n_species,
          y = 0.95, hjust = 1.1, color = "darkred",
          label = paste0(
            "(n = ", round(threshold_species_proportion * n_species),
            ")\n(p = ",
            round(threshold_species_proportion * 100, digits = 1), "%)"
          )
        )
      )
    
  } else {
    vline_annotation <- NULL
  }
  
  # Plot
  ggplot2::ggplot(
    number_species_per_trait, ggplot2::aes(.data$n_species, .data$trait)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_segment(
      ggplot2::aes(
        y = .data$trait, yend = .data$trait, x = 0, xend = .data$n_species
      )
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(round(.data$prop_species * 100, digits = 1), "%")
      ),
      hjust = 0.5, vjust = -0.6, size = 3
    ) +
    category_facet +
    vline_annotation + 
    ggplot2::scale_x_continuous(
      "Number of Species",
      sec.axis = ggplot2::sec_axis(
        trans = ~./n_species, "Proportion of Species",
        labels = scales::label_percent()
      ),
      # Add a tiny bit of space so that proportion can be shown
      limits = c(0, NA)
    ) +
    ggplot2::labs(y = "Trait Name") +
    ggplot2::theme_bw()
}
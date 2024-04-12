#' Plot Trait Coverage per Species for each Trait
#'
#' Display a binary heatmap visualizing the species x traits matrix with colors
#' displaying present and missing traits. Traits are ordered from the most to
#' the least known (left to right).
#' Species are ordered from the ones with most to the ones with least traits
#' (bottom to top). The proportion of species with non-missing traits is shown
#' on the x-axis labels.
#' An additional column at the very right of the plot named `"all_traits"`
#' shows a summary considering if all other traits are known.
#'
#' @inheritParams fb_get_all_trait_coverages_by_site
#' @param species_categories 2-columns `data.frame` giving species categories
#'   `NULL` by default, with the first column describing the species name, and
#'   the second column giving their corresponding categories
#'
#' @return a `ggplot2` object
#'
#' @examples
#' data(species_traits)
#' fb_plot_species_traits_missingness(species_traits)
#' 
#' @importFrom rlang .data
#' @export
fb_plot_species_traits_missingness <- function(
    species_traits, species_categories = NULL, all_traits = TRUE
) {
  
  check_species_categories(species_categories)
  
  # Make dataset long to get all trait values by rows
  species_traits_long <- tidyr::pivot_longer(
    species_traits, -"species", names_to = "trait",
    values_to = "trait_value", values_transform = as.character
  )
  
  
  # Split species by categories (even when there are none)
  species_traits_categories <- split_species_categories(
    species_traits, species_categories
  )
  species_traits_long_categories <- species_traits_long
  
  if (!is.null(species_categories)) {
    
    species_traits_long_categories <- merge(
      species_traits_long, species_categories,
      by.x = "species", by.y = colnames(species_categories)[1]
    )
    
  }
  
  # Count Number of Species per Trait (per category class)
  number_species_per_trait <- lapply(
    species_traits_categories,
    function(categorical_df) {
      coverage_df <- fb_count_species_by_trait(categorical_df)
      
      coverage_df[["total_species"]] <- coverage_df[["n_species"]] /
        coverage_df[["coverage"]]
      coverage_df[["missing_species"]] <- coverage_df[["total_species"]] -
        coverage_df[["n_species"]]
      
      return(coverage_df)
    }
  )
  
  # Get Combination for All Traits (per category class)
  number_trait_per_species <- lapply(
    species_traits_categories, fb_count_traits_by_species
  )
  
  n_max_trait <- ncol(species_traits[, -1, drop = FALSE])
  
  # Get number of species with maximum known trait (per category)
  all_traits_list <- lapply(
    number_trait_per_species,
    function(x) {
      
      with(x, sum(n_traits == n_max_trait))
      
    }
  )
  
  
  # Rename list if they don't have names (e.g., no categories specified)
  if (is.null(names(number_trait_per_species))) {
    names(number_trait_per_species) <- seq_len(
      length(number_trait_per_species)
    )
    names(number_species_per_trait) <- seq_len(
      length(number_species_per_trait)
    )
  }
  
  # Convert number of species with all traits into comparable row
  all_traits_df <- list(number_species_per_trait[[1]][0,])
  
  if (all_traits) {
    
    all_traits_df <- mapply(
      function(x, y) {
        data.frame(
          trait     = "all_traits",
          n_species = x,
          coverage  = x/nrow(y),
          total_species = nrow(y),
          missing_species = nrow(y) - x
        )
      }, all_traits_list, species_traits_categories, SIMPLIFY = FALSE
    )
    
  }
  
  # Add number of species with all traits known per category
  number_species_per_trait <- mapply(
    rbind, number_species_per_trait, all_traits_df, SIMPLIFY = FALSE
  )
  
  # Label with name of trait and proportion of species
  number_species_per_trait <- lapply(
    number_species_per_trait, 
    function(x) {
      x$trait_label <- paste0(
        x$trait, "\n(", round(x$coverage * 100, digits = 1), "%)"
      )
      
      return(x)
    }
  )
  
  # Conditional facet
  if (!is.null(species_categories)) {
    
    category_facet <- ggplot2::facet_wrap(
      ggplot2::vars(
        !!rlang::sym(colnames(species_categories)[[2]])), scales = "free"
    )
    
    number_species_per_trait <- lapply(
      names(number_species_per_trait),
      function(x) {
        
        single_category <- number_species_per_trait[[x]]
        
        single_category[[colnames(species_categories)[2]]] <- x
        
        return(single_category)
      }
    )
    
  } else {
    category_facet <- NULL
  }
  
  
  number_species_per_trait <- do.call(rbind, number_species_per_trait)
  
  # Clean environment for clean ggplot2 object
  rm(all_traits, all_traits_df, all_traits_list, n_max_trait,
     number_trait_per_species, species_traits, species_traits_categories,
     species_traits_long)
  
  number_species_per_trait <- tidyr::pivot_longer(
    number_species_per_trait[, c(1, 2, 4)], -trait
  )
  
  # Plot Species x Trait completeness
  ggplot2::ggplot(
    number_species_per_trait,
    ggplot2::aes(
      .data$value,
      factor(
        .data$trait, levels = unique(.data$trait)
      ),
      fill = .data$name
    )
  ) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$value),
      position = ggplot2::position_fill(vjust = 0.5), color = "white"
    ) +
    category_facet +
    ggplot2::scale_x_continuous("Proportion of species", labels = scales::label_percent()) +
    ggplot2::scale_y_discrete("Trait") +
    ggplot2::scale_fill_manual(
      "Known Trait?",
      values = c(`missing_species` = "#E41A1C", `n_species` = "#377EB8"),
      labels = c(`missing_species` = "No", `n_species` = "Yes")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "top"
    )
  
}
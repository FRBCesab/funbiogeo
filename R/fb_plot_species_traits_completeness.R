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
#' @param species_categories (default = `NULL`) 2-columns `data.frame` giving
#'   species categories, with the first column describing the species name, and
#'   the second column giving their corresponding categories
#'
#' @return a `ggplot2` object
#'
#' @examples
#' data(species_traits)
#' fb_plot_species_traits_completeness(species_traits)
#' 
#' @importFrom rlang .data
#' @export
fb_plot_species_traits_completeness <- function(
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
    species_traits_categories, fb_count_species_by_trait
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
          coverage  = x/nrow(y)
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
  
  
  # Add all traits in long table
  all_traits_subset <- lapply(
    names(number_trait_per_species), function(x) {
      
      single_category <- number_trait_per_species[[x]]
      
      output_df <- data.frame(
        species = single_category$species, trait = "all_traits",
        trait_value = ifelse(single_category$n_traits == n_max_trait, TRUE, NA)
      )
      
      if (!is.null(species_categories)) {
        output_df[[colnames(species_categories)[2]]] <- x
      }
      
      return(output_df)
    }
  )
  
  all_traits_subset <- do.call(rbind, all_traits_subset)
  
  species_traits_long_categories <- rbind(
    species_traits_long_categories, all_traits_subset
  )
  
  
  # Add column for value
  species_traits_long_categories$has_trait <- ifelse(
    !is.na(species_traits_long_categories$trait_value) &
      !(species_traits_long_categories$trait_value == "NaN"), TRUE, FALSE
  )
  
  # Merge all datasets before plotting
  number_species_per_trait <- lapply(
    names(number_species_per_trait),
    function(x) {
      
      single_category <- number_species_per_trait[[x]]
      
      if (!is.null(species_categories)) {
        single_category[[colnames(species_categories)[2]]] <- x
      }
      
      return(single_category)
    }
  )
  number_species_per_trait <- do.call(rbind, number_species_per_trait)
  
  number_trait_per_species <- lapply(
    names(number_trait_per_species),
    function(x) {
      
      single_category <- number_trait_per_species[[x]]
      
      if (!is.null(species_categories)) {
        single_category[[colnames(species_categories)[2]]] <- x
      }
      
      return(single_category)
    }
  )
  number_trait_per_species <- do.call(rbind, number_trait_per_species)
  
  
  # Merge full dataset
  common_colnames <- intersect(
    colnames(species_traits_long_categories), colnames(number_species_per_trait)
  )
  
  species_traits_long_categories <- merge(
    species_traits_long_categories, number_species_per_trait,
    by.x = common_colnames, by.y = common_colnames
  )
  
  # Conditional facet
  if (!is.null(species_categories)) {
    
    category_facet <- ggplot2::facet_wrap(
      ggplot2::vars(
        !!rlang::sym(colnames(species_categories)[[2]])), scales = "free"
    )
    
  } else {
    category_facet <- NULL
  }
  
  # Clean environment for clean ggplot2 object
  rm(all_traits, all_traits_df, all_traits_list, all_traits_subset,
     common_colnames, n_max_trait, species_traits_categories, species_traits,
     species_traits_long)
  
  # Plot Species x Trait completeness
  ggplot2::ggplot(
    species_traits_long_categories,
    ggplot2::aes(
      factor(
        .data$trait_label, levels = unique(number_species_per_trait$trait_label)
      ),
      factor(
        .data$species, levels = unique(number_trait_per_species$species)
      )
    )
  ) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$has_trait)) +
    category_facet +
    ggplot2::scale_x_discrete(
      "Trait", guide = ggplot2::guide_axis(n.dodge = 2)
    ) +
    ggplot2::scale_y_discrete("Species", labels = NULL) +
    ggplot2::scale_fill_manual(
      "Known Trait?",
      values = c(`FALSE` = "#E41A1C", `TRUE` = "#377EB8"),
      labels = c(`FALSE` = "No", `TRUE` = "Yes")
    ) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "top"
    )
}
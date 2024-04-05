#' Plot Trait Correlation Matrix
#' 
#' This functions outputs a `ggplot2` figure that outputs a trait correlation
#' matrix. It only works on numerical traits and will output messages if
#' non-numerical traits are found and errors if no numerical traits are found.
#' Internally it uses the [stats::cor()] function and only works on complete
#' observation (it removes any row that contains any `NA`).
#' Use the `...` argument to pass options to the `cor()` function.
#'
#' @inheritParams fb_get_trait_coverage_by_site 
#' @inheritParams fb_plot_species_traits_completeness
#' @param ... Additional options passed to [stats::cor()]
#'
#' @return a `ggplot` object
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' fb_plot_trait_correlation(species_traits)
#' 
#' # Plot Spearman's correlation
#' fb_plot_trait_correlation(species_traits, method = "spearman")
fb_plot_trait_correlation <- function(
    species_traits, species_categories = NULL, ...
) {
  
  # Checks ---------------------------------------------------------------------
  
  check_species_traits(species_traits)
  check_species_categories(species_categories)
  
  # Keep only numeric columns
  numerical_traits <- vapply(
    species_traits[, -1, drop = FALSE], is.numeric, logical(1)
  )
  
  if (!any(numerical_traits)) {
    stop("No numerical traits found, cannot plot trait correlations",
         call. = FALSE)
  }
  
  if (!all(numerical_traits)) {
    message("Non-numerical traits found, only keeping numerical traits ",
            "to display trait correlations")
  }
  
  # Subset Traits --------------------------------------------------------------
  
  trait_subset <- species_traits[
    , c(
      colnames(species_traits)[1], colnames(species_traits)[-1][numerical_traits]
    ), drop = FALSE
  ]
  
  species_traits_categories <- split_species_categories(
    trait_subset, species_categories
  )
  
  
  # Compute correlation --------------------------------------------------------
  
  trait_cor <-  lapply(species_traits_categories, function(x, ...) {
    
    individual_cor <- as.data.frame(
      stats::cor(
        x[,-1, drop = FALSE], use = "complete.obs", ...
      )
    )
    
    individual_cor[["first_trait"]] <- rownames(individual_cor)
    
    tidyr::pivot_longer(
      individual_cor, -"first_trait", names_to = "second_trait",
      values_to = "trait_cor"
    )
    
  })
  
  
  # Manage conditional faceting
  if (is.null(species_categories)) {
    
    category_facet <- NULL
    
  } else {
    
    trait_cor <- lapply(
      names(trait_cor),
      function(x) {
        
        single_category <- trait_cor[[x]]
        
        single_category[[colnames(species_categories)[2]]] <- x
        
        return(single_category)
      }
    )
    
    category_facet <- ggplot2::facet_wrap(
      ggplot2::vars(
        !!rlang::sym(colnames(species_categories)[[2]])), scales = "free"
    )
    
  }
  
  trait_cor <- do.call(rbind, trait_cor)
  
  
  # Clean environment
  rm(species_traits, species_categories, species_traits_categories)
  
  
  # Actual Figure --------------------------------------------------------------
  
  ggplot2::ggplot(
    trait_cor,
    ggplot2::aes(.data$first_trait, .data$second_trait, fill = .data$trait_cor)
  ) +
    ggplot2::geom_raster() +
    ggplot2::geom_text(
      ggplot2::aes(label = round(.data$trait_cor, digits = 2))
    ) +
    category_facet +
    # Re-order traits to have a matrix corresponding to what we compute
    ggplot2::scale_x_discrete(
      limits = unique(trait_cor$first_trait)
    ) +
    ggplot2::scale_y_discrete(
      limits = rev(unique(trait_cor$first_trait))
    ) +
    # Cosmetic changes
    ggplot2::scale_fill_distiller(
      "Trait Correlation", palette = "PuOr", limits = c(-1, 1)
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      aspect.ratio = 1,
      legend.position ="top"
    )
}
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
#' 
#' @param ... Additional options passed to [stats::cor()]
#'
#' @return a `ggplot` object
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' fb_plot_trait_correlation(species_traits)
fb_plot_trait_correlation <- function(species_traits, ...) {
  
  # Checks ---------------------------------------------------------------------
  
  check_species_traits(species_traits)
  
  # Keep only numeric columns
  trait_type <- vapply(species_traits[, -1, drop = FALSE], typeof, character(1))
  
  numerical_traits <- trait_type %in% c("double", "integer")
  
  if (!any(numerical_traits)) {
    stop("No numerical traits found, cannot plot trait correlations")
  }
  
  if (!all(numerical_traits)) {
    message("Non-numerical traits found, only keeping numerical traits ",
            "to show trait correlation")
  }
  
  # Subset Traits --------------------------------------------------------------
  
  trait_subset <- species_traits[, -1, drop = FALSE][
    , numerical_traits, drop = FALSE
  ]
  
  
  # Compute correlation --------------------------------------------------------
  
  trait_cor <-  as.data.frame(
    stats::cor(trait_subset, use = "complete.obs", ...)
  )
  
  trait_cor[["first_trait"]] <- rownames(trait_cor)
  
  trait_cor <- tidyr::pivot_longer(
    trait_cor, -"first_trait", names_to = "second_trait",
    values_to = "trait_cor"
  )
  
  
  # Actual Figure --------------------------------------------------------------
  
  ggplot2::ggplot(
    trait_cor,
    ggplot2::aes(.data$first_trait, .data$second_trait, fill = .data$trait_cor)
  ) +
    ggplot2::geom_raster() +
    ggplot2::geom_text(
      ggplot2::aes(label = round(.data$trait_cor, digits = 2))
    ) +
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
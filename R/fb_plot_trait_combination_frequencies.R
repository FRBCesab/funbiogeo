#' Plot Present and Missing Trait Combinations Frequencies
#'
#' Display a figure of Present/Missing Trait Combinations where one square is
#' represents one trait. The y-axis gives the frequency of the row
#' (as well as its proportions compared to the total number of rows).
#'
#' @inheritParams fb_plot_species_traits_completeness
#' @param order_by {`character(1)` either `"number"` or `"complete`}\cr{}
#'                 If `"number"` order rows by frequency so that most
#'                 frequent rows are at the bottom.
#'                 Otherwise order rows to order table by the number of
#'                 non-missing traits then by the frequency of combinations
#'
#' @return a `ggplot2` object
#' 
#' @examples
#' # Ordered by number by default
#' fb_plot_trait_combination_frequencies(species_traits)
#' 
#' # Order by present traits
#' fb_plot_trait_combination_frequencies(species_traits, "complete")
#'
#' @export
#' @importFrom rlang .data
fb_plot_trait_combination_frequencies = function(
    species_traits, order_by = c("number", "complete")
) {
  
  # Check arguments
  check_species_traits(species_traits)
  order_by <- match.arg(order_by)
  
  # Get data with combinations of values
  combinations <- as.data.frame(
    !is.na(species_traits[, -1, drop = FALSE])
  )
  
  # Get unique combinations and their count
  unique_combinations <- stats::aggregate(
    comb_count ~ ., cbind(combinations, comb_count = 1), FUN = sum
  )
  
  # Count number of present traits
  number_traits <- rowSums(
    unique_combinations[, seq(1, ncol(unique_combinations) - 1), drop = FALSE]
  )
  
  unique_combinations$n_present <- number_traits
  
  # Order dataset based on 'order_by' argument
  if (order_by == "number") {
    
    # Order table by decreasing number of combinations
    unique_combinations <- unique_combinations[
      order(unique_combinations$comb_count, decreasing = TRUE),
    ]
    
  } else {
    
    
    
    # Order rows columns with most non-missing traits first then by count
    unique_combinations <- unique_combinations[
      order(unique_combinations$n_present, unique_combinations$comb_count,
            decreasing = TRUE),
    ]
    
  }
  
  # Get row order
  unique_combinations[["row_order"]] <- seq(nrow(unique_combinations))
  
  # Make table longer
  unique_comb_long <- tidyr::pivot_longer(
    unique_combinations, !c("comb_count", "row_order", "n_present"),
    names_to = "trait_name", values_to = "trait_value"
  )
  
  ggplot2::ggplot(
    unique_comb_long, ggplot2::aes(.data$trait_name, factor(.data$row_order))
  ) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = .data$trait_value), color = "white"
    ) +
    ggplot2::scale_y_discrete(
      "Number and Proportion of Species",
      labels = paste0(
        "n = ", unique_combinations$comb_count," (",
        round(unique_combinations$comb_count / nrow(species_traits) * 100, 1),
        "%)"
      )
    ) +
    ggplot2::scale_fill_brewer(
      "Trait", palette = "Set1",
      labels = c(`TRUE` = "Present", `FALSE` = "Missing")
    ) +
    ggplot2::scale_x_discrete(
      "Trait Name", guide = ggplot2::guide_axis(n.dodge = 2)
    ) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")
}
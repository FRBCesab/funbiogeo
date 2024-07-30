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
#' fb_plot_trait_combination_frequencies(woodiv_traits)
#' 
#' # Order by present traits
#' fb_plot_trait_combination_frequencies(woodiv_traits, NULL, "complete")
#'
#' @export
#' @importFrom rlang .data
fb_plot_trait_combination_frequencies = function(
    species_traits, species_categories = NULL,
    order_by = c("number", "complete")
) {
  
  # Check arguments
  check_species_traits(species_traits)
  check_species_categories(species_categories)
  order_by <- match.arg(order_by)
  
  # Split species traits
  species_traits_categories <- split_species_categories(
    species_traits, species_categories
  )
  
  # Get data with combinations of values
  combinations <- lapply(
    species_traits_categories, function(x) {
      as.data.frame(
        !is.na(x[, -1, drop = FALSE])
      )
    }
  )
  
  # Get unique combinations and their count
  unique_combinations <- lapply(
    combinations, function(x) {
      stats::aggregate(
        comb_count ~ ., cbind(x, comb_count = 1), FUN = sum
      )
    }
  )
  
  # Count number of present traits
  unique_combinations <- lapply(
    unique_combinations, function(x) {
      n_traits <- rowSums(
        x[, seq(1, ncol(x) - 1), drop = FALSE]
      )
      
      x$n_present <- n_traits
      
      return(x)
    }
  )
  
  
  # Order dataset based on 'order_by' argument
  if (order_by == "number") {
    
    # Order table by decreasing number of combinations
    unique_combinations <- lapply(
      unique_combinations, function(x) {
        x[order(x$comb_count, decreasing = TRUE),]
      }
    )
    
  } else {
    
    # Order rows columns with most non-missing traits first then by count
    unique_combinations <- lapply(
      unique_combinations, function(x) {
        x[order(x$n_present, x$comb_count, decreasing = TRUE),]
      }
    )
    
  }
  
  # Get row order
  unique_combinations <- lapply(
    unique_combinations, function(x) {
      x[["row_order"]] <- seq(nrow(x))
      
      return(x)
    }
  )
  
  # Make table longer
  unique_comb_long <- mapply(
    function(x, y) {
      
      x$trait_label <- paste0(
        "n = ", x$comb_count," (",
        round(x$comb_count / nrow(y) * 100, 1), "%)"
      )
      
      # To disambiguate labels throughout- table
      x$unique_label <- seq(nrow(x))
      
      long_df <- tidyr::pivot_longer(
        x, !c("comb_count", "row_order", "n_present", "trait_label",
              "unique_label"),
        names_to = "trait_name", values_to = "trait_value"
      )
      
      return(long_df)
    }, unique_combinations, species_traits_categories, SIMPLIFY = FALSE
  )
  
  # Manage conditional faceting
  if (is.null(species_categories)) {
    
    category_facet <- NULL
    
  } else {
    
    unique_comb_long <- lapply(
      names(unique_comb_long),
      function(x) {
        
        single_category <- unique_comb_long[[x]]
        
        single_category[[colnames(species_categories)[2]]] <- x
        
        return(single_category)
      }
    )
    
    category_facet <- ggplot2::facet_wrap(
      ggplot2::vars(
        !!rlang::sym(colnames(species_categories)[[2]])), scales = "free"
    )
    
  }
  
  unique_comb_long <- do.call(rbind, unique_comb_long)
  
  
  # Tidy up environment
  rm(combinations, order_by, species_categories, species_traits,
     species_traits_categories, unique_combinations)
  
  
  # Actual Plot
  ggplot2::ggplot(
    unique_comb_long,
    ggplot2::aes(
      .data$trait_name, interaction(.data$trait_label, .data$unique_label)
    )
  ) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = .data$trait_value), color = "white"
    ) +
    category_facet +
    ggplot2::scale_y_discrete(
      "Number and Proportion of Species",
      labels = function(x) unlist(strsplit(x, "\\.[0-9]+$"))
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
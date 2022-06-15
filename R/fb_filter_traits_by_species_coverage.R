#' Filter traits with a given species coverage threshold
#' 
#' @description
#' Selects traits (columns) for which the percentage of species without 
#' `NA` (missing trait values) is higher than a threshold.
#' 
#' @param threshold_species_proportion a numeric of length 1 between 0 and 1.
#' The percentage of species coverage threshold.
#' 
#' @inheritParams fb_get_coverage
#'
#' @return A subset of `species_traits` with traits for the specified
#'   proportion of species.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("species_traits")
#' 
#' species_traits <- fb_filter_traits_by_species_coverage(
#'   species_traits,
#'   threshold_species_proportion = 0.6)

fb_filter_traits_by_species_coverage <- function(
    species_traits, 
    threshold_species_proportion = 0
) {
  
  ## Check inputs ----
  
  if (missing(species_traits)) {
    stop("Argument 'species_traits' (species x traits data frame) is required")
  }
  
  check_species_traits(species_traits)
  
  if (!is.numeric(threshold_species_proportion) | 
      threshold_species_proportion > 1 | threshold_species_proportion < 0) {
    stop("Coverage threshold should be a numeric value >= 0 and <= 1",
         call. = FALSE)
  }
  
  
  # Check for absence of variability in traits ----
  
  n_modalities  <- unlist(
    lapply(
      colnames(species_traits)[-1], function(x) {
        length(unique(species_traits[[x]][!is.na(species_traits[[x]])]))
      }
    )
  )
  
  only_na_traits <- colnames(species_traits)[-1][which(n_modalities == 0)]
  
  if (length(only_na_traits) > 0) {
    message("Some traits have only NA values. ", 
            "Maybe you would like to remove them.")
  }
  
  unique_traits <- colnames(species_traits)[-1][which(n_modalities == 1)]
  
  if (length(unique_traits) > 0) {
    message("Some traits have no variability (one single value). ", 
            "Maybe you would like to remove them.")
  }
  
  
  # Get species coverage for each trait ----
  
  species_coverage <- fb_count_species_by_traits(species_traits)
  
  
  # Filter traits by species coverage ----
  
  selected_traits <- species_coverage[
    which(species_coverage[["coverage"]] >= threshold_species_proportion), 
    "trait"]
  
  
  if (identical(selected_traits, character(0))) {
    message("No trait has the specified species coverage threshold")
  }
  
  species_traits[, c(colnames(species_traits)[1], selected_traits), 
                  drop = FALSE]
}

#' Filter species with a given traits coverage threshold
#' 
#' @description
#' Selects species (rows) for which the percentage of traits without 
#' `NA` (missing trait values) is higher than a threshold.
#' 
#' @param threshold_traits_proportion a numeric of length 1 between 0 and 1.
#'   The percentage of traits coverage threshold.
#' 
#' @inheritParams fb_get_trait_coverage_by_site
#'
#' @return A subset of `species_traits` with species covered by X% of traits.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("species_traits")
#' 
#' species_traits <- fb_filter_species_by_trait_coverage(
#'   species_traits,
#'   threshold_traits_proportion = 0.6)

fb_filter_species_by_trait_coverage <- function(
    species_traits, threshold_traits_proportion = 0
  ) { 
  
  # Check inputs
  check_species_traits(species_traits)
  
  if (!is.numeric(threshold_traits_proportion) | 
      threshold_traits_proportion > 1 | threshold_traits_proportion < 0) {
    stop("Coverage threshold should be a numeric value >= 0 and <= 1",
         call. = FALSE)
  }
  
  
  # Check for absence of variability in traits ----
  
  n_modalities  <- apply(species_traits[ , -1], 1, function(x)
    length(unique(x[!is.na(x)])))
  
  only_na_traits <- species_traits[which(n_modalities == 0), 1]
  
  if (length(only_na_traits) > 0) {
    message("Some species have only NA values for all traits. ", 
            "Maybe you would like to remove them.")
  }
  
  
  # Get species coverage for each trait ----
  
  traits_coverage <- fb_count_traits_by_species(species_traits)
  
  
  # Filter traits by species coverage ----
  
  selected_species <- traits_coverage[
    which(traits_coverage[["coverage"]] >= threshold_traits_proportion), 
    "species"]
  
  
  returned_traits <- species_traits[
    species_traits[ , 1] %in% selected_species, , drop = FALSE
  ]
  
  if (identical(selected_species, character(0))) {
    message("No species has the specified traits coverage threshold")
    
    returned_traits <- species_traits[NULL, ]
  }
  
  return(returned_traits)
}

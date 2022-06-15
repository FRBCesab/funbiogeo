#' Compute species coverage for each trait
#' 
#' @description
#' For each trait computes the percentage of species without `NA` (missing 
#' trait values).
#' 
#' @inheritParams fb_get_coverage
#'
#' @return A three-column `data.frame` with:
#' - `trait`: the name of the trait;
#' - `n_species`: the number of species with non-missing value for the trait;
#' - `coverage`: the percentage of species with non-missing value for the trait.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("species_traits")
#' 
#' species_coverage_by_trait <- fb_count_species_by_traits(species_traits)

fb_count_species_by_traits <- function(species_traits) {
  
  ## Check inputs ----
  
  if (missing(species_traits)) {
    stop("Argument 'species_traits' (species x traits data frame) is required")
  }
  
  check_species_traits(species_traits)
  
  
  ## Compute species coverage by trait ----
  
  species_coverage <- unlist(lapply(colnames(species_traits)[-1], function(x) 
    sum(!is.na(species_traits[[x]]))))
  
  species_coverage <- data.frame(
    "trait"      = colnames(species_traits)[-1],
    "n_species"  = species_coverage,
    "coverage"   = species_coverage / nrow(species_traits))
  
  species_coverage <- species_coverage[order(species_coverage$"coverage", 
                                             decreasing = TRUE), ]
  
  rownames(species_coverage) <- NULL
  
  species_coverage
}



#' Filter traits with a given species coverage threshold
#' 
#' @description
#' Selects traits (columns) for which the percentage of species without 
#' `NA` (missing trait values) is higher than a threshold.
#' 
#' @param threshold a numeric of length 1 between 0 and 1. The percentage of 
#' species coverage threshold.
#' 
#' @inheritParams fb_get_coverage
#'
#' @return A subset of `species_traits` with traits covered by X% of species.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("species_traits")
#' 
#' species_traits <- fb_filter_traits_by_species_coverage(species_traits,
#'                                                        threshold = 0.6)

fb_filter_traits_by_species_coverage <- function(species_traits, 
                                                 threshold = 0) {
  
  ## Check inputs ----
  
  if (missing(species_traits)) {
    stop("Argument 'species_traits' (species x traits data frame) is required")
  }
  
  check_species_traits(species_traits)
  
  if (!is.numeric(threshold) | threshold > 1 | threshold < 0) {
    stop("Coverage threshold should be a numeric value >= 0 and <= 1",
         call. = FALSE)
  }
  
  
  # Check for absence of variability in traits ----
  
  n_modalities  <- unlist(lapply(colnames(species_traits)[-1], function(x) 
    length(unique(species_traits[[x]][!is.na(species_traits[[x]])]))))
  
  only_na_traits <- colnames(species_traits)[-1][which(n_modalities == 0)]
  
  if (length(only_na_traits)) {
    message("Some traits have only NA values. ", 
            "Maybe you would like to remove them.")
  }
  
  unique_traits <- colnames(species_traits)[-1][which(n_modalities == 1)]
  
  if (length(unique_traits)) {
    message("Some traits have no variability (one single value). ", 
            "Maybe you would like to remove them.")
  }
  
  
  # Get species coverage for each trait ----
  
  species_coverage <- fb_count_species_by_traits(species_traits)
  
  
  # Filter traits by species coverage ----
  
  selected_traits <- species_coverage[
    which(species_coverage[["coverage"]] >= threshold), "trait"]
  
  
  if (identical(selected_traits, character(0))) {
    message("No trait has the specified species coverage threshold")
  }
  
  species_traits[ , c(colnames(species_traits)[1], selected_traits), 
                  drop = FALSE]
}



#' Compute traits coverage for each species
#' 
#' @description
#' For each species computes the percentage of traits without `NA` (missing 
#' trait values).
#' 
#' @inheritParams fb_get_coverage
#'
#' @return A three-column `data.frame` with:
#' - `species`: the name of the species;
#' - `n_traits`: the number of traits with non-missing value for the species;
#' - `coverage`: the percentage of traits with non-missing value for the 
#' species.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("species_traits")
#' 
#' trait_coverage_by_species <- fb_count_traits_by_species(species_traits)

fb_count_traits_by_species <- function(species_traits) { 

  ## Check inputs ----
  
  if (missing(species_traits)) {
    stop("Argument 'species_traits' (species x traits data frame) is required")
  }
  
  check_species_traits(species_traits)
  
  
  ## Compute traits coverage by species ----
  
  traits_coverage <- apply(species_traits[ , -1], 1, function(x)
    sum(!is.na(x)))
  
  traits_coverage <- data.frame(
    "species"    = species_traits[ , 1],
    "n_traits"   = traits_coverage,
    "coverage"   = traits_coverage / (ncol(species_traits) - 1))
  
  traits_coverage <- traits_coverage[order(traits_coverage$"coverage", 
                                           decreasing = TRUE), ]
  
  rownames(traits_coverage) <- NULL
  
  traits_coverage
}



#' Filter species with a given traits coverage threshold
#' 
#' @description
#' Selects species (rows) for which the percentage of traits without 
#' `NA` (missing trait values) is higher than a threshold.
#' 
#' @param threshold a numeric of length 1 between 0 and 1. The percentage of 
#' traits coverage threshold.
#' 
#' @inheritParams fb_get_coverage
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
#' species_traits <- fb_filter_species_by_traits_coverage(species_traits,
#'                                                        threshold = 0.6)

fb_filter_species_by_traits_coverage <- function(species_traits, 
                                                 threshold = 0) { 

  ## Check inputs ----
  
  if (missing(species_traits)) {
    stop("Argument 'species_traits' (species x traits data frame) is required")
  }
  
  check_species_traits(species_traits)
  
  if (!is.numeric(threshold) | threshold > 1 | threshold < 0) {
    stop("Coverage threshold should be a numeric value >= 0 and <= 1",
         call. = FALSE)
  }
  
  
  # Check for absence of variability in traits ----
  
  n_modalities  <- apply(species_traits[ , -1], 1, function(x)
    length(unique(x[!is.na(x)])))
  
  only_na_traits <- species_traits[which(n_modalities == 0), 1]
  
  if (length(only_na_traits)) {
    message("Some species have only NA values for all traits. ", 
            "Maybe you would like to remove them.")
  }
  
  
  # Get species coverage for each trait ----
  
  traits_coverage <- fb_count_traits_by_species(species_traits)
  
  
  # Filter traits by species coverage ----
  
  selected_species <- traits_coverage[
    which(traits_coverage[["coverage"]] >= threshold), "species"]
  
  
  if (identical(selected_species, character(0))) {
    message("No species has the specified traits coverage threshold")
  }
  
  species_traits[species_traits[ , 1] %in% selected_species, , drop = FALSE]
}

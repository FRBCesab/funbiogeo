#' Filter species with a given sites coverage threshold
#' 
#' @description
#' Selects species (columns) for which the percentage of sites where the 
#' species is present (distribution value higher than 0 and non-NA) is higher 
#' than a threshold.
#' 
#' @param threshold_sites_proportion a numeric of length 1 between 0 and 1.
#'   The percentage of sites coverage threshold.
#' 
#' @inheritParams fb_get_trait_coverage_by_site
#'
#' @return A subset of `site_species` with species with a prevalence higher 
#' than `threshold_sites_proportion`.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("site_species")
#' 
#' site_species <- fb_filter_species_by_site_coverage(
#'   site_species,
#'   threshold_sites_proportion = 0.1)

fb_filter_species_by_site_coverage <- function(
    site_species, threshold_sites_proportion = 0
) { 
  
  ## Check inputs ----
  
  check_site_species(site_species)
  
  if (!is.numeric(threshold_sites_proportion) | 
      threshold_sites_proportion > 1 | threshold_sites_proportion < 0) {
    stop("Coverage threshold should be a numeric value >= 0 and <= 1",
         call. = FALSE)
  }
  
  
  # Check for missing species ----
  
  unknown_species <- apply(site_species[ , -1], 2, function(x)
    length(unique(x[!is.na(x) & x > 0])))
  
  unknown_species <- names(unknown_species)[which(unknown_species == 0)]
  
  if (length(unknown_species) > 0) {
    message("Some species are absent from the study area. ", 
            "Maybe you would like to remove them.")
  }
  
  
  # Get sites coverage for each species ----
  
  sites_coverage <- fb_count_sites_by_species(site_species)
  
  
  # Filter species by site coverage ----
  
  selected_species <- sites_coverage[
    which(sites_coverage[["coverage"]] >= threshold_sites_proportion), 
    "species"]
  
  if (identical(selected_species, character(0))) {
    message("All species are absent from the study area")
  }
  
  site_species[ , c("site", selected_species), drop = FALSE]
}

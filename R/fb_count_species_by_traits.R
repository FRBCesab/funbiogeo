#' Compute species coverage for each trait
#' 
#' @description
#' For each trait computes the percentage of species without `NA` (missing 
#' trait values).
#' 
#' @inheritParams fb_get_trait_coverage_by_site
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

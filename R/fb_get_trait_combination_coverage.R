#' Compute site trait coverage for each trait combination
#' 
#' This function computes trait coverage for each site for different trait
#' combinations. If not provided, consider all possible trait combinations.
#' The function will not run if the total number of combinations given is over
#' 10,000.
#'
#' @inheritParams fb_get_trait_coverage_by_site
#' @param comb_size an integer vector defining one or more sizes of combinations
#'   (default: `NULL`)
#'
#' @return
#'   a data.frame with the following columns:
#'   
#'   * `site` with the site indices from `site_species`,
#'   * `combination_length` with the number of traits in given combinations,
#'   * `combination_name` with the name of the trait combination (concatenated
#'     trait names with `__`),
#'   * `trait_coverage` the corresponding trait coverage for the given
#'     trait combination and site.
#' 
#' @export
#'
#' @examples
#' # Compute Coverages using All Trait Combinations
#' all_combinations = fb_get_trait_combination_coverage(
#' site_species, species_traits
#' )
#' 
#' # Get only combinations of 3 traits
#' three_traits = fb_get_trait_combination_coverage(
#' site_species, species_traits, 3
#' )
#' 
#' # Combinations of 2, 3, and 4 traits
#' two_to_four = fb_get_trait_combination_coverage(
#' site_species, species_traits, c(2, 3, 4)
#' )
fb_get_trait_combination_coverage = function(
    site_species, species_traits, comb_size = NULL
) {
  
  # Checks
  check_site_species(site_species)
  check_species_traits(species_traits)
  
  if (!is.null(comb_size) & !is.numeric(comb_size)) {
    stop("Target combination length 'comb_size' should be numeric")
  }
  
  # Check for dimensionality issue
  n_traits = ncol(species_traits) - 1
  
  target_combs = seq(n_traits)
  
  n_comb = sum(
    sapply(seq(n_traits), function(x) choose(n_traits, x))
  )
  
  if (!is.null(comb_size)) {
    n_comb = sum(
      sapply(comb_size, function(x) choose(n_traits, x))
    )
    
    target_combs = comb_size
  }
  
  if (n_comb > 1e4) {
    stop("More than 10,000 combinations detected. Function will not proceed.\n",
         "Use argument 'comb_size' to provide target combination size(s)")
  }
  
  # Generate all combinations of trait to use
  traits = colnames(species_traits)[-1]
  
  all_combinations = lapply(
    target_combs,
    function(x) {
      combn(traits, x, simplify = FALSE)
    }
  )
  
  all_combinations = unlist(all_combinations, recursive = FALSE)
  
  
  # Compute trait coverages across all combinations
  trait_coverages = lapply(
    all_combinations,
    function(combination) {
      
      trait_coverage = fb_get_trait_coverage_by_site(
        site_species, species_traits[, c("species", combination)]
      )
      
      combination_length = length(combination)
      
      combination_name = paste(combination, collapse = "__")
      
      # Add Info on Given Combination
      trait_coverage$combination_length = combination_length
      trait_coverage$combination_name   = combination_name
      
      # Reorder columns
      trait_coverage[
        , c("site", "combination_length", "combination_name", "trait_coverage")
      ]
    }
  )
  
  # Bind all obtained trait coverages
  do.call(rbind.data.frame, trait_coverages)
}
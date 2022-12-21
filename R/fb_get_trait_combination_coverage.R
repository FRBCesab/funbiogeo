#' Compute site trait coverage for each trait combination
#' 
#' 
#' 
fb_get_trait_combination_coverage = function(
    site_species, species_traits, comb_size = NULL
) {
  
  # Checks
  check_site_species(site_species)
  check_species_traits(species_traits)
  
  if (!is.null(comb_size) & !is.numeric(comb_size)) {
    stop("Target combination length should be numeric")
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
    stop("More than 10,000 combinations detected. Function will not proceed.",
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
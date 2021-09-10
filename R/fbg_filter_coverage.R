#' Filter sites with a given trait threshold
#'
#' @param site_sp 
#' @param traits 
#' @param coverage_threshold 
#'
#' @return sites covered by X% of abundance/coverage consider all provided traits
#' @export
#'
#' @examples
fbg_filter_coverage = function(site_sp, traits, coverage_threshold = 1) {
  # Check input format of site-species matrix
  check_site_sp(site_sp)
  
  # Check input format of species-traits matrix
  check_traits(traits)
  
  if (!is.numeric(coverage_threshold) | coverage_threshold > 1 |
      coverage_threshold < 0) {
    stop("Coverage threshold should be a numeric value >= 0 and <= 1",
         call. = FALSE)
  }
  
  # Get trait coverage for site and traits
  trait_coverage = fbg_get_coverage(site_sp, traits)
  
  # Filter sites by coverage
  selected_sites = trait_coverage[
    which(trait_coverage$trait_coverage >= coverage_threshold), "site"
  ]
  
  if (identical(selected_sites, character(0))) {
    message("No sites had the specified trait coverage threshold")
    NULL
  } else {
    site_sp[selected_sites,, drop = FALSE]
  }
}
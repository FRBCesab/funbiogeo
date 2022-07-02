#' Count Number of Sites Occupied by Species
#' 
#' @description
#' For each species computes the percentage of sites where the species is 
#' present (distribution value higher than 0 and non-NA).
#' 
#' @inheritParams fb_get_trait_coverage_by_site
#'
#' @return A three-column `data.frame` with:
#' - `species`: the name of the species;
#' - `n_sites`: the number of sites where the species is present;
#' - `coverage`: the percentage of sites where the species is present.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("site_species")
#' 
#' site_coverage_by_species <- fb_count_sites_by_species(site_species)
#' head(site_coverage_by_species)

fb_count_sites_by_species <- function(site_species) {
  
  ## Check inputs ----
  
  check_site_species(site_species)
  
  
  ## Compute sites coverage by species ----
  
  sites_coverage <- unlist(lapply(colnames(site_species)[-1], function(x) 
    sum(!is.na(site_species[[x]]) & site_species[[x]] > 0)))
  
  sites_coverage <- data.frame(
    "species"  = colnames(site_species)[-1],
    "n_sites"  = sites_coverage,
    "coverage" = sites_coverage / nrow(site_species)
  )
  
  sites_coverage <- sites_coverage[order(sites_coverage$"coverage", 
                                         decreasing = TRUE), ]
  
  rownames(sites_coverage) <- NULL
  
  sites_coverage
}

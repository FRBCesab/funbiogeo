#' Count Number of Species per Site
#' 
#' @description
#' For each site computes the proportion of species present (distribution value
#' higher than 0 and non-NA) compared to all species provided.
#' For example, a site could contain only 20% of all species provided.
#' 
#' @inheritParams fb_get_trait_coverage_by_site
#'
#' @return A three-column `data.frame` with:
#' - `site`: the name of the site;
#' - `n_species`: the number of present species;
#' - `coverage`: the percentage of present species.
#' 
#' @export
#'
#' @examples
#' library("funbiogeo")
#' 
#' data("site_species")
#' 
#' species_coverage_by_site <- fb_count_species_by_site(site_species)

fb_count_species_by_site <- function(site_species) {
  
  ## Check inputs ----
  
  check_site_species(site_species)
  
  
  ## Compute species coverage by site ----
  
  species_coverage <- apply(site_species[ , -1], 1, function(x) 
    sum(!is.na(x) & x > 0))
  
  species_coverage <- data.frame(
    "site"      = site_species[ , 1],
    "n_species" = species_coverage,
    "coverage"  = species_coverage / ncol(site_species[ , -1]))
  
  species_coverage <- species_coverage[order(species_coverage$"coverage", 
                                         decreasing = TRUE), ]
  
  rownames(species_coverage) <- NULL
  
  species_coverage
}

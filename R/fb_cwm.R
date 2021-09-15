#' Compute community-weighted means of trait values
#' 
#' @description
#' ...
#' 
#' @inheritParams fb_get_coverage
#'
#' @return A `data.frame` with sites in rows and the following variables:
#' `site`, the site label, `trait`, quantitative traits, and `cwm`, the 
#' community-weighted means of quantitative traits values.
#' 
#' @export
#'
#' @examples
#' ## Add an example ----

fb_cwm <- function(sites_species, species_traits) {
  
  
  ## Check inputs ----
  
  if (missing(sites_species)) {
    stop("Argument 'sites_species' (sites x species matrix) is required")
  }
  
  if (missing(species_traits)) {
    stop("Argument 'species_traits' (species x traits matrix) is required")
  }
  
  check_sites_species(sites_species)
  
  check_species_traits(species_traits)
  
  
  # Get species in common between both matrices ----
  
  species <- list_common_species(colnames(sites_species), 
                                 rownames(species_traits))
  
  
  # Select quantitative traits for CWM ---
  
  quanti_traits  <- apply(species_traits, 2, is.numeric)
  species_traits <- species_traits[ , quanti_traits, drop = FALSE]
  
  if (identical(as.logical(species_traits), logical(0))) {
    stop("CWM can only be computed on numeric traits", call. = FALSE)
  }
  
  
  # Total sites abundances ----
  
  total_abund <- rowSums(sites_species[ , species, drop = FALSE])
  
  
  # Compute CWM ----
  
  cwm <- (sites_species[ , species, drop = FALSE] / total_abund) %*%
    species_traits[species, , drop = FALSE]
  
  
  # Reformat CWM in good way (tidy format) ----
  
  col_names <- expand.grid(colnames(cwm), rownames(cwm))
  
  data.frame("site"  = col_names[ , 2], 
             "trait" = col_names[ , 1],
             "cwm"   = as.numeric(t(cwm)))
}

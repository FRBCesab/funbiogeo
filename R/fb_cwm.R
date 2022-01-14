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
#' library("funbiogeo")
#' 
#' data("site_species")
#' data("species_traits")
#' 
#' cover <- fb_cwm(site_species, species_traits)

fb_cwm <- function(site_species, species_traits) {
  
  
  ## Check inputs ----
  
  if (missing(site_species)) {
    stop("Argument 'site_species' (site x species matrix) is required")
  }
  
  if (missing(species_traits)) {
    stop("Argument 'species_traits' (species x traits matrix) is required")
  }
  
  check_site_species(site_species)
  check_species_traits(species_traits)
  
  
  # Get species in common between both matrices --------------------------------
  
  species <- list_common_species(colnames(site_species),
                                 species_traits[["species"]])
  
  
  # Select quantitative traits for CWM -----------------------------------------
  
  quanti_traits  <- vapply(species_traits, is.numeric, TRUE)
  quanti_traits[["species"]] <- TRUE
  species_traits <- species_traits[, quanti_traits, drop = FALSE]
  
  
  # Convert to matrix ----------------------------------------------------------
  
  if (sum(quanti_traits) <= 1) {
    stop("CWM can only be computed on numeric traits", call. = FALSE)
  }
  
  
  # Total sites abundances -----------------------------------------------------
  
  total_abund <- rowSums(site_species[ , species, drop = FALSE])
  
  
  # Compute CWM ----------------------------------------------------------------
  
  cwm <- (as.matrix(site_species[ , species, drop = FALSE]) / total_abund) %*%
    as.matrix(
      species_traits[species_traits[["species"]] %in% species, -1, drop = FALSE]
    )
  
  
  # Reformat CWM in good way (tidy format) -------------------------------------
  
  col_names <- expand.grid(colnames(cwm), site_species[["site"]])
  
  data.frame("site"  = col_names[ , 2], 
             "trait" = col_names[ , 1],
             "cwm"   = as.numeric(t(cwm)))
}

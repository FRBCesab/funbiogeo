#' Compute community-weighted means (CWM) of trait values
#' 
#' This function returns the community-weighted mean of provided trait values.
#' It only works with quantitative traits and will warn you otherwise.
#' It will remove species that either have `NA` values in the `site_species`
#' input or `NA` values as their trait.
#' 
#' @inheritParams fb_get_trait_coverage_by_site
#'
#' @return A `data.frame` with sites in rows and the following variables:
#'   * `site`, the site label,
#'   * `trait`, the trait label as provided in `species_traits`,
#'   * and `cwm`, the community-weighted means of quantitative traits values.
#' 
#' @export
#'
#' @examples
#' site_cwm <- fb_cwm(head(woodiv_site_species), woodiv_traits)
#' head(site_cwm)

fb_cwm <- function(site_species, species_traits) {
  
  # Check inputs
  check_site_species(site_species)
  check_species_traits(species_traits)
  
  
  # Get species in common between both matrices
  species <- list_common_species(
    colnames(site_species), species_traits[["species"]]
  )
  
  
  # Select quantitative traits for CWM
  quanti_traits  <- vapply(species_traits, is.numeric, TRUE)
  quanti_traits[["species"]] <- TRUE
  species_traits <- species_traits[, quanti_traits, drop = FALSE]
  

  if (sum(quanti_traits) <= 1) {
    stop(
      "No numeric traits found. CWM can only be computed on numeric traits",
      call. = FALSE
    )
  }
  
  
  # Total sites abundances
  if (any(is.na(site_species[ , species, drop = FALSE]))) {
    message(
      "Some species had NA abundances, removing them from CWM computation"
    )
  }
  
  site_species_long <- tidyr::pivot_longer(
    site_species, -"site", names_to = "species", values_to = "ab_value"
  )
  
  # Extracting Trait Matrix
  trait_matrix <- species_traits[
    species_traits[["species"]] %in% species,, drop = FALSE
  ]
  
  if (any(is.na(trait_matrix))) {
    message(
      "Some species had NA trait values, removing them from CWM computation"
    )
  }
  
  trait_matrix_long <- tidyr::pivot_longer(
    trait_matrix, -"species", names_to = "trait_name", values_to = "trait_value"
  )
  
  
  site_species_traits <- merge(
    site_species_long, trait_matrix_long, by = "species"
  )
  
  # Compute CWM
  cwm <- by(
    site_species_traits,
    list(site_species_traits[["site"]], site_species_traits[["trait_name"]]),
      function(x) {
      weighted_mean(x$trait_value, x$ab_value, na.rm = TRUE)
    }, simplify = TRUE
  )
  
  cwm <- as.data.frame.table(cwm)
  colnames(cwm) <- c("site", "trait", "cwm")
  
  return(cwm)
}

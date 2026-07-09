#' Compute Number of Known Trait(s) per Species
#'
#' @description
#' For each species, this function computes the number and proportion of traits
#' without `NA` (missing trait values).
#'
#' @inheritParams fb_get_trait_coverage_by_site
#'
#' @return A three-column `data.frame` with:
#' - `species`: the name of the species;
#' - `n_traits`: the number of traits with non-missing value for the species;
#' - `coverage`: the percentage of traits with non-missing value for the
#' species.
#'
#' @family {count functions}
#' 
#' @export
#'
#' @examples
#' trait_coverage_by_species <- fb_count_traits_by_species(woodiv_traits)
#' head(trait_coverage_by_species)

fb_count_traits_by_species <- function(species_traits) {
  # Check inputs
  check_species_traits(species_traits)

  # Compute traits coverage by species
  traits_coverage <- apply(
    drop_column(species_traits, "species"),
    1,
    function(x) sum(!is.na(x))
  )

  traits_coverage <- data.frame(
    "species" = species_traits[["species"]],
    "n_traits" = traits_coverage,
    "coverage" = traits_coverage / (ncol(species_traits) - 1)
  )

  traits_coverage <- traits_coverage[
    order(traits_coverage$"coverage", decreasing = TRUE),
  ]

  rownames(traits_coverage) <- NULL

  traits_coverage
}

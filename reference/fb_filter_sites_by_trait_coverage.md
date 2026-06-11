# Filter sites with a given trait coverage threshold

Select sites (rows of the `site_species` object) with all given traits
available for at least the user-defined proportion of species
`threshold_traits_proportion`. If a single trait is given, then the
threshold applies to a single trait, if more than one trait is provided,
then the function considers a threshold across all traits taken
together.

## Usage

``` r
fb_filter_sites_by_trait_coverage(
  site_species,
  species_traits,
  threshold_traits_proportion = 1
)
```

## Arguments

- site_species:

  a `data.frame` with sites in rows and species in columns. **NOTE**:
  the first column should be named **`"site"`** and indicate site names.
  The other columns should be named according to species names.

- species_traits:

  a `data.frame` with species in rows and traits as columns. **NOTE**:
  The first column should be named **`"species"`** and contain species
  names. The other columns should be named according to trait names.

- threshold_traits_proportion:

  a numeric of length 1 between 0 and 1. The percentage trait coverage
  threshold

## Value

A subset of `site_species` with sites covered by X% of
abundance/coverage considering all provided traits.

## Examples

``` r
# Filter all the sites where all species have known traits
new_site_species <- fb_filter_sites_by_trait_coverage(
  woodiv_site_species, woodiv_traits
)

# There is only one such site
nrow(new_site_species)
#> [1] 4580

# Filter sites where at least 80% of species have known traits
new_site_species_2 <- fb_filter_sites_by_trait_coverage(
  woodiv_site_species, woodiv_traits, threshold_traits_proportion = 0.8
)

# There are now four sites
nrow(new_site_species_2)
#> [1] 4873
```

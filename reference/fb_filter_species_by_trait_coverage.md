# Filter species with a given traits coverage threshold

Selects species (rows) for which the proportion of traits without `NA`
(missing trait values) is higher than the user-defined threshold. It
considers as many traits as the ones provided to filter given the
threshold.

## Usage

``` r
fb_filter_species_by_trait_coverage(
  species_traits,
  threshold_traits_proportion = 0
)
```

## Arguments

- species_traits:

  a `data.frame` with species in rows and traits as columns. **NOTE**:
  The first column should be named **`"species"`** and contain species
  names. The other columns should be named according to trait names.

- threshold_traits_proportion:

  a numeric of length 1 between 0 and 1. The percentage of traits
  coverage threshold.

## Value

A subset of `species_traits` with species covered by X% of traits.

## Examples

``` r
# Filter species that have at least 60% of the traits described
new_species_traits <- fb_filter_species_by_trait_coverage(
  woodiv_traits,
  threshold_traits_proportion = 0.6
)

# There are now only 93 species
nrow(new_species_traits)
#> [1] 19
```

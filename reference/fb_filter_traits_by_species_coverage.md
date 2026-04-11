# Filter traits with a given species coverage threshold

Selects traits (columns) for which the percentage of species without
`NA` (missing trait values) is higher than a threshold.

## Usage

``` r
fb_filter_traits_by_species_coverage(
  species_traits,
  threshold_species_proportion = 0
)
```

## Arguments

- species_traits:

  a `data.frame` with species in rows and traits as columns. **NOTE**:
  The first column should be named **`"species"`** and contain species
  names. The other columns should be named according to trait names.

- threshold_species_proportion:

  `numeric(1)` \[default = `NULL`\]  
  between 0 and 1. The percentage of species coverage threshold.

## Value

A subset of `species_traits` with traits for the specified proportion of
species.

## Examples

``` r
# Filter traits that have at least 60% non-missing values
new_species_traits <- fb_filter_traits_by_species_coverage(
  woodiv_traits,
  threshold_species_proportion = 0.6
)

# There are now only 2 traits
ncol(new_species_traits)
#> [1] 5
```

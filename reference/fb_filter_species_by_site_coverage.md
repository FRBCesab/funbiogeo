# Filter species with a given sites coverage threshold

Selects species (columns) for which the percentage of sites where the
species is present (distribution value higher than 0 and non-NA) is
higher than a threshold.

## Usage

``` r
fb_filter_species_by_site_coverage(
  site_species,
  threshold_sites_proportion = 0
)
```

## Arguments

- site_species:

  a `data.frame` with sites in rows and species in columns. **NOTE**:
  the first column should be named **`"site"`** and indicate site names.
  The other columns should be named according to species names.

- threshold_sites_proportion:

  a numeric of length 1 between 0 and 1. The percentage of sites
  coverage threshold.

## Value

A subset of `site_species` with species with a prevalence higher than
`threshold_sites_proportion`.

## Examples

``` r
# Filter species present in at least 10% of the sites
new_site_species <- fb_filter_species_by_site_coverage(
  woodiv_site_species,
  threshold_sites_proportion = 0.1
)

new_site_species[1:3, 1:4]
#>       site PPIR PHAL JPHO
#> 1 41152325    0    0    1
#> 2 40852325    0    1    1
#> 3 40852345    0    0    1

# There are now only 84 species (instead of 149)
ncol(new_site_species)
#> [1] 11
```

# Filter sites with a given species coverage threshold

Selects sites (rows) for which the proportion of species present
(distribution value higher than 0 and non-NA) is higher than the
user-defined threshold.

## Usage

``` r
fb_filter_sites_by_species_coverage(
  site_species,
  threshold_species_proportion = 0
)
```

## Arguments

- site_species:

  a `data.frame` with sites in rows and species in columns. **NOTE**:
  the first column should be named **`"site"`** and indicate site names.
  The other columns should be named according to species names.

- threshold_species_proportion:

  a numeric of length 1 between 0 and 1. The threshold of species
  coverage under which to exclude the sites.

## Value

A subset of `site_species` with sites with at least
`threshold_species_proportion`% species present.

## Examples

``` r
# Get sites with more than 40% of the species
new_site_species <- fb_filter_sites_by_species_coverage(
  woodiv_site_species,
  threshold_species_proportion = 0.4
)
# There are now only 148 sites
dim(new_site_species)
#> [1] 60 25
new_site_species[1:3, 1:4]
#>        site AALB ACEP APIN
#> 13 41052315    1    0    0
#> 14 41252315    1    0    0
#> 18 40552295    1    1    1
```

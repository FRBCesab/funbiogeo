# Compute Trait Coverage For Each Site Weighted by Abundance

Compute trait coverage for all sites, i.e., the percentage of total
abundance/presence of species that have traits data compared to total
species. This function assumes that all species provided in the traits
dataset have all their traits specified (meaning that all species have
either known or `NA` values reported as their traits). **NB**: this
function returns trait coverage using all traits provided in the input
`species_traits` data.frame.

## Usage

``` r
fb_get_trait_coverage_by_site(site_species, species_traits)
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

## Value

A `data.frame` with `n` rows (where `n` is the number of sites) and two
columns: `site`, the site label, and `trait_coverage`, the percent of
total abundance/presence of species that have traits data.

## Examples

``` r
site_trait_cov <- fb_get_trait_coverage_by_site(
    woodiv_site_species, woodiv_traits
)

head(site_trait_cov)
#>       site trait_coverage
#> 1 41152325      1.0000000
#> 2 40852325      1.0000000
#> 3 40852345      0.8750000
#> 4 42552105      1.0000000
#> 5 41152315      0.8888889
#> 6 37452365      1.0000000
```

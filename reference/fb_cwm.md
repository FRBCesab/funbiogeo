# Compute community-weighted means (CWM) of trait values

This function returns the community-weighted mean of provided trait
values. It only works with quantitative traits and will warn you
otherwise. It will remove species that either have `NA` values in the
`site_species` input or `NA` values as their trait.

## Usage

``` r
fb_cwm(site_species, species_traits)
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

A `data.frame` with sites in rows and the following variables:

- `site`, the site label,

- `trait`, the trait label as provided in `species_traits`,

- and `cwm`, the community-weighted means of quantitative traits values.

## Examples

``` r
site_cwm <- fb_cwm(head(woodiv_site_species), woodiv_traits)
#> Some species had NA trait values, removing them from CWM computation
head(site_cwm)
#>       site        trait      cwm
#> 1 37452365 plant_height 28.26817
#> 2 40852325 plant_height 20.05912
#> 3 40852345 plant_height 19.42354
#> 4 41152315 plant_height 19.11680
#> 5 41152325 plant_height 17.25338
#> 6 42552105 plant_height 22.16911
```

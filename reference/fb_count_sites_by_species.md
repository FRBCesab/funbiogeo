# Count Number of Sites Occupied by Species

This function computes the number and proportion of sites occupied by
each species (distribution value higher than 0 and non-NA).

## Usage

``` r
fb_count_sites_by_species(site_species)
```

## Arguments

- site_species:

  a `data.frame` with sites in rows and species in columns. **NOTE**:
  the first column should be named **`"site"`** and indicate site names.
  The other columns should be named according to species names.

## Value

A three-column `data.frame` with:

- `species`: the name of the species;

- `n_sites`: the number of sites where the species is present;

- `coverage`: the percentage of sites where the species is present.

## Examples

``` r
site_coverage_by_species <- fb_count_sites_by_species(woodiv_site_species)
head(site_coverage_by_species)
#>   species n_sites  coverage
#> 1    PPIR    2264 0.4219158
#> 2    PHAL    2203 0.4105479
#> 3    JPHO    1781 0.3319046
#> 4    JOXY    1576 0.2937011
#> 5    PPIA    1481 0.2759970
#> 6    PSYL    1297 0.2417070
```

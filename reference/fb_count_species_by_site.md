# Count Number of Species per Site

For each site computes the proportion of species present (distribution
value higher than 0 and non-NA) compared to all species provided. For
example, a site could contain only 20% of all species provided.

## Usage

``` r
fb_count_species_by_site(site_species)
```

## Arguments

- site_species:

  a `data.frame` with sites in rows and species in columns. **NOTE**:
  the first column should be named **`"site"`** and indicate site names.
  The other columns should be named according to species names.

## Value

A three-column `data.frame` with:

- `site`: the name of the site;

- `n_species`: the number of present species;

- `coverage`: the percentage of present species.

## Examples

``` r
species_coverage_by_site <- fb_count_species_by_site(woodiv_site_species)
head(species_coverage_by_site)
#>       site n_species  coverage
#> 1 40552295        12 0.5000000
#> 2 39252405        12 0.5000000
#> 3 37752305        12 0.5000000
#> 4 39852255        12 0.5000000
#> 5 39152415        12 0.5000000
#> 6 39552415        11 0.4583333
```

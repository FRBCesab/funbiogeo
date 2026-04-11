# Compute Trait Coverage per Site for Each Trait

Compute trait coverage for all sites, i.e., the percentage of total
abundance/presence of species that have traits data compared to total
species. This function assumes that all species provided in the traits
dataset have all their traits specified (meaning that all species have
either known or `NA` values reported as their traits). The coverage of
each trait separately is returned as well as all traits taken together
if wanted.

## Usage

``` r
fb_get_all_trait_coverages_by_site(
  site_species,
  species_traits,
  all_traits = TRUE
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

- all_traits:

  a logical (default = `TRUE`) which tell if the coverage considering
  all provided traits should be provided in an additional column
  `all_traits`

## Value

a data.frame with a column with sites and one column per provided trait
giving its coverage (percent species per site, weighted by abundance
that have trait data), and, when argument `all_traits = TRUE`, an
additional column named `all_traits` considering the coverage of all
traits taken together.

## Examples

``` r
site_trait_cov <- fb_get_all_trait_coverages_by_site(
  woodiv_site_species, woodiv_traits
)

head(site_trait_cov)
#>       site all_traits plant_height seed_mass sla wood_density
#> 1 26351755          1            1         1   1            1
#> 2 26351765          1            1         1   1            1
#> 3 26351955          1            1         1   1            1
#> 4 26351965          1            1         1   1            1
#> 5 26451755          1            1         1   1            1
#> 6 26451765          1            1         1   1            1
```

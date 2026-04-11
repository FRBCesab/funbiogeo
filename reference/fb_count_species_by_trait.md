# Count Number of Species for Each Trait

For each trait computes the percentage of species without `NA` (missing
trait values).

## Usage

``` r
fb_count_species_by_trait(species_traits)
```

## Arguments

- species_traits:

  a `data.frame` with species in rows and traits as columns. **NOTE**:
  The first column should be named **`"species"`** and contain species
  names. The other columns should be named according to trait names.

## Value

A three-column `data.frame` with:

- `trait`: the name of the trait;

- `n_species`: the number of species with non-missing value for the
  trait;

- `coverage`: the percentage of species with non-missing value for the
  trait.

## Examples

``` r
species_coverage_by_trait <- fb_count_species_by_trait(woodiv_traits)
head(species_coverage_by_trait)
#>          trait n_species  coverage
#> 1 plant_height        24 1.0000000
#> 2    seed_mass        23 0.9583333
#> 3          sla        17 0.7083333
#> 4 wood_density        17 0.7083333
```

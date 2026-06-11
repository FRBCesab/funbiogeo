# Compute Number of Known Trait(s) per Species

For each species, this function computes the number and proportion of
traits without `NA` (missing trait values).

## Usage

``` r
fb_count_traits_by_species(species_traits)
```

## Arguments

- species_traits:

  a `data.frame` with species in rows and traits as columns. **NOTE**:
  The first column should be named **`"species"`** and contain species
  names. The other columns should be named according to trait names.

## Value

A three-column `data.frame` with:

- `species`: the name of the species;

- `n_traits`: the number of traits with non-missing value for the
  species;

- `coverage`: the percentage of traits with non-missing value for the
  species.

## Examples

``` r
trait_coverage_by_species <- fb_count_traits_by_species(woodiv_traits)
head(trait_coverage_by_species)
#>   species n_traits coverage
#> 1    AALB        4        1
#> 2    APIN        4        1
#> 3    CSEM        4        1
#> 4    JCOM        4        1
#> 5    JOXY        4        1
#> 6    JPHO        4        1
```

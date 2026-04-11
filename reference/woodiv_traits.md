# Species x Traits of some Mediterranean Conifers

This dataset is derived from the WOODIV database (available at:
<https://www.nature.com/articles/s41597-021-00873-3>). It contains the
values of four functional traits for 24 Conifer tree species occurring
in Portugal, Spain, France, and Italy (Mediterranean part).

This dataset shows the format of the argument `species_traits` used in
several functions of `funbiogeo`. Note that species names must be the
first column and must be named `species`.

## Usage

``` r
woodiv_traits
```

## Format

A `data.frame` with 24 rows (species) and the following five columns:

- species:

  species name corresponding to the columns of `site_species`

- plant_height:

  adult plant height (in \\m\\)

- seed_mass:

  seed mass (in \\g\\)

- sla:

  specific leaf area, i.e. the ratio between leaf area and dry mass (in
  \\m^{2}\cdot kg^{-1}\\)

- wood_density:

  wood density, i.e. stem specific density (in \\kg\cdot dm^{–3}\\)

## References

Monnet AC, Cilleros K, Médail F *et al.* (2021) WOODIV, a database of
occurrences, functional traits, and phylogenetic data for all
Euro-Mediterranean trees. **Scientific Data**, 8, 89. DOI:
[10.1038/s41597-021-00873-3](https://doi.org/10.1038/s41597-021-00873-3)

## Examples

``` r
data("woodiv_traits")
class(woodiv_traits)
#> [1] "data.frame"
dim(woodiv_traits)
#> [1] 24  5
head(woodiv_traits)
#>   species plant_height seed_mass      sla wood_density
#> 1    AALB     49.64162 67.866923 7.483978    0.4490821
#> 3    ACEP     25.87500 64.703750       NA           NA
#> 4    ANEB     15.00000        NA 3.420603           NA
#> 5    APIN     27.33333 55.520000 3.420603    0.4586508
#> 6    CLIB     35.63636 86.872600       NA    0.4500000
#> 7    CSEM     24.69231  7.608125 5.824112    0.5184729
```

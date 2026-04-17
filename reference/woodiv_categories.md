# Species x Categories of some Mediterranean Conifers

This dataset is derived from the WOODIV database (available at:
<https://www.nature.com/articles/s41597-021-00873-3>). It contains
information (classification, endemism, etc.) of the 24 Conifer tree
species occurring in Portugal, Spain, France, and Italy (Mediterranean
part). This information will be used as a species grouping factor.

This dataset shows the format of the argument `species_categories` used
in several functions of `funbiogeo`. Note that species names must be the
first column and must be named `species`.

## Usage

``` r
woodiv_categories
```

## Format

A `data.frame` with 24 rows (species) and 6 columns (`species`,
`family`, `genus`, `binomial`, `endemism`, and `cultivated`).

## References

Monnet AC, Cilleros K, Médail F *et al.* (2021) WOODIV, a database of
occurrences, functional traits, and phylogenetic data for all
Euro-Mediterranean trees. **Scientific Data**, 8, 89. DOI:
[doi:10.1038/s41597-021-00873-3](https://doi.org/10.1038/s41597-021-00873-3)
.

## Examples

``` r
data("woodiv_categories")
class(woodiv_categories)
#> [1] "data.frame"
dim(woodiv_categories)
#> [1] 24  6
woodiv_categories[1:6, ]
#>   species       family     genus               binomial endemism cultivated
#> 1    AALB     Pinaceae     Abies             Abies alba        0          0
#> 3    ACEP     Pinaceae     Abies      Abies cephalonica        1          0
#> 4    ANEB     Pinaceae     Abies      Abies nebrodensis        1          0
#> 5    APIN     Pinaceae     Abies          Abies pinsapo        1          0
#> 6    CLIB     Pinaceae    Cedrus          Cedrus libani        0          1
#> 7    CSEM Cupressaceae Cupressus Cupressus sempervirens        0          1
```

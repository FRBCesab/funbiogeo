# Sites x Species of some Mediterranean Conifers

This dataset is derived from the WOODIV database (available at:
<https://www.nature.com/articles/s41597-021-00873-3>). It contains the
presence/absence of the 24 Conifer tree species occurring in Portugal,
Spain, France, and Italy (Mediterranean part). The presence/absence is
available at 5,366 sites (grid cells of 10 x 10 km horizontal
resolution). Sites coordinates are provided in `site_locations`.

This dataset shows the format of the argument `site_species` used in
several functions of `funbiogeo`. Note that site labels must be the
first column and must be named `site`.

## Usage

``` r
woodiv_site_species
```

## Format

A `data.frame` with 5,366 rows (sites) and 25 columns (1 column for site
label and 24 for species occurrence).

## References

Monnet AC, Cilleros K, Médail F *et al.* (2021) WOODIV, a database of
occurrences, functional traits, and phylogenetic data for all
Euro-Mediterranean trees. **Scientific Data**, 8, 89. DOI:
[10.1038/s41597-021-00873-3](https://doi.org/10.1038/s41597-021-00873-3)

## Examples

``` r
data("woodiv_site_species")
class(woodiv_site_species)
#> [1] "data.frame"
dim(woodiv_site_species)
#> [1] 5366   25
woodiv_site_species[1:6, 1:6]
#>       site AALB ACEP APIN CLIB CSEM
#> 1 41152325    1    0    0    0    0
#> 2 40852325    1    0    0    0    0
#> 3 40852345    1    0    0    0    0
#> 4 42552105    1    0    0    0    0
#> 5 41152315    1    0    0    0    0
#> 6 37452365    1    0    0    0    0
```

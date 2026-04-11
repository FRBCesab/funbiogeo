# Sites x Locations of some Mediterranean Conifers

This dataset is derived from the WOODIV database (available at:
<https://www.nature.com/articles/s41597-021-00873-3>). It contains the
grid cells of sites (10 km x 10 km horizontal resolution) sampled in
Portugal, Spain, France, and Italy (Mediterranean part) for which at
least one of the 24 Conifer tree species occurs.

This dataset exemplifies the argument `site_locations` used in several
functions of `funbiogeo`. The variable `site` corresponds to the site
labels (must match the same column in `site_species`) and the variable
`country` will be used as a site grouping factor.

## Usage

``` r
woodiv_locations
```

## Format

An `sf` `POLYGON` object with 5,366 rows (grid cells) defined in the
ETRS89-extended / LAEA Europe Coordinate Reference System (EPSG:3035).

## References

Monnet AC, Cilleros K, Médail F *et al.* (2021) WOODIV, a database of
occurrences, functional traits, and phylogenetic data for all
Euro-Mediterranean trees. **Scientific Data**, 8, 89. DOI:
[10.1038/s41597-021-00873-3](https://doi.org/10.1038/s41597-021-00873-3)

## Examples

``` r
data("woodiv_locations")
class(woodiv_locations)
#> [1] "sf"         "data.frame"
dim(woodiv_locations)
#> [1] 5366    3
head(woodiv_locations)
#> Simple feature collection with 6 features and 2 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 2630000 ymin: 1750000 xmax: 2650000 ymax: 1970000
#> Projected CRS: ETRS89-extended / LAEA Europe
#>       site  country                       geometry
#> 1 26351755 Portugal POLYGON ((2630000 1750000, ...
#> 2 26351765 Portugal POLYGON ((2630000 1760000, ...
#> 4 26351955 Portugal POLYGON ((2630000 1950000, ...
#> 5 26351965 Portugal POLYGON ((2630000 1960000, ...
#> 6 26451755 Portugal POLYGON ((2640000 1750000, ...
#> 7 26451765 Portugal POLYGON ((2640000 1760000, ...
```

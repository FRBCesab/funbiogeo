# Aggregate Site Data Along Coarser Spatial Grid

This function helps aggregating site data along a coarser grid. The idea
is that you have any type of data at the site scale (diversity metrics,
environmental data, etc.) but you would like to work on it at a coarser
scale, or you want to visualize it at that scale. This function helps
you do exactly that.

## Usage

``` r
fb_aggregate_site_data(site_locations, site_data, agg_geom, fun = mean, ...)
```

## Arguments

- site_locations:

  an `sf` object with the spatial geometries of sites. **NOTE**: the
  first column should be named **`"site"`** and indicate site names.

- site_data:

  a `matrix` or `data.frame` containing values per sites to aggregate
  along the provided grid. Can have one or several columns (variables to
  aggregate). The first column must contain sites names as provided in
  the first argument `site_locations`.

- agg_geom:

  a
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or an `sf` object. This defines the geometry along which to aggregate
  the initial data. See more in the Details section.

- fun:

  the function used to aggregate points values when there are multiple
  points in one cell. Default is `mean`.

- ...:

  additional argument(s) passed to the provided function `fun`

## Value

An object of the same type as the `agg_geom` input with as many layers
(if `SpatRaster`) or columns (if `sf`) as columns provided in the input
`site_data`.

## Details

The `agg_geom` object will condition the type of object ouput by the
function. It can be of any sort as an `SpatRaster` or `sf` object.
Depending on the need, it could be a regular square grid or hexagonal
grid, it could also be irregular polygons like biomes or ecoregions, or
points, and even lines (such as when aggregating across transects or
trajectories).

## Examples

``` r
library("sf")
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
## Raster grid
tavg <- system.file(
  "extdata", "annual_mean_temp.tif", package = "funbiogeo"
)
tavg <- terra::rast(tavg)

# Rasterize 3 first species counts
fb_aggregate_site_data(
    head(woodiv_locations), woodiv_site_species[, 1:4], tavg, fun = sum
)
#> class       : SpatRaster 
#> size        : 290, 405, 3  (nrow, ncol, nlyr)
#> resolution  : 0.08333333, 0.08333333  (x, y)
#> extent      : -10.5, 23.25, 35.83333, 60  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> varnames    : annual_mean_temp 
#>               annual_mean_temp 
#>               annual_mean_temp 
#> names       : AALB, ACEP, APIN 
#> min values  :    0,    0,    0 
#> max values  :    0,    0,    0 

## Irregular polygons
countries <- readRDS(system.file(
  "extdata", "countries_sf.rds", package = "funbiogeo"
))
# Aggregate occurrence per country
fb_aggregate_site_data(
    head(woodiv_locations, n = 20), woodiv_site_species[, 1:4], countries,
    fun = sum
)
```

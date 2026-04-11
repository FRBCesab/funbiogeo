# Extract Raster Values at Location of Sites

Extract Raster Values at Location of Sites

## Usage

``` r
fb_get_environment(site_locations, environment_raster)
```

## Arguments

- site_locations:

  an `sf` object with the spatial geometries of sites. **NOTE**: the
  first column should be named **`"site"`** and indicate site names.

- environment_raster:

  a `SpatRaster` object (package `terra`). A single or multi-layers
  environmental raster.

## Value

A `data.frame` with average environmental values (columns) per site
(rows), with the first column being `"site"` indicating site names.

## Examples

``` r
## Import climate rasters ----
prec <- system.file("extdata", "annual_tot_prec.tif", package = "funbiogeo")
tavg <- system.file("extdata", "annual_mean_temp.tif", package = "funbiogeo")

layers <- terra::rast(c(tavg, prec))

fb_get_environment(head(woodiv_locations), layers)
#>       site annual_mean_temp annual_tot_prec
#> 1 26351755              NaN             NaN
#> 2 26351765         16.44746             495
#> 3 26351955         16.08605             671
#> 4 26351965              NaN             NaN
#> 5 26451755         16.66498             495
#> 6 26451765         16.39656             517
```

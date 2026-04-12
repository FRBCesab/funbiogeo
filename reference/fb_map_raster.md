# Map a Single Raster Layer

Map a Single Raster Layer

## Usage

``` r
fb_map_raster(x, background = FALSE, ...)
```

## Arguments

- x:

  a `SpatRaster` object (package `terra`). A raster of one single layer

- background:

  a `logical`. If `TRUE` adds a layer of country boundaries from Natural
  Earth.

- ...:

  other parameters passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)

## Value

A `ggplot` object.

## Examples

``` r
library(ggplot2)

## Load raster ----
tavg <- system.file("extdata", "annual_mean_temp.tif", package = "funbiogeo")
tavg <- terra::rast(tavg)

## Default map ----
fb_map_raster(tavg)


## Map with a background ----
fb_map_raster(tavg, background = TRUE)


## Map with custom theme ----
fb_map_raster(tavg, legend.position = "bottom")


## Advanced customization ----
my_map <- fb_map_raster(tavg) +
  scale_fill_distiller("Temperature", palette = "Spectral") +
  theme(legend.position = "bottom") +
  ggtitle("Mean annual temperature in Western Europe")

my_map


## Map w/o annotation ----
fb_map_raster(tavg) +
  theme_void() +
  theme(legend.position = "none")
```

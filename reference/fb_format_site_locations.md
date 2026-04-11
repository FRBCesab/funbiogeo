# Extract site x locations information from long format data

Convert a flat `data.frame` with site coordinates into a proper `sf`
object that can then be used by other functions. This function assumes
that the coordinates are given in WGS84 (longitude vs. latitude). The
function automatically removes repeated coordinates from the input
dataset.

## Usage

``` r
fb_format_site_locations(
  data,
  site,
  longitude,
  latitude,
  crs = sf::st_crs(4326),
  na_rm = FALSE
)
```

## Arguments

- data:

  a `data.frame` in a long format (see example).

- site:

  a `character` of length 1. Name of the column with site labels.

- longitude:

  a `character` of length 1. Name of the column with longitude. The
  function assumes coordinates are WGS84 (EPSG:4326).

- latitude:

  a `character` of length 1. Name of the column with latitude. The
  function assumes coordinates are WGS84 (EPSG:4326).

- crs:

  a `character` of length 1 or an object of class `crs`. Coordinate
  Reference System (CRS) of the specified coordinates. The CRS should be
  a [valid CRS in
  R](https://geocompr.robinlovelace.net/spatial-class.html?q=CRS#crs-in-r).
  It can either be a character like
  `"+proj=longlat +datum=WGS84 +no_defs"` or as specified using
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  like `sf::st_crs(4326)` the default value.

- na_rm:

  a logical value. If `TRUE` remove sites with incomplete coordinates.
  Default is `FALSE`.

## Value

An `sf` object with a `site` column specifying site coordinates.

## Examples

``` r
filename <- system.file(
    "extdata", "woodiv_raw_data.csv",
    package = "funbiogeo"
)
all_data <- read.csv(filename)

head(all_data)
#>       site  country longitude latitude species count       family     genus
#> 1 26351755 Portugal   2635000  1755000    JPHO     1 Cupressaceae Juniperus
#> 2 26351755 Portugal   2635000  1755000    PPIR     1     Pinaceae     Pinus
#> 3 26351765 Portugal   2635000  1765000    JPHO     1 Cupressaceae Juniperus
#> 4 26351955 Portugal   2635000  1955000    JPHO     1 Cupressaceae Juniperus
#> 5 26351955 Portugal   2635000  1955000    PPIR     1     Pinaceae     Pinus
#> 6 26351965 Portugal   2635000  1965000    JPHO     1 Cupressaceae Juniperus
#>              binomial endemism cultivated plant_height seed_mass      sla
#> 1 Juniperus phoenicea        0          0      4.88150  79.86000 4.365246
#> 2      Pinus pinaster        0          0     19.75384  55.83434 3.357539
#> 3 Juniperus phoenicea        0          0      4.88150  79.86000 4.365246
#> 4 Juniperus phoenicea        0          0      4.88150  79.86000 4.365246
#> 5      Pinus pinaster        0          0     19.75384  55.83434 3.357539
#> 6 Juniperus phoenicea        0          0      4.88150  79.86000 4.365246
#>   wood_density
#> 1    0.6487500
#> 2    0.4430277
#> 3    0.6487500
#> 4    0.6487500
#> 5    0.4430277
#> 6    0.6487500

site_locations <- fb_format_site_locations(
    all_data, "site", "longitude",
    "latitude"
)
head(site_locations)
#> Simple feature collection with 6 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1755000 ymin: 2635000 xmax: 1965000 ymax: 2645000
#> Geodetic CRS:  WGS 84
#>       site                geometry
#> 1 26351755 POINT (1755000 2635000)
#> 3 26351765 POINT (1765000 2635000)
#> 4 26351955 POINT (1955000 2635000)
#> 6 26351965 POINT (1965000 2635000)
#> 8 26451755 POINT (1755000 2645000)
#> 9 26451765 POINT (1765000 2645000)
```

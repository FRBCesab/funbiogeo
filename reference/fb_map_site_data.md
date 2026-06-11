# Map Arbitrary Site Data

This function helps to map arbitrary site data using the site-locations
object and a dataset organized by site. The returned plot is as little
customized as possible to let the user choose. The function can provide
a basic background map if the `background` argument is toggled.

## Usage

``` r
fb_map_site_data(site_locations, site_data, selected_col, background = FALSE)
```

## Arguments

- site_locations:

  a `sf` object with the spatial geometries of sites. **NOTE**: the
  first column should be named **`"site"`** and indicate site names.

- site_data:

  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) of additional
  site information containing the column `"site"` to merge with the
  `site_locations` argument

- selected_col:

  `character(1)` name of the column to plot

- background:

  a `logical`. If `TRUE` adds a layer of country boundaries from Natural
  Earth.

## Value

a `ggplot` object.

## Examples

``` r
site_rich <- fb_count_species_by_site(woodiv_site_species)

# Map of Species Richness
rich_map <- fb_map_site_data(woodiv_locations, site_rich, "n_species")
rich_map


# Customize the map
rich_map +
  ggplot2::scale_fill_viridis_c("Species Richness")


# Map w/ a background
fb_map_site_data(
  woodiv_locations, site_rich, "n_species", background = TRUE
)
```

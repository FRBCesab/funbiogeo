# Map Trait Coverage Per Site

Returns a `ggplot2` map of sites colored by trait coverage (proportion
of species having a known trait value). By default shows one plot for
each trait and add an additional facet named `"all_traits"` considering
the trait coverage with all traits taken together.

## Usage

``` r
fb_map_site_traits_completeness(
  site_locations,
  site_species,
  species_traits,
  all_traits = TRUE,
  background = FALSE
)
```

## Arguments

- site_locations:

  an `sf` object with the spatial geometries of sites. **NOTE**: the
  first column should be named **`"site"`** and indicate site names.

- site_species:

  a `data.frame` with sites in rows and species in columns. **NOTE**:
  the first column should be named **`"site"`** and indicate site names.
  The other columns should be named according to species names.

- species_traits:

  a `data.frame` with species in rows and traits as columns. **NOTE**:
  The first column should be named **`"species"`** and contain species
  names. The other columns should be named according to trait names.

- all_traits:

  a logical (default = `TRUE`) which tell if the coverage considering
  all provided traits should be provided in an additional column
  `all_traits`

- background:

  a `logical`. If `TRUE` adds a layer of country boundaries from Natural
  Earth.

## Value

a 'ggplot2' object

## Examples

``` r
# Map without a background
fb_map_site_traits_completeness(
  woodiv_locations, woodiv_site_species, woodiv_traits
)


# Map with a background
fb_map_site_traits_completeness(
  woodiv_locations, woodiv_site_species, woodiv_traits, background = TRUE
)
```

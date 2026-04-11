# Plot Trait Coverage per Site for each Trait

Display a binary heatmap visualizing the site x traits matrix with
colors displaying the proportion of occurring species with known trait
values. Traits are ordered from the most to the least known (left to
right). Sites are ordered from the ones with highest to lowest overall
trait coverage (bottom to top). The site average proportion of species
with known trait for each trait (across all sites) is shown in the
x-axis labels. An additional column at the very right of the plot named
`"all_traits"` shows a summary considering traits together.

## Usage

``` r
fb_plot_site_traits_completeness(
  site_species,
  species_traits,
  species_categories = NULL,
  all_traits = TRUE
)
```

## Arguments

- site_species:

  a `data.frame` with sites in rows and species in columns. **NOTE**:
  the first column should be named **`"site"`** and indicate site names.
  The other columns should be named according to species names.

- species_traits:

  a `data.frame` with species in rows and traits as columns. **NOTE**:
  The first column should be named **`"species"`** and contain species
  names. The other columns should be named according to trait names.

- species_categories:

  (default = `NULL`) 2-columns `data.frame` giving species categories,
  with the first column describing the species name, and the second
  column giving their corresponding categories

- all_traits:

  a logical (default = `TRUE`) which tell if the coverage considering
  all provided traits should be provided in an additional column
  `all_traits`

## Value

a ggplot2 object

## Examples

``` r
fb_plot_site_traits_completeness(woodiv_site_species, woodiv_traits)

```

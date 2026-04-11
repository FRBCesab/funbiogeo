# Plot Distribution of Trait Coverages across all Sites

Plots the distributions of trait coverage across site, i.e. the
proportion of species weighted by abundance with known trait values, for
each trait separately and all traits taken together. The trait
distributions are ordered from the lowest to the highest average trait
coverage (top to bottom). The top always displays a distribution named
`"all_traits"` that contains the distribution of coverage all traits
taken together.

## Usage

``` r
fb_plot_distribution_site_trait_coverage(
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

a 'ggplot2' object

## Details

**Note**: this function requires the installation of the `ggridges`
package. If it isn't installed, the function will give an error
suggesting installing it through `install.packages("ggrdiges")`.

## Examples

``` r
fb_plot_distribution_site_trait_coverage(woodiv_site_species, woodiv_traits)
#> Loading required namespace: ggridges
#> Picking joint bandwidth of 0.0445

```

# Plot Number of Species per Trait

Display a lollipop graph showing the number and proportion of species
with non-NA trait for each trait ranked in decreasing order.

## Usage

``` r
fb_plot_number_species_by_trait(
  species_traits,
  species_categories = NULL,
  threshold_species_proportion = NULL
)
```

## Arguments

- species_traits:

  a `data.frame` with species in rows and traits as columns. **NOTE**:
  The first column should be named **`"species"`** and contain species
  names. The other columns should be named according to trait names.

- species_categories:

  (default = `NULL`) 2-columns `data.frame` giving species categories,
  with the first column describing the species name, and the second
  column giving their corresponding categories

- threshold_species_proportion:

  `numeric(1)` \[default = `NULL`\]  
  between 0 and 1. The percentage of species coverage threshold.

## Value

a ggplot2 object

## Examples

``` r
fb_plot_number_species_by_trait(woodiv_traits)


# Add a vertical cutoff line (12.5% of species)
fb_plot_number_species_by_trait(woodiv_traits, NULL, 1/8)

```

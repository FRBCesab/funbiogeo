# Plot Present and Missing Trait Combinations Frequencies

Display a figure of Present/Missing Trait Combinations where one square
is represents one trait. The y-axis gives the frequency of the row (as
well as its proportions compared to the total number of rows).

## Usage

``` r
fb_plot_trait_combination_frequencies(
  species_traits,
  species_categories = NULL,
  order_by = c("number", "complete")
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

- order_by:

  `character(1)` either `"number"` or `"complete`  
  If `"number"` order rows by frequency so that most frequent rows are
  at the bottom. Otherwise order rows to order table by the number of
  non-missing traits then by the frequency of combinations

## Value

a `ggplot2` object

## Examples

``` r
# Ordered by number by default
fb_plot_trait_combination_frequencies(woodiv_traits)


# Order by present traits
fb_plot_trait_combination_frequencies(woodiv_traits, NULL, "complete")

```

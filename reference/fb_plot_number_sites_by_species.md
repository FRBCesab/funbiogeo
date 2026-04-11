# Plot Number of Sites by Species

Represent all species in each function of the number of sites they
occupy. The species are ordered from the ones that occupy the least
number of sites from the ones that occupy the most. The number of site
is indicated at the bottom x-axis, while the top x-axis represents the
proportion of occupied sites. The left y-axis label species names and
their rank by increasing prevalence. The user can supplied a threshold
of sites to see how many species occupy more or less than the given
proportion of sites.

## Usage

``` r
fb_plot_number_sites_by_species(
  site_species,
  threshold_sites_proportion = NULL
)
```

## Arguments

- site_species:

  a `data.frame` with sites in rows and species in columns. **NOTE**:
  the first column should be named **`"site"`** and indicate site names.
  The other columns should be named according to species names.

- threshold_sites_proportion:

  a numeric of length 1 between 0 and 1. The percentage of sites
  coverage threshold.

## Value

a `ggplot2` object

## Examples

``` r
fb_plot_number_sites_by_species(woodiv_site_species)


# Add a vertical cutoff line (40% of sites)
fb_plot_number_sites_by_species(woodiv_site_species, 0.4)
```

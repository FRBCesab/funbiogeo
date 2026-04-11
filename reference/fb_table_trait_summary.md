# Summary Table on Traits (Missingness, Range, etc.)

This function outputs a `data.frame` that summarises the species by
trait table to have many information in a glance. This can then return a
data.frame or a nicely formatted
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) for
inclusion in an Rmarkdown document.

## Usage

``` r
fb_table_trait_summary(species_traits, kable = FALSE)
```

## Arguments

- species_traits:

  a `data.frame` with species in rows and traits as columns. **NOTE**:
  The first column should be named **`"species"`** and contain species
  names. The other columns should be named according to trait names.

- kable:

  `TRUE` or `FALSE` Should function returns a
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html)? defaults
  `FALSE`

## Value

a `data.frame` with the following columns:

- `trait_name`: a `character` column with the trait name as indicated in
  `species_traits`

- `trait_type`: the nature of the trait (`numeric`, `categorical`, or
  `ordered`)

- `number_non_missing`: the total number of non-`NA` trait values

- `proportion_non_missing`: the proportion of non-`NA` trait values

- `trait_range`: for numerical traits, the range of values

- `trait_mean_sd`: for numerical traits, the mean plus-minus the
  standard deviation

- `number_distinct`: for non-numerical traits, the number of categories

- `list_distinct`: for non-numerical traits, the list of categories

## Examples

``` r
# Get a data.frame back
fb_table_trait_summary(woodiv_traits)
#>     trait_name trait_type number_non_missing proportion_non_missing trait_range
#> 1 plant_height    numeric                 24                  100 %  1.37-49.64
#> 2    seed_mass    numeric                 23                   96 % 7.61-626.19
#> 3          sla    numeric                 17                   71 %   0.51-10.6
#> 4 wood_density    numeric                 17                   71 %   0.28-0.65
#>      trait_mean_sd number_distinct list_distinct
#> 1  18.78 +/- 11.76              NA          <NA>
#> 2 62.25 +/- 125.44              NA          <NA>
#> 3    4.77 +/- 2.18              NA          <NA>
#> 4    0.49 +/- 0.09              NA          <NA>

# Get a kable (to use in Rmd documents)
fb_table_trait_summary(woodiv_traits, TRUE)
#> 
#> 
#> Table: Summary of trait data
#> 
#> |  Trait Name  | Nature of Trait | Number of Non-Missing Values| Proportion of Non-Missing Values| Range of Trait | Trait Mean +/- SD | Number of Distinct Values|List of Distinct Values |
#> |:------------:|:---------------:|----------------------------:|--------------------------------:|:--------------:|:-----------------:|-------------------------:|:-----------------------|
#> | plant_height |     numeric     |                           24|                            100 %|   1.37-49.64   |  18.78 +/- 11.76  |                        NA|NA                      |
#> |  seed_mass   |     numeric     |                           23|                             96 %|  7.61-626.19   | 62.25 +/- 125.44  |                        NA|NA                      |
#> |     sla      |     numeric     |                           17|                             71 %|   0.51-10.6    |   4.77 +/- 2.18   |                        NA|NA                      |
#> | wood_density |     numeric     |                           17|                             71 %|   0.28-0.65    |   0.49 +/- 0.09   |                        NA|NA                      |
```

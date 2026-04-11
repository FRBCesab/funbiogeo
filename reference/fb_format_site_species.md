# Extract site x species information from long format data

Convert a flat `data.frame` with species occurrence/abundance at site
level into a proper `data.frame` object that can then be used by other
functions. The final output contains sites in rows and species in
columns.

## Usage

``` r
fb_format_site_species(data, site, species, value, na_to_zero = TRUE)
```

## Arguments

- data:

  a `data.frame` in a long format (see example).

- site:

  a `character` of length 1. Name of the column with site labels.

- species:

  a `character` of length 1. Name of the column with species names.

- value:

  a `character` of length 1. Name of the column with species
  occurrence/abundance.

- na_to_zero:

  a logical value. If `TRUE` (default) `NA` are replaced by `0`.

## Value

A `data.frame` with sites in rows and species in columns. The first
column is named `"site"` and contains the name of the sites.

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

site_species <- fb_format_site_species(all_data, "site", "species", "count")
site_species[1:3, 1:4]
#>       site JPHO PPIR PPIA
#> 1 26351755    1    1    0
#> 2 26351765    1    0    0
#> 3 26351955    1    1    0
```

# Extract species x categories information from long format data

Convert a flat `data.frame` with species names and species
(supra-)category (e.g. family, order, endemism status, etc.) into a
proper `data.frame` object that can then be used by other functions. The
final output contains species in rows and two columns (species name and
species category).

## Usage

``` r
fb_format_species_categories(data, species, category)
```

## Arguments

- data:

  a `data.frame` in a long format (see example).

- species:

  a `character` of length 1. Name of the column with species names.

- category:

  a `character` of length 1. Name of the column with species category.

## Value

A `data.frame` with species in rows and two columns (species name and
species category).

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

species_categories <- fb_format_species_categories(
  all_data, "species", "genus"
)
species_categories[1:6, ]
#>     species     genus
#> 1      JPHO Juniperus
#> 2      PPIR     Pinus
#> 7      PPIA     Pinus
#> 58     JNAV Juniperus
#> 372    JMAC Juniperus
#> 382    JOXY Juniperus
```

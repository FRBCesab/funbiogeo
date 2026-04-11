# Extract species x traits information from long format data

Convert a flat `data.frame` with traits values for different species
into a proper `data.frame` object that can then be used by other
functions. The final output contains species in rows and traits in
columns.

## Usage

``` r
fb_format_species_traits(data, species, traits)
```

## Arguments

- data:

  a `data.frame` in a long format (see example).

- species:

  a `character` of length 1. Name of the column with species names.

- traits:

  a `character` of length \>= 1. Name(s) of trait column(s).

## Value

A `data.frame` with species in rows and traits in columns, with the
first column names `"species"` containing the species names.

## Examples

``` r
filename <- system.file(
  "extdata", "woodiv_raw_data.csv", package = "funbiogeo"
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

traits <- c("plant_height", "seed_mass", "sla", "wood_density")

species_traits <- fb_format_species_traits(all_data, "species", traits)
head(species_traits)
#>   species plant_height seed_mass      sla wood_density
#> 1    APIN    27.333333  55.52000 3.420603    0.4586508
#> 2    JCOM     6.894711  14.55688 6.877889    0.5805503
#> 3    JMAC     5.000000   8.55000       NA           NA
#> 4    JNAV     1.367750  45.63000 3.890000           NA
#> 5    JOXY     5.800133  59.28214 5.904155    0.2796087
#> 6    JPHO     4.881500  79.86000 4.365246    0.6487500
```

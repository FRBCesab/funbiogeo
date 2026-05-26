# Formatting your data

This vignette will explain how to process a dataset that is aggregated
in long format to work with `funbiogeo`

Most functions in `funbiogeo` need three different datasets to work:

- the **species x traits** `data.frame` (example dataset:`woodiv_traits`
  in `funbiogeo`), which contains trait values for several traits (in
  columns) for several species (in rows).
- the **site x species** `data.frame` (example
  dataset:`woodiv_site_species` in `funbiogeo`), which contains the
  presence/absence, abundance, or cover information for species (in
  columns) by sites (in rows).
- the **site x locations** `sf` object (example
  dataset:`woodiv_locations` in `funbiogeo`), which contains the
  physical locations of the sites of interest.

Optionally, an additional dataset can be provided:

- a **species x categories** `data.frame` (example
  dataset:`species_categories` in `funbiogeo`), which contains two
  columns: one for species, one for potential categorization of species
  (whether it’s taxonomic classes, specific diets, or any arbitrary
  classification)

``` r

library(funbiogeo)
```

## Wide vs long format

In `funbiogeo` these datasets **must be** in a wide format (where one
row hosts several variables across columns), but sometimes information
is structured in a long format (one observation per row, also called
[**tidy format**](https://r4ds.had.co.nz/tidy-data.html)).

For instance, the following dataset illustrates the wider format (the
presence/absence of all species is spread across columns).

| site | species_1 | species_2 | species_3 | species_4 |
|:----:|:---------:|:---------:|:---------:|:---------:|
|  A   |     1     |     0     |     1     |     1     |
|  B   |     0     |     0     |     1     |     1     |
|  C   |     1     |     1     |     1     |     0     |

Wide format dataset (used in `funbiogeo`) {.table}

The following dataset illustrates the long format (the column `species`
contains the name of the species and the column `occurrence` contains
the presence/absence of species).

| site |  species  | occurrence |
|:----:|:---------:|:----------:|
|  A   | species_1 |     1      |
|  B   | species_1 |     0      |
|  C   | species_1 |     1      |
|  A   | species_2 |     0      |
|  B   | species_2 |     0      |
|  C   | species_2 |     1      |
|  A   | species_3 |     1      |
|  B   | species_3 |     1      |
|  C   | species_3 |     1      |
|  A   | species_4 |     1      |
|  B   | species_4 |     1      |
|  C   | species_4 |     0      |

Long format dataset {.table}

In order to use this dataset in `funbiogeo`, we need to transform this
tidy dataset into a wide dataset with all species as different columns.

## The `fb_format_*()` functions

If your data are not split into these wider datasets, you can use the
functions `fb_format_*()` to create these specific objects from a long
format dataset.

- [`fb_format_site_locations()`](https://frbcesab.github.io/funbiogeo/reference/fb_format_site_locations.md)
  allows extracting the **site x locations** information from the long
  format data
- [`fb_format_site_species()`](https://frbcesab.github.io/funbiogeo/reference/fb_format_site_species.md)
  allows extracting the **site x species** information from the long
  format data
- [`fb_format_species_traits()`](https://frbcesab.github.io/funbiogeo/reference/fb_format_species_traits.md)
  allows extracting the **species x traits** information from the long
  format data
- [`fb_format_species_categories()`](https://frbcesab.github.io/funbiogeo/reference/fb_format_species_categories.md)
  allows extracting the **species x categories** information from the
  long format data

All these functions take a long dataset as input (argument `data`),
where one row corresponds to the occurrence/abundance/coverage of one
species at one site and output a wider object.

## Usage

`funbiogeo` provides an excerpt of long format data to show how to use
the functions. This data sits at
`system.file("extdata", "woodiv_raw_data.csv", package = "funbiogeo")`.

Let’s import the long format dataset provided by `funbiogeo`:

``` r

# Define the path to long format dataset ----
file_name <- system.file(
  "extdata",
  "woodiv_raw_data.csv",
  package = "funbiogeo"
)


# Read the file ----
all_data <- read.csv(file_name)
```

| site | country | longitude | latitude | species | count | family | genus | binomial | endemism | cultivated | plant_height | seed_mass | sla | wood_density |
|:--:|:--:|---:|---:|:--:|:--:|:--:|:---|---:|---:|---:|---:|:--:|:--:|---:|
| 26351755 | Portugal | 2635000 | 1755000 | JPHO | 1 | Cupressaceae | Juniperus | Juniperus phoenicea | 0 | 0 | 4.88150 | 79.86000 | 4.365246 | 0.6487500 |
| 26351755 | Portugal | 2635000 | 1755000 | PPIR | 1 | Pinaceae | Pinus | Pinus pinaster | 0 | 0 | 19.75384 | 55.83434 | 3.357539 | 0.4430277 |
| 26351765 | Portugal | 2635000 | 1765000 | JPHO | 1 | Cupressaceae | Juniperus | Juniperus phoenicea | 0 | 0 | 4.88150 | 79.86000 | 4.365246 | 0.6487500 |
| 26351955 | Portugal | 2635000 | 1955000 | JPHO | 1 | Cupressaceae | Juniperus | Juniperus phoenicea | 0 | 0 | 4.88150 | 79.86000 | 4.365246 | 0.6487500 |
| 26351955 | Portugal | 2635000 | 1955000 | PPIR | 1 | Pinaceae | Pinus | Pinus pinaster | 0 | 0 | 19.75384 | 55.83434 | 3.357539 | 0.4430277 |
| 26351965 | Portugal | 2635000 | 1965000 | JPHO | 1 | Cupressaceae | Juniperus | Juniperus phoenicea | 0 | 0 | 4.88150 | 79.86000 | 4.365246 | 0.6487500 |
| 26351965 | Portugal | 2635000 | 1965000 | PPIA | 1 | Pinaceae | Pinus | Pinus pinea | 0 | 1 | 22.67000 | 626.18882 | 4.216176 | 0.5178617 |
| 26451755 | Portugal | 2645000 | 1755000 | JPHO | 1 | Cupressaceae | Juniperus | Juniperus phoenicea | 0 | 0 | 4.88150 | 79.86000 | 4.365246 | 0.6487500 |
| 26451765 | Portugal | 2645000 | 1765000 | JPHO | 1 | Cupressaceae | Juniperus | Juniperus phoenicea | 0 | 0 | 4.88150 | 79.86000 | 4.365246 | 0.6487500 |
| 26451765 | Portugal | 2645000 | 1765000 | PPIA | 1 | Pinaceae | Pinus | Pinus pinea | 0 | 1 | 22.67000 | 626.18882 | 4.216176 | 0.5178617 |

Long table example {.table style="width:100%;"}

### Extracting species x traits data

The function
[`fb_format_species_traits()`](https://frbcesab.github.io/funbiogeo/reference/fb_format_species_traits.md)
extracts species traits values from this long table to create the
species x traits dataset. Note that the function assumes each species
must have a single unique trait value. `funbiogeo` can deal with
intraspecific variation but not the
[`fb_format_species_traits()`](https://frbcesab.github.io/funbiogeo/reference/fb_format_species_traits.md)
function.

``` r

# Extract species x traits data ----
species_traits <- fb_format_species_traits(
  data = all_data,
  species = "species",
  traits = c("plant_height", "seed_mass", "sla", "wood_density")
)

# Preview ----
head(species_traits, 10)
#>    species plant_height  seed_mass      sla wood_density
#> 1     APIN    27.333333  55.520000 3.420603    0.4586508
#> 2     JCOM     6.894711  14.556875 6.877889    0.5805503
#> 3     JMAC     5.000000   8.550000       NA           NA
#> 4     JNAV     1.367750  45.630000 3.890000           NA
#> 5     JOXY     5.800133  59.282143 5.904155    0.2796087
#> 6     JPHO     4.881500  79.860000 4.365246    0.6487500
#> 7     PHAL    17.535250  20.152008 4.707311    0.4642383
#> 8     PPIA    22.670000 626.188824 4.216176    0.5178617
#> 9     PPIR    19.753843  55.834338 3.357539    0.4430277
#> 10    PSYL    24.795416   8.376612 4.765988    0.4365944
```

### Extracting site x species data

The function
[`fb_format_site_species()`](https://frbcesab.github.io/funbiogeo/reference/fb_format_site_species.md)
extracts species occurrence/abundance/coverage from this long table to
create the site x species dataset. Note that one species must have been
observed one time at one site (the package `funbiogeo` does not yet
consider temporal resurveys).

``` r

# Format site x species data ----
site_species <- fb_format_site_species(
  data = all_data,
  site = "site",
  species = "species",
  value = "count",
  na_to_zero = TRUE
)

# Preview ----
head(site_species[, 1:8], 10)
#>        site JPHO PPIR PPIA JNAV JMAC JOXY JCOM
#> 1  26351755    1    1    0    0    0    0    0
#> 2  26351765    1    0    0    0    0    0    0
#> 3  26351955    1    1    0    0    0    0    0
#> 4  26351965    1    0    1    0    0    0    0
#> 5  26451755    1    0    0    0    0    0    0
#> 6  26451765    1    1    1    0    0    0    0
#> 7  26451775    1    0    1    0    0    0    0
#> 8  26451955    1    1    0    0    0    0    0
#> 9  26451965    1    1    1    0    0    0    0
#> 10 26451975    1    1    1    0    0    0    0
```

### Extracting site x locations data

The function
[`fb_format_site_locations()`](https://frbcesab.github.io/funbiogeo/reference/fb_format_site_locations.md)
extracts sites coordinates from this long table to create the site x
locations dataset. Note that one site must have one unique longitude x
latitude value.

``` r

# Format site x locations data ----
site_locations <- fb_format_site_locations(
  data = all_data,
  site = "site",
  longitude = "longitude",
  latitude = "latitude",
  na_rm = FALSE
)

# Preview ----
head(site_locations)
#> Simple feature collection with 6 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1755000 ymin: 2635000 xmax: 1965000 ymax: 2645000
#> Geodetic CRS:  WGS 84
#>       site                geometry
#> 1 26351755 POINT (1755000 2635000)
#> 3 26351765 POINT (1765000 2635000)
#> 4 26351955 POINT (1955000 2635000)
#> 6 26351965 POINT (1965000 2635000)
#> 8 26451755 POINT (1755000 2645000)
#> 9 26451765 POINT (1765000 2645000)
```

### Extracting species x categories data

The function
[`fb_format_species_categories()`](https://frbcesab.github.io/funbiogeo/reference/fb_format_species_categories.md)
extracts species values for one supra-category (optional) from this long
table to create the species x categories dataset. This category
(e.g. order, family, endemism status, conservation status, etc.) can be
later by several functions in `funbiogeo` to aggregate metrics at this
level.

``` r

# Extract species x categories data ----
species_categories <- fb_format_species_categories(
  data = all_data,
  species = "species",
  category = "genus"
)

# Preview ----
head(species_categories, 10)
#>     species     genus
#> 1      JPHO Juniperus
#> 2      PPIR     Pinus
#> 7      PPIA     Pinus
#> 58     JNAV Juniperus
#> 372    JMAC Juniperus
#> 382    JOXY Juniperus
#> 486    JCOM Juniperus
#> 488    TBAC     Taxus
#> 573    PSYL     Pinus
#> 916    PHAL     Pinus
```

Once your data are in the good format, you can refer to the [Get Started
vignette](https://frbcesab.github.io/funbiogeo/articles/funbiogeo.html)
to continue your journey with `funbiogeo`.

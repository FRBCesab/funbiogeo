# Package index

## Included Datasets

- [`woodiv_traits`](https://frbcesab.github.io/funbiogeo/reference/woodiv_traits.md)
  : Species x Traits of some Mediterranean Conifers
- [`woodiv_site_species`](https://frbcesab.github.io/funbiogeo/reference/woodiv_site_species.md)
  : Sites x Species of some Mediterranean Conifers
- [`woodiv_locations`](https://frbcesab.github.io/funbiogeo/reference/woodiv_locations.md)
  : Sites x Locations of some Mediterranean Conifers
- [`woodiv_categories`](https://frbcesab.github.io/funbiogeo/reference/woodiv_categories.md)
  : Species x Categories of some Mediterranean Conifers

## Formatting Functions

Transform your raw dataset to the expected format by `funbiogeo`

- [`fb_format_site_locations()`](https://frbcesab.github.io/funbiogeo/reference/fb_format_site_locations.md)
  : Extract site x locations information from long format data
- [`fb_format_site_species()`](https://frbcesab.github.io/funbiogeo/reference/fb_format_site_species.md)
  : Extract site x species information from long format data
- [`fb_format_species_categories()`](https://frbcesab.github.io/funbiogeo/reference/fb_format_species_categories.md)
  : Extract species x categories information from long format data
- [`fb_format_species_traits()`](https://frbcesab.github.io/funbiogeo/reference/fb_format_species_traits.md)
  : Extract species x traits information from long format data

## Coverage functions

Count species, traits, sites by trait/species coverage (proportion of
species with known traits/present). Also filter datasets based on their
coverages.

### Count functions

- [`fb_count_sites_by_species()`](https://frbcesab.github.io/funbiogeo/reference/fb_count_sites_by_species.md)
  : Count Number of Sites Occupied by Species
- [`fb_count_species_by_site()`](https://frbcesab.github.io/funbiogeo/reference/fb_count_species_by_site.md)
  : Count Number of Species per Site
- [`fb_count_species_by_trait()`](https://frbcesab.github.io/funbiogeo/reference/fb_count_species_by_trait.md)
  : Count Number of Species for Each Trait
- [`fb_count_traits_by_species()`](https://frbcesab.github.io/funbiogeo/reference/fb_count_traits_by_species.md)
  : Compute Number of Known Trait(s) per Species

### Trait coverage functions

- [`fb_get_trait_combination_coverage()`](https://frbcesab.github.io/funbiogeo/reference/fb_get_trait_combination_coverage.md)
  : Compute site trait coverage for each trait combination
- [`fb_get_trait_coverage_by_site()`](https://frbcesab.github.io/funbiogeo/reference/fb_get_trait_coverage_by_site.md)
  : Compute Trait Coverage For Each Site Weighted by Abundance
- [`fb_get_all_trait_coverages_by_site()`](https://frbcesab.github.io/funbiogeo/reference/fb_get_all_trait_coverages_by_site.md)
  : Compute Trait Coverage per Site for Each Trait

### Filtering functions

- [`fb_filter_sites_by_species_coverage()`](https://frbcesab.github.io/funbiogeo/reference/fb_filter_sites_by_species_coverage.md)
  : Filter sites with a given species coverage threshold
- [`fb_filter_sites_by_trait_coverage()`](https://frbcesab.github.io/funbiogeo/reference/fb_filter_sites_by_trait_coverage.md)
  : Filter sites with a given trait coverage threshold
- [`fb_filter_species_by_site_coverage()`](https://frbcesab.github.io/funbiogeo/reference/fb_filter_species_by_site_coverage.md)
  : Filter species with a given sites coverage threshold
- [`fb_filter_species_by_trait_coverage()`](https://frbcesab.github.io/funbiogeo/reference/fb_filter_species_by_trait_coverage.md)
  : Filter species with a given traits coverage threshold
- [`fb_filter_traits_by_species_coverage()`](https://frbcesab.github.io/funbiogeo/reference/fb_filter_traits_by_species_coverage.md)
  : Filter traits with a given species coverage threshold

## Functional Diversity Functions

- [`fb_cwm()`](https://frbcesab.github.io/funbiogeo/reference/fb_cwm.md)
  : Compute community-weighted means (CWM) of trait values

## Upscaling functions

Aggregate your site data over a coarser spatial grid

- [`fb_get_environment()`](https://frbcesab.github.io/funbiogeo/reference/fb_get_environment.md)
  : Extract Raster Values at Location of Sites
- [`fb_aggregate_site_data()`](https://frbcesab.github.io/funbiogeo/reference/fb_aggregate_site_data.md)
  : Aggregate Site Data Along Coarser Spatial Grid

## Visualization & Summary functions

### Maps

- [`fb_map_raster()`](https://frbcesab.github.io/funbiogeo/reference/fb_map_raster.md)
  : Map a Single Raster Layer
- [`fb_map_site_data()`](https://frbcesab.github.io/funbiogeo/reference/fb_map_site_data.md)
  : Map Arbitrary Site Data
- [`fb_map_site_traits_completeness()`](https://frbcesab.github.io/funbiogeo/reference/fb_map_site_traits_completeness.md)
  : Map Trait Coverage Per Site

### Regular plots and summary

- [`fb_plot_distribution_site_trait_coverage()`](https://frbcesab.github.io/funbiogeo/reference/fb_plot_distribution_site_trait_coverage.md)
  : Plot Distribution of Trait Coverages across all Sites
- [`fb_plot_number_sites_by_species()`](https://frbcesab.github.io/funbiogeo/reference/fb_plot_number_sites_by_species.md)
  : Plot Number of Sites by Species
- [`fb_plot_number_species_by_trait()`](https://frbcesab.github.io/funbiogeo/reference/fb_plot_number_species_by_trait.md)
  : Plot Number of Species per Trait
- [`fb_plot_number_traits_by_species()`](https://frbcesab.github.io/funbiogeo/reference/fb_plot_number_traits_by_species.md)
  : Plot Number of Traits per Species
- [`fb_plot_site_environment()`](https://frbcesab.github.io/funbiogeo/reference/fb_plot_site_environment.md)
  : Plot Position of Sites in Environmental Space
- [`fb_plot_site_traits_completeness()`](https://frbcesab.github.io/funbiogeo/reference/fb_plot_site_traits_completeness.md)
  : Plot Trait Coverage per Site for each Trait
- [`fb_plot_species_traits_completeness()`](https://frbcesab.github.io/funbiogeo/reference/fb_plot_species_traits_completeness.md)
  : Plot Trait Coverage per Species for each Trait
- [`fb_plot_species_traits_missingness()`](https://frbcesab.github.io/funbiogeo/reference/fb_plot_species_traits_missingness.md)
  : Plot Trait Coverage per Species for each Trait
- [`fb_plot_trait_combination_frequencies()`](https://frbcesab.github.io/funbiogeo/reference/fb_plot_trait_combination_frequencies.md)
  : Plot Present and Missing Trait Combinations Frequencies
- [`fb_plot_trait_correlation()`](https://frbcesab.github.io/funbiogeo/reference/fb_plot_trait_correlation.md)
  : Plot Trait Correlation Matrix
- [`fb_table_trait_summary()`](https://frbcesab.github.io/funbiogeo/reference/fb_table_trait_summary.md)
  : Summary Table on Traits (Missingness, Range, etc.)

## Pre-analysis R Markdown report

- [`fb_make_report()`](https://frbcesab.github.io/funbiogeo/reference/fb_make_report.md)
  : Create an Rmarkdown Report to Explore User Data

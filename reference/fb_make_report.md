# Create an Rmarkdown Report to Explore User Data

Creates an R Markdown (`.Rmd`) report from a template to explore and
summarize user data in (functional) biogeography through the use of the
site-species, the species-traits, and the site-locations objects. Users
can modify this report and use the function
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
(or click the *Render* of the RStudio IDE) to convert this `.Rmd` in
different formats:

- HTML document (`output_format = "bookdown::html_document2"`);

- PDF document (`output_format = "bookdown::pdf_document2"`);

- Word document (`output_format = "bookdown::word_document2"`);

- HTML, PDF and Word documents (`output_format = "all"`).

## Usage

``` r
fb_make_report(
  path = ".",
  filename = NULL,
  title = NULL,
  author = NULL,
  species_traits,
  site_species,
  site_locations,
  species_categories = NULL,
  overwrite = FALSE,
  open = TRUE
)
```

## Arguments

- path:

  a `character` of length 1. The directory in which the `.Rmd` and
  `.rds` files will be created. This directory must exist. Note that
  subdirectories `funbiogeo/` and `funbiogeo/data/` will be created.
  Default is the current directory.

- filename:

  a `character` of length 1. The name of the `.Rmd` file to be created.
  If `NULL` (default) the `.Rmd` file will be named from the `title` (if
  provided) or `funbiogeo_report.Rmd` otherwise.

- title:

  a `character` of length 1. The title of the report. If `NULL`
  (default) the title will be named from the `title` (if provided) or
  `funbiogeo Report` otherwise.

- author:

  a `character` of length 1. The author(s) of the report. If `NULL`
  (default) no author will be added.

- species_traits:

  a `data.frame` with species in rows and traits as columns. **NOTE**:
  The first column should be named **`"species"`** and contain species
  names. The other columns should be named according to trait names.

- site_species:

  a `data.frame` with sites in rows and species in columns. **NOTE**:
  the first column should be named **`"site"`** and indicate site names.
  The other columns should be named according to species names.

- site_locations:

  a `sf` object with the spatial geometries of sites. **NOTE**: the
  first column should be named **`"site"`** and indicate site names.

- species_categories:

  (default = `NULL`) 2-columns `data.frame` giving species categories,
  with the first column describing the species name, and the second
  column giving their corresponding categories

- overwrite:

  a logical. If the `.Rmd` file (or any `.rds` dataset) is already
  present and `overwrite = TRUE`, the `.Rmd` file (and all `.rds` files)
  will be replaced. Default is `FALSE`.

- open:

  a logical. If `TRUE` (default), the `.Rmd` file will be opened in the
  text editor.

## Value

No return value.

## Details

Note that a copy of user data will be saved as `.rds` files in
`path/funbiogeo/data/` (where `path` is the directory defined by the
user).

## Examples

``` r
if (FALSE) { # \dontrun{
# Create temporary folder (optional) ----
temp_path <- tempdir()

# Create report ----
fb_make_report(
  path           = temp_path, 
  author         = "Casajus N. and Grenié M.",
  species_traits = woodiv_traits,
  site_species   = woodiv_site_species,
  site_locations = woodiv_locations,
  open           = FALSE
)

# Open Rmd file ----
utils::file.edit(file.path(temp_path, "funbiogeo", "funbiogeo_report.Rmd"))

# Render Rmd file ----
rmarkdown::render(
  input         = file.path(temp_path, "funbiogeo", "funbiogeo_report.Rmd"),
  output_format = "all"
)
} # }
```

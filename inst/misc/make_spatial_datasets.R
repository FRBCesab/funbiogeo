# Script to create different shapes of locations
# Packages ---------------------------------------------------------------------
library("dplyr")
library("ggplot2")
library("sf")

pkgload::load_all()


# Load data --------------------------------------------------------------------
countries <- rnaturalearth::ne_countries(returnclass = "sf")


# Create larger irregular polygons ---------------------------------------------
countries_sf <- countries |>
  subset(name %in% c("France", "Portugal", "Spain", "Italy"))
countries_sf <- countries_sf[, "name"]

countries_sf <- countries_sf |>
  sf::st_transform(sf::st_crs(woodiv_locations))

split_pols <- countries_sf |>
  st_cast("POLYGON")

# Remove French Guiana
countries_sf |>
  st_cast("POLYGON") |>
  dplyr::mutate(id = rownames(split_pols)) |>
  dplyr::filter(id != 44) |>
  ggplot(aes(fill = id)) +
  geom_sf()

countries_sf_no_gf <- countries_sf |>
  st_cast("POLYGON") |>
  dplyr::mutate(id = rownames(split_pols)) |>
  dplyr::filter(id != 44) |>
  dplyr::select(-id) |>
  dplyr::group_by(name) |>
  dplyr::summarise()


# Create points ----------------------------------------------------------------
set.seed(20250310)
woodiv_points <- woodiv_locations |>
  sf::st_centroid() |>
  slice_sample(n = 1e3)

woodiv_points |>
  ggplot() +
  geom_sf(alpha = 1 / 5, size = 0.5)


# Create transects -------------------------------------------------------------
woodiv_transects <- woodiv_points |>
  group_by(country) |>
  slice_sample(n = 6) |>
  summarise(do_union = FALSE) |>
  st_cast("LINESTRING")

woodiv_transects |>
  ggplot() +
  geom_sf(aes(color = country))

# Save datasets ----------------------------------------------------------------

list(
  countries_sf = countries_sf_no_gf,
  woodiv_points = woodiv_points,
  woodiv_transects = woodiv_transects
) |>
  purrr::iwalk(
    \(obj, name) saveRDS(obj, paste0("inst/extdata/", name, ".rds"))
  )

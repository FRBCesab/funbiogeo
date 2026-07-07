#'
#' Create an Hexagonal Sticker for the Package
#'

# Packages ---------------------------------------------------------------------
library("magrittr")


# Robinson projection ----------------------------------------------------------

prj <- paste0(
  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 ",
  "+ellps=WGS84 ",
  "+datum=WGS84 +units=m +no_defs"
)


# Get World map layer ----------------------------------------------------------

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- subset(
  sf::st_transform(world, prj),
  region_wb != "Antarctica" & 
  continent != "Seven seas (open ocean)" & 
  region_un != "Seven seas (open ocean)"
)

# Simplify map
world_simp <- world |>
  sf::st_cast("POLYGON")

world_simp$area <- sf::st_area(world_simp)

world_simp <- world_simp |>
  dplyr::filter(as.numeric(area) > 1e10)

world_simp <- world_simp |>
  rmapshaper::ms_simplify(keep = 0.015)


## Temperature Raster ----------------------------------------------------------

ras <- geodata::worldclim_global("tavg", 10, here::here("inst", "sticker"))

# Keep July Mean Temperature as target raster
target_raster <- ras[[7]]

sf_rast <- target_raster |>
  terra::project(prj) |>
  terra::mask(world_simp) |>
  terra::as.polygons() |>
  sf::st_as_sf()


# Map --------------------------------------------------------------------------

p <- ggplot2::ggplot() +

  ggplot2::geom_sf(
    data = world_simp,
    fill = "#0D3E6F",
    col = "white",
    linewidth = 0.5
  ) +
  ggplot2::geom_sf(
    data = sf_rast,
    ggplot2::aes(fill = wc2.1_10m_tavg_07),
    color = NA
  ) +

  ggplot2::coord_sf(
    ylim = c(-6145789, 8611877),
    expand = FALSE,
    crs = prj,
    clip = "off"
  ) +

  rphylopic::add_phylopic(
    uuid = "f20144d1-d243-4cca-aba2-24bce6c81d42",
    x = -2000000,
    y = -6000000,
    height = 1000000,
    fill = "#266E8C"
  ) +
  rphylopic::add_phylopic(
    uuid = "f20144d1-d243-4cca-aba2-24bce6c81d42",
    x = 1000000,
    y = -6000000,
    height = 1750000,
    fill = "#55C968"
  ) +
  rphylopic::add_phylopic(
    uuid = "f20144d1-d243-4cca-aba2-24bce6c81d42",
    x = 5000000,
    y = -6000000,
    height = 2500000,
    fill = "#FFE740"
  ) +

  ggplot2::geom_segment(
    ggplot2::aes(x = -2500000, y = -4400000, xend = 7050000, yend = -4400000),
    arrow = ggplot2::arrow(
      length = ggplot2::unit(0.1, "cm"),
      ends = "last",
      type = "closed"
    ),
    linewidth = 0.5,
    color = "white"
  ) +
  ggplot2::scale_fill_viridis_b() +

  ggplot2::theme_void() +
  ggpubr::theme_transparent() +
  ggplot2::theme(legend.position = "none")

p

## Export Sticker ----

s <- hexSticker::sticker(
  subplot = p,
  package = "funbiogeo",
  filename = here::here("man", "figures", "logo.png"),
  dpi = 2400,

  p_size = 150.0, # Title
  u_size = 32.0, # URL
  p_family = "Aller_Rg",

  p_color = "#ffffff", # Title
  h_fill = "#000000", # Background
  h_color = "#226D88", # Border
  u_color = "#ffffff", # URL

  p_x = 1.00, # Title
  p_y = 1.50, # Title
  s_x = 1.00, # Subplot
  s_y = 0.90, # Subplot

  s_width = 1.8, # Subplot
  s_height = 1.8, # Subplot

  url = "https://github.com/frbcesab/",

  spotlight = FALSE,
  l_alpha = 0.10,
  l_width = 4,
  l_height = 4
)

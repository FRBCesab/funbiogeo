#'
#' Create an Hexagonal Sticker for the Package
#'
library("magrittr")

## Robinson projection ----

prj <- paste0("+proj=robin +lon_0=0 +x_0=0 +y_0=0 ", "+ellps=WGS84 ", 
              "+datum=WGS84 +units=m +no_defs")


## Get World map layer ----

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- subset(sf::st_transform(world, prj), region_wb != "Antarctica",
                region_un != "Seven seas (open ocean)")


## Raster ----

ras <- geodata::worldclim_global("tavg", 10, "inst/sticker/")

# Keep July Mean Temperature as target raster
target_raster <- ras[[7]]

sf_rast <- target_raster %>%
  terra::as.polygons() %>%
  sf::st_as_sf() %>%
  sf::st_transform(prj) %>%
  sf::st_filter(world)


## Silhouette ----

sil <- png::readPNG(here::here("inst", "sticker", "plant-silhouette.png"))
sil <- grid::rasterGrob(sil, interpolate = TRUE)


## Map ----

p <- ggplot2::ggplot() +
  
  ggplot2::geom_sf(data = world, fill = "#0D3E6F", col = "#005094", lwd = 0.05) +
  ggplot2::geom_sf(data = sf_rast,
                   ggplot2::aes(fill = wc2.1_10m_tavg_07), color = NA) +
  
  ggplot2::coord_sf(ylim = c(-6145789, 8611877), expand = FALSE, crs  = prj,
                    clip = "off") +
  
  ggplot2::annotation_custom(sil, 
                             xmin = -16000000, xmax = -14000000, 
                             ymin =  -6000000, ymax =  -5000000) +
  ggplot2::annotation_custom(sil, 
                             xmin = -16000000, xmax = -14000000, 
                             ymin =   7000000, ymax =   8000000) +
  ggplot2::annotation_custom(sil, 
                             xmin = -17000000, xmax = -13000000,
                             ymin =  -2000000, ymax =   4000000) +
  
  ggplot2::geom_segment(ggplot2::aes(x    = -15000000, y    = -1250000, 
                                     xend = -15000000, yend = -4500000),
                        
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"), 
                                               ends = "last", type = "closed"), 
                        lwd = 0.3) +
  ggplot2::geom_segment(ggplot2::aes(x    = -15000000, y    = 3250000, 
                                     xend = -15000000, yend = 6500000),
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"), 
                                               ends = "last", type = "closed"), 
                        lwd = 0.3) +
  
  ggplot2::scale_fill_viridis_c() +

  ggplot2::theme_void() +
  ggpubr::theme_transparent() + 
  ggplot2::theme(legend.position = "none")


## Export Sticker ----

hexSticker::sticker(

  subplot   = p,
  package   = "funbiogeo",
  filename  = here::here("man", "figures", "funbiogeo-sticker.png"),
  dpi       = 2400,

  p_size    = 105.0,         # Title
  u_size    = 15.0,         # URL
  p_family  = "Aller_Rg",

  p_color   = "#ffffff",   # Title
  h_fill    = "#0B6116",   # Background
  h_color   = "#023408",   # Border
  u_color   = "#ffffff",   # URL

  p_x       = 1.00,        # Title
  p_y       = 0.60,        # Title
  s_x       = 1,        # Subplot
  s_y       = 1.15,        # Subplot

  s_width   = 1.8,         # Subplot 
  s_height  = 1.8,         # Subplot

  url       = "https://frbcesab.github.io/funbiogeo/",

  spotlight = TRUE,
  l_alpha   = 0.10,
  l_width   = 4,
  l_height  = 4
)

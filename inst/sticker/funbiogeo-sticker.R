#'
#' Create an Hexagonal Sticker for the Package
#'


## Robinson projection ----

prj <- paste0("+proj=robin +lon_0=0 +x_0=0 +y_0=0 ", "+ellps=WGS84 ", 
              "+datum=WGS84 +units=m +no_defs")


## Get World map layer ----

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- sf::st_transform(world, prj)


## Raster ----

# ras <- stars::read_stars("~/Desktop/wc2.1_10m_tavg_07.tif")
# ras <- stars::st_transform_proj(ras, prj)


## Silhouette ----

sil <- png::readPNG(here::here("inst", "sticker", "plant-silhouette.png"))
sil <- grid::rasterGrob(sil, interpolate = TRUE)


## Map ----

p <- ggplot2::ggplot() +
  
  ggplot2::geom_sf(data = world, fill = "#0D3E6F", col = "#005094", lwd = 0.05) +
  # stars::geom_stars(data = ras, downsample = 2, alpha = 1) +
  
  ggplot2::coord_sf(ylim = c(-6145789, 8611877), expand = FALSE, crs  = prj) +
  
  ggplot2::annotation_custom(sil, 
                             xmin = -13000000, xmax = -11000000, 
                             ymin =  -6000000, ymax =  -5000000) +
  ggplot2::annotation_custom(sil, 
                             xmin = -13000000, xmax = -11000000, 
                             ymin =   7000000, ymax =   8000000) +
  ggplot2::annotation_custom(sil, 
                             xmin = -14000000, xmax = -10000000,
                             ymin =  -2000000, ymax =   4000000) +
  
  ggplot2::geom_segment(ggplot2::aes(x    = -12000000, y    = -1250000, 
                                     xend = -12000000, yend = -4500000),
                        
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"), 
                                               ends = "last", type = "closed"), 
                        lwd = 0.25) +
  ggplot2::geom_segment(ggplot2::aes(x = -12000000, y = 3250000, 
                                     xend = -12000000, yend = 6500000),
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "cm"), 
                                               ends = "last", type = "closed"), 
                        lwd = 0.25) +

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
  h_fill    = "#005094",   # Background
  h_color   = "#0D3E6F",   # Border
  u_color   = "#ffffff",   # URL

  p_x       = 1.00,        # Title
  p_y       = 0.60,        # Title
  s_x       = 0.92,        # Subplot
  s_y       = 1.15,        # Subplot

  s_width   = 1.95,        # Subplot
  s_height  = 1.95,        # Subplot

  url       = "https://frbcesab.github.io/funbiogeo/",

  spotlight = TRUE,
  l_alpha   = 0.10,
  l_width   = 4,
  l_height  = 4
)

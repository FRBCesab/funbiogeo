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

ras <- geodata::worldclim_global("tavg", 10, here::here("inst", "sticker"))

# Keep July Mean Temperature as target raster
target_raster <- ras[[7]]

sf_rast <- target_raster %>%
  terra::as.polygons() %>%
  sf::st_as_sf() %>%
  sf::st_transform(prj) %>%
  sf::st_filter(world)


## Silhouette ----

sil <- rphylopic::image_data("f20144d1-d243-4cca-aba2-24bce6c81d42", size = 512)[[1]]


## Map ----

p <- ggplot2::ggplot() +
  
  ggplot2::geom_sf(data = world, fill = "#0D3E6F", col = "#005094", lwd = 0.05) +
  ggplot2::geom_sf(data = sf_rast,
                   ggplot2::aes(fill = wc2.1_10m_tavg_07), color = NA) +
  
  ggplot2::coord_sf(ylim = c(-6145789, 8611877), expand = FALSE, crs  = prj,
                    clip = "off") +
  
  # rphylopic::add_phylopic(sil, 1,        0, -6000000, ysize =  780000, color = "white") + 
  rphylopic::add_phylopic(sil, 1,        0, -6000000, ysize =  750000, color = "#266E8C") + 
  rphylopic::add_phylopic(sil, 1,  4000000, -6000000, ysize = 1500000, color = "#55C968") + 
  rphylopic::add_phylopic(sil, 1,  9000000, -6000000, ysize = 3000000, color = "#FFE740") + 

  ggplot2::geom_segment(ggplot2::aes(x    =   -500000, y    = -7900000,
                                     xend =  10550000, yend = -7900000),
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.05, "cm"),
                                               ends = "last", type = "closed"),
                        lwd = 0.1, color = "white") +
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

  p_size    = 140.0,         # Title
  u_size    =  32.0,         # URL
  p_family  = "Aller_Rg",

  p_color   = "#ffffff",   # Title
  h_fill    = "#000000",   # Background
  h_color   = "#226D88",   # Border
  u_color   = "#ffffff",   # URL

  p_x       = 1.00,        # Title
  p_y       = 1.50,        # Title
  s_x       = 1.00,        # Subplot
  s_y       = 0.90,        # Subplot

  s_width   = 1.8,         # Subplot 
  s_height  = 1.8,         # Subplot

  url       = "https://github.com/frbcesab/",

  spotlight = FALSE,
  l_alpha   = 0.10,
  l_width   = 4,
  l_height  = 4
)

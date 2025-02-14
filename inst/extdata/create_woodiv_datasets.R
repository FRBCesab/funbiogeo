## Script to create funbiogeo intenal datasets
## 
## Download 'WOODIV_DB_release_v1' dataset at:
## <https://figshare.com/ndownloader/files/26576827>



## Western Europe countries ----

selected_countries <- c("Portugal", "Spain", "France", "Italy", "Corsica", 
                        "Sardinia", "Sicily", "Balearic", "Gibraltar")


## Conifers species ----

families <- c("Cupressaceae", "Pinaceae", "Taxaceae")


## WOODIV dataset path ----

path_data <- file.path("~", "downloads", "WOODIV_DB_release_v1")


## Create 'woodiv_categories' ----

nomenclature <- read.csv(
  file.path(path_data, "SPECIES", "WOODIV_Nomenclature.csv")
)

nomenclature <- nomenclature[ , c("spcode", "family", "genus", "species", "subspecies")]

nomenclature <- nomenclature[nomenclature$"family" %in% families, ]
nomenclature <- nomenclature[nomenclature$"subspecies" == "", ]

nomenclature$"binomial" <- paste(nomenclature$"genus", nomenclature$"species")

nomenclature <- nomenclature[ , c("spcode", "family", "genus", "binomial")]

sp_status <- read.csv(
  file.path(path_data, "SPECIES", "WOODIV_Status.csv")
)

nomenclature <- merge(nomenclature, sp_status, by = "spcode", all = FALSE)
colnames(nomenclature)[1] <- "species"

woodiv_categories <- nomenclature



## Create 'woodiv_traits' dataset ----

sp_codes <- sort(unique(nomenclature$"species"))

traits <- read.csv(
  file.path(path_data, "TRAITS", "WOODIV_Trait_data.csv")
)

traits <- traits[ , c("spcode", "Traits", "value")]

traits <- traits[traits$"spcode" %in% sp_codes, ]

values <- tapply(
  traits$"value", 
  list(traits$"spcode", traits$"Traits"), 
  function(x) mean(x, na.rm = TRUE)
)

values <- data.frame(species = rownames(values), values, row.names = NULL)

colnames(values) <- c("species", "plant_height", "seed_mass", "sla", "wood_density")

woodiv_traits <- values



## Create 'woodiv_site_species' dataset ----

occurrence <- read.csv(
  file.path(path_data, "OCCURRENCE", "WOODIV_Occurrence_data.csv")
)

countries <- occurrence[ , c("Idgrid", "country")]
pos <- which(!duplicated(countries$"Idgrid"))

countries <- countries[pos, ]
countries <- countries[countries$"country" %in% selected_countries, ]

occurrence <- occurrence[ c('Idgrid', "spcode")]

occurrence$"spcode" <- substr(occurrence$"spcode", 1, 4)
pos <- which(!duplicated(paste(occurrence$"Idgrid", occurrence$"spcode")))
occurrence <- occurrence[pos, ]

occurrence <- occurrence[occurrence$"Idgrid" %in% countries$"Idgrid", ]
occurrence <- occurrence[occurrence$"spcode" %in% sp_codes, ]

occurrence$"value" <- 1

occurrence <- tidyr::pivot_wider(occurrence, names_from = spcode, values_from = value, values_fill = 0)

occurrence <- as.data.frame(occurrence)
colnames(occurrence)[1] <- "site"

woodiv_site_species <- occurrence



## Create 'woodiv_locations' dataset ----

spatial <- sf::st_read(
  file.path(path_data, "SPATIAL", "WOODIV_grid", "WOODIV_grid.shp")
)

spatial <- spatial[ , "Idgrid", drop = FALSE]


pos <- which(spatial$"Idgrid" %in% countries$"Idgrid")
spatial <- spatial[pos, ]

spatial <- merge(spatial, countries, by = "Idgrid", all = FALSE)
colnames(spatial)[1] <- "site"

spatial <- spatial[spatial$"site" %in% woodiv_site_species$"site", ]

woodiv_locations <- spatial



## Match species ----

species <- colnames(woodiv_site_species)[-1]

woodiv_categories <- woodiv_categories[woodiv_categories$"species" %in% species, ]
woodiv_traits     <- woodiv_traits[woodiv_traits$"species" %in% species, ]



## Save datasets ----

usethis::use_data(woodiv_categories, overwrite = TRUE)
usethis::use_data(woodiv_site_species, overwrite = TRUE)
usethis::use_data(woodiv_traits, overwrite = TRUE)
usethis::use_data(woodiv_locations, overwrite = TRUE)



## Create long table ----

xy <- woodiv_locations |> 
  sf::st_centroid() |> 
  sf::st_coordinates() |> 
  as.data.frame()

colnames(xy) <- c("longitude", "latitude")

woodiv_locations <- sf::st_drop_geometry(woodiv_locations)

woodiv_raw_data <- data.frame(woodiv_locations, xy)

woodiv_site_species <- tidyr::pivot_longer(woodiv_site_species, cols = -1, 
                                           names_to = "species", 
                                           values_to = "count")

woodiv_site_species <- as.data.frame(woodiv_site_species)
woodiv_site_species <- woodiv_site_species[woodiv_site_species$"count" == 1, ]

woodiv_raw_data <- merge(woodiv_raw_data, woodiv_site_species, by = "site", 
                         all = FALSE)

woodiv_raw_data <- merge(woodiv_raw_data, woodiv_categories, by = "species", 
                         all = TRUE
)

woodiv_raw_data <- merge(woodiv_raw_data, woodiv_traits, by = "species", 
                         all = TRUE
)

woodiv_raw_data <- woodiv_raw_data[ , 
  c("site", "country", "longitude", "latitude", "species", "count", "family", 
    "genus", "binomial", "endemism", "cultivated", "plant_height", 
    "seed_mass", "sla", "wood_density"
)]

woodiv_raw_data <- woodiv_raw_data[with(woodiv_raw_data, order(site, species)), ]

rownames(woodiv_raw_data) <- NULL

write.csv(woodiv_raw_data, 
          file = here::here("inst", "extdata", "woodiv_raw_data.csv"), 
          row.names = FALSE)

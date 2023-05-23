## Create a long-format dataset

data("species_categories")

data("site_species")

site_species <- tidyr::pivot_longer(data = site_species, 
                                    cols = colnames(site_species)[-1], 
                                    names_to = "species", values_to = "count")

site_species <- as.data.frame(site_species)
site_species <- site_species[site_species$"count" == 1, ]

long_format <- merge(site_species, species_categories, by = "species", all = FALSE)

data("species_traits")
long_format <- merge(long_format, species_traits, by = "species", all = FALSE)


data("site_locations")
site_locations <- data.frame(sf::st_drop_geometry(site_locations), sf::st_coordinates(sf::st_centroid(site_locations)))
site_locations <- site_locations[ , 1:3]
colnames(site_locations) <- c("site", "longitude", "latitude")

long_format <- merge(site_locations, long_format, by = "site", all = FALSE)

long_format <- long_format[with(long_format, order(site, species)), ]
rownames(long_format) <- NULL

write.csv(long_format, here::here("inst", "extdata", "raw_mammals_data.csv"),
          row.names = FALSE)

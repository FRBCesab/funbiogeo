#' Sites x Locations of Western Europe Mammals
#' 
#' This dataset contains spatial coordinates (grid cells of 0.5° x 0.5° 
#' horizontal resolution) of sites of Western Europe where mammals species 
#' presence/absence (see `site_species` dataset) has been sampled.
#' 
#' See \url{https://github.com/frbcesab/eumammals} for a complete description 
#' of the workflow used to create this dataset.
#' 
#' This dataset shows the format of the argument `site_locations` used in 
#' several functions of `funbiogeo`.
#' 
#' The variable `site` corresponds to the sites labels.
#'   
#' @format An `sf` object with 1,505 rows (grid cells) with a WGS84 (EPSG:4236)
#'   Coordinate Reference System.
#' 
#' @examples 
#' data("site_locations")
#' class(site_locations)
#' dim(site_locations)
#' head(site_locations)

"site_locations"



#' Sites x Species of Western Europe Mammals
#' 
#' This dataset contains the presence/absence of mammals species in Western
#' Europe. This dataset is derived from IUCN range maps downloaded at:
#' \url{https://www.iucnredlist.org/resources/spatial-data-download}. In 
#' respect to the license, species names have been anonymized.
#' 
#' See \url{https://github.com/frbcesab/eumammals} for a complete description 
#' of the workflow used to create this dataset.
#' 
#' This dataset shows the format of the argument `site_species` used in 
#' several functions of `funbiogeo`.
#' 
#' Note that sites labels (`site`) is the first column of this dataset.
#'   
#' @format A `data.frame` with 1,505 rows (sites) and 150 columns (1 column for 
#' site label and 149 for species occurrence).
#' 
#' @references 
#' IUCN (2021) The IUCN Red List of Threatened Species. Version 2021-2.
#' Downloaded on 2021/10/11.
#' 
#' @examples 
#' data("site_species")
#' class(site_species)
#' dim(site_species)
#' site_species[1:6, 1:6]

"site_species"



#' Species x Traits of Western Europe Mammals
#' 
#' This dataset contains values for six functional traits of Western Europe
#' mammals species. These values were extracted from the **PanTHERIA** database
#' (Jones 2009), a database on World mammals traits available at: 
#' \url{https://esapubs.org/archive/ecol/E090/184/metadata.htm}.
#' In respect to the IUCN license, species names have been anonymized.
#' 
#' See \url{https://github.com/frbcesab/eumammals} for a complete description 
#' of the workflow used to create this dataset.
#' 
#' This dataset shows the format of the argument `species_traits` used in 
#' several functions of `funbiogeo`.
#' 
#' Note that species names (`species`) is the first column of this dataset.
#'   
#' @format A matrix with 149 rows (species) and the following seven columns:
#' \describe{
#'   \item{species}{species name corresponding to the columns of `site_species`}
#'   \item{adult_body_mass}{adult body mass (in grams)}
#'   \item{gestation_length}{length of time of non-inactive fetal growth 
#'   (in days)}
#'   \item{litter_size}{number of offspring born per litter per female}
#'   \item{max_longevity}{maximum adult age (in months)}
#'   \item{sexual_maturity_age}{age when individuals are first physically 
#'   capable of reproducing (in days)}
#'   \item{diet_breadth}{number of dietary categories eaten}
#' }
#' 
#' @references 
#' Jones KE _et al._ (2009) PanTHERIA: A species-level database of life 
#' history, ecology, and geography of extant and recently extinct mammals. 
#' _Ecology_, **90**, 2648. DOI: 10.1890/08-1494.1.
#' 
#' @examples 
#' data("species_traits")
#' class(species_traits)
#' dim(species_traits)
#' head(species_traits)

"species_traits"



#' Species x Categories of Western Europe Mammals
#' 
#' This dataset contains the order of mammals species in Western
#' Europe. This dataset is derived from IUCN range maps downloaded at:
#' \url{https://www.iucnredlist.org/resources/spatial-data-download}. In 
#' respect to the license, species names have been anonymized.
#' 
#' See \url{https://github.com/frbcesab/eumammals} for a complete description 
#' of the workflow used to create this dataset.
#' 
#' This dataset shows the format of the argument `species_categories` used in 
#' several functions of `funbiogeo`.
#' 
#' Note that species names (`species`) is the first column of this dataset.
#'   
#' @format A `data.frame` with 149 rows (species) and 2 columns (1 column for 
#' species name and 1 for the order).
#' 
#' @references 
#' IUCN (2021) The IUCN Red List of Threatened Species. Version 2021-2.
#' Downloaded on 2021/10/11.
#' 
#' @examples 
#' data("species_categories")
#' class(species_categories)
#' dim(species_categories)
#' species_categories[1:6, ]

"species_categories"

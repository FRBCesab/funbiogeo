#' Data: sites x locations of Western Europe mammals occurrences
#' 
#' @description
#' This dataset contains spatial coordinates of sites of Western Europe where
#' species presence/absence (see `site_species` dataset) has been sampled.
#' 
#' See https://github.com/frbcesab/eumammals for a complete description of the
#' workflow used to create this dataset.
#' 
#' This dataset shows the format of the argument `site_locations` used in 
#' several functions of `funbiogeo`.
#' 
#' Note that sites labels are specified in row names.
#'   
#' @format A `data.frame` with 1,505 rows (sites) and the following two 
#' columns:
#' \describe{
#'   \item{x}{longitude of the site (in WGS84)}
#'   \item{y}{latitude of the site (in WGS84)}
#' }
#' 
#' @examples 
#' data("site_locations")
#' class(site_locations)
#' dim(site_locations)
#' head(site_locations)

"site_locations"



#' Data: sites x species of Western Europe mammals occurrences
#' 
#' @description
#' This dataset contains the presence/absence of mammals species in Western
#' Europe. This dataset is derived from IUCN range maps downloaded at:
#' https://www.iucnredlist.org/resources/spatial-data-download. In respect to
#' the license, species names have been anonymized.
#' 
#' See https://github.com/frbcesab/eumammals for a complete description of the
#' workflow used to create this dataset.
#' 
#' This dataset shows the format of the argument `site_species` used in 
#' several functions of `funbiogeo`.
#' 
#' Note that sites labels are specified in row names.
#'   
#' @format A `data.frame` with 1,505 rows (sites) and 149 columns (species 
#' occurrence).
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



#' Data: species x traits of Western Europe mammals
#' 
#' @description
#' This dataset contains values for six functional traits of Western Europe
#' mammals species. These values were extracted from the **PanTHERIA** database
#' (Jones 2009), a database on World mammals traits available at: 
#' https://esapubs.org/archive/ecol/E090/184/metadata.htm.
#' In respect to the IUCN license, species names have been anonymized.
#' 
#' See https://github.com/frbcesab/eumammals for a complete description of the
#' workflow used to create this dataset.
#' 
#' This dataset shows the format of the argument `species_traits` used in 
#' several functions of `funbiogeo`.
#' 
#' Note that species names are specified in row names.
#'   
#' @format A matrix with 149 rows (species) and the following six columns:
#' \describe{
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

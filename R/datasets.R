#' Coordinates of Pennsylvania survey sites
#' 
#' @description
#' This dataset contains spatial coordinates of sites sampled in Pennsylvania 
#' where trees species were counted (individuals counts). This dataset shows 
#' the format of the argument `sites_locations` used in several functions of
#' `funbiogeo`. Note that sites labels are specified in row names.
#'   
#' @format A data frame with 5,770 rows (sites) and the following two columns:
#' \describe{
#'   \item{longitude}{longitude of the site (in WGS84)}
#'   \item{latitude}{latitude of the site (in WGS84)}
#' }
#' 
#' @examples 
#' data("sites_locs")
#' head(sites_locs)

"sites_locs"



#' Tree species counts in Pennsylvania survey sites
#' 
#' @description
#' This dataset contains trees species counts in sites sampled in Pennsylvania. 
#' This dataset shows the format of the argument `site_species` used in 
#' several functions of `funbiogeo`. Note that sites labels are specified in 
#' row names.
#'   
#' @format A matrix with 5,770 rows (sites) and 93 columns (species).
#' 
#' @examples 
#' data("species_occs")
#' class(species_occs)
#' dim(species_occs)
#' species_occs[1:6, 1:6]

"species_occs"



#' Tree species traits
#' 
#' @description
#' This dataset contains traits of trees species. It shows the format of the 
#' argument `species_traits` used in several functions of `funbiogeo`. Note 
#' that species names are specified in row names.
#'   
#' @format A matrix with 67 rows (species) and the following three columns:
#' \describe{
#'   \item{sla}{Specific Leaf Area}
#'   \item{n_mass}{Mass}
#'   \item{wood_dens}{Wood density}
#' }
#' 
#' @examples 
#' data("species_traits")
#' class(species_traits)
#' dim(species_traits)
#' head(species_traits)

"species_traits"

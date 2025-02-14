#' Sites x Locations example data
#' 
#' @description
#' This dataset is derived from the WOODIV database (available at:
#' \url{https://www.nature.com/articles/s41597-021-00873-3}). It contains the 
#' spatial coordinates of sites (grid cells of 10 x 10 km  horizontal 
#' resolution) sampled in Portugal, Spain, France, and Italy (Mediterranean 
#' part) for which at least one of the 24 Conifer tree species occurs.
#' 
#' This dataset shows the format of the argument `site_locations` used in 
#' several functions of `funbiogeo`. The variable `site` corresponds to the 
#' site labels (must match the same column in `site_species`) and the variable
#' `country` will be used as a site grouping factor.
#'   
#' @format An `sf` `POLYGON` object with 5,366 rows (grid cells) defined in the
#'   ETRS89-extended / LAEA Europe Coordinate Reference System (EPSG:3035).
#' 
#' @references 
#' Monnet AC, Cilleros K, Médail F _et al._ (2021) WOODIV, a database of 
#' occurrences, functional traits, and phylogenetic data for all 
#' Euro-Mediterranean trees. **Scientific Data**, 8, 89.
#' DOI: [10.1038/s41597-021-00873-3](https://doi.org/10.1038/s41597-021-00873-3)
#' 
#' @examples 
#' data("woodiv_locations")
#' class(woodiv_locations)
#' dim(woodiv_locations)
#' head(woodiv_locations)
"woodiv_locations"



#' Sites x Species example data
#' 
#' @description
#' This dataset is derived from the WOODIV database (available at:
#' \url{https://www.nature.com/articles/s41597-021-00873-3}). It contains the 
#' presence/absence of the 24 Conifer tree species occurring in Portugal, 
#' Spain, France, and Italy (Mediterranean part). The presence/absence is 
#' available at 5,366 sites (grid cells of 10 x 10 km horizontal resolution). 
#' Sites coordinates are provided in `site_locations`.
#' 
#' This dataset shows the format of the argument `site_species` used in 
#' several functions of `funbiogeo`. Note that site labels must be the first 
#' column and must be named `site`.
#'   
#' @format A `data.frame` with 5,366 rows (sites) and 25 columns (1 column for 
#' site label and 24 for species occurrence).
#' 
#' @references 
#' Monnet AC, Cilleros K, Médail F _et al._ (2021) WOODIV, a database of 
#' occurrences, functional traits, and phylogenetic data for all 
#' Euro-Mediterranean trees. **Scientific Data**, 8, 89.
#' DOI: [10.1038/s41597-021-00873-3](https://doi.org/10.1038/s41597-021-00873-3)
#' 
#' @examples 
#' data("woodiv_site_species")
#' class(woodiv_site_species)
#' dim(woodiv_site_species)
#' woodiv_site_species[1:6, 1:6]
"woodiv_site_species"



#' Species x Traits example data
#' 
#' @description
#' This dataset is derived from the WOODIV database (available at:
#' \url{https://www.nature.com/articles/s41597-021-00873-3}). It contains the 
#' values of four functional traits for 24 Conifer tree species occurring in 
#' Portugal, Spain, France, and Italy (Mediterranean part).
#' 
#' This dataset shows the format of the argument `species_traits` used in 
#' several functions of `funbiogeo`. Note that species names must be
#' the first column and must be named `species`.
#'   
#' @format A `data.frame` with 24 rows (species) and the following five columns:
#' \describe{
#'   \item{species}{species name corresponding to the columns of `site_species`}
#'   \item{plant_height}{adult plant height (in \eqn{m})}
#'   \item{seed_mass}{seed mass (in \eqn{g})}
#'   \item{sla}{specific leaf area, i.e. the ratio between leaf area and dry 
#'   mass (in \eqn{m^{2}\cdot kg^{−1}})}
#'   \item{wood_density}{wood density, i.e. stem specific density 
#'   (in \eqn{kg\cdot dm^{–3}})}
#' }
#' 
#' @references 
#' Monnet AC, Cilleros K, Médail F _et al._ (2021) WOODIV, a database of 
#' occurrences, functional traits, and phylogenetic data for all 
#' Euro-Mediterranean trees. **Scientific Data**, 8, 89.
#' DOI: [10.1038/s41597-021-00873-3](https://doi.org/10.1038/s41597-021-00873-3)
#' 
#' @examples 
#' data("woodiv_traits")
#' class(woodiv_traits)
#' dim(woodiv_traits)
#' head(woodiv_traits)
"woodiv_traits"



#' Species x Categories example data
#' 
#' @description
#' This dataset is derived from the WOODIV database (available at:
#' \url{https://www.nature.com/articles/s41597-021-00873-3}). It contains 
#' information (classification, endemism, etc.) of the 24 Conifer tree species 
#' occurring in Portugal, Spain, France, and Italy (Mediterranean part).
#' This information will be used as a species grouping factor.
#' 
#' This dataset shows the format of the argument `species_categories` used in 
#' several functions of `funbiogeo`. Note that species names must be
#' the first column and must be named `species`.
#'   
#' @format A `data.frame` with 24 rows (species) and 6 columns (`species`, 
#' `family`, `genus`, `binomial`, `endemism`, and `cultivated`).
#' 
#' @references 
#' Monnet AC, Cilleros K, Médail F _et al._ (2021) WOODIV, a database of 
#' occurrences, functional traits, and phylogenetic data for all 
#' Euro-Mediterranean trees. **Scientific Data**, 8, 89.
#' DOI: [10.1038/s41597-021-00873-3](https://doi.org/10.1038/s41597-021-00873-3)
#' 
#' @examples 
#' data("woodiv_categories")
#' class(woodiv_categories)
#' dim(woodiv_categories)
#' woodiv_categories[1:6, ]
"woodiv_categories"

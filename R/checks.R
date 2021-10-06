#' Check sites x species object format
#'
#' @description
#' The object should be either a `data.frame` or a `matrix` with only positive
#' real numbers or `NA` (of the good type!).
#' 
#' @param sites_species A sites x species object (`data.frame` or a `matrix`).
#'
#' @return NULL
#' 
#' @noRd

check_sites_species <- function(sites_species) {

  
  # Check object type ----
  stop_condition(
    !is.data.frame(sites_species) & !is.matrix(sites_species), 
    "The sites x species object must be a matrix or a data.frame"
  )
  
  stop_condition(
    0 %in% dim(sites_species), 
    "The sites x species object should have at least one row and one column"
  )
  
  
  # Check for labels (sites and species) ----

  if (is.matrix(sites_species)) {
    
    stop_condition(
      is.null(rownames(sites_species)), 
      "The sites x species object must have row names (sites names)"
    )
    
    stop_condition(
      is.null(colnames(sites_species)),
      "The sites x species object must have column names (species names)"
    )
    
  }
  
  
  if (is.data.frame(sites_species)) {
    
    stop_condition(
      any(rownames(sites_species) %in% seq_nrow(sites_species)),
      "The sites x species object must have row names (sites names)"
    )
    
    stop_condition(
      any(colnames(sites_species) %in% paste0("V", seq_nrow(sites_species))),
      "The sites x species object must have column names (species names)"
    )

    stop_condition(
      is.null(colnames(sites_species)),
      "The sites x species object must have column names (species names)"
    )

  }
  
  
  stop_condition(
    !is.numeric(as.matrix(sites_species)),
    "The sites x species object must contain only numeric values. Sites ", 
    "names must be provided as row names"
  )
  
  stop_condition(
    any(sites_species < 0, na.rm = TRUE),
    "The sites x species object cannot contain negative values"
  )
  
  invisible(NULL)
} 



#' Check species x traits object format
#' 
#' @description
#' The object should be either a `data.frame` or a `matrix`.
#' 
#' @param species_traits A species x traits object (`data.frame` or a `matrix`)
#'
#' @return NULL
#'
#' @noRd

check_species_traits <- function(species_traits) {
  
  
  # Check object type ----
  
  stop_condition(
    !is.data.frame(species_traits) & !is.matrix(species_traits),
    "The species x traits object must be a matrix or a data.frame"
  )

  stop_condition(
    0 %in% dim(species_traits),
    "The species x traits object should have at least one row and one column"
  )
  
  
  # Check for labels (sites and species) ----
  
  if (is.matrix(species_traits)) {
    
    stop_condition(
      is.null(rownames(species_traits)),
      "The species x traits object must have row names (species names)"
    )
    
    stop_condition(
      is.null(colnames(species_traits)),
      "The species x traits object must have column names (traits names)"
    )
    
  }
  
  
  if (is.data.frame(species_traits)) {
    
    stop_condition(
      any(rownames(species_traits) %in% seq_nrow(species_traits)),
      "The species x traits object must have row names (species names)"
    )
    
    stop_condition(
      any(colnames(species_traits) %in% paste0("V", seq_ncol(species_traits))),
      "The species x traits object must have column names (traits names)"
    )

    stop_condition(
      is.null(colnames(species_traits)),
      "The species x traits object must have column names (traits names)"
    )

  }
  
  invisible(NULL)
}



#' Check sites x locations object format
#'
#' Errors if the object is not an `sf` object and returns NULL otherwise.
#' 
#' @param sites_locations an `sf` object with each sites defined as points
#'
#' @return NULL
#' 
#' @noRd

check_sites_locations <- function(sites_locations) {
  
  # Check object type ----
  
  stop_condition(
    !is.data.frame(sites_locations) & !is.matrix(sites_locations),
    "The sites x locations object must be a matrix or a data.frame"
  )
  
  stop_condition(
    nrow(sites_locations) == 0,
    "The sites x locations object should have at least one row"
  )
  
  stop_condition(
    ncol(sites_locations) != 2,
    "The sites x locations object should have two columns (longitude and ",
    "latitude)"
  )
  
  
  # Check for labels (sites) ----
  
  if (is.matrix(sites_locations)) {
    
    stop_condition(
      is.null(rownames(sites_locations)),
      "The sites x locations object must have row names (sites names)"
    )
    
  }
  
  
  if (is.data.frame(sites_locations)) {
    
    stop_condition(
      any(rownames(sites_locations) %in% seq_nrow(sites_locations)),
      "The sites x locations object must have row names (sites names)"
    )
    
  }
  
  invisible(NULL)
}

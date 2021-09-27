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
  
  if (!is.data.frame(sites_species) & !is.matrix(sites_species)) {
    stop("The sites x species object must be a matrix or a data.frame", 
         call. = FALSE)
  }
  
  if (0 %in% dim(sites_species)) {
    stop("The sites x species object should have at least one row and ",
         "one column", call. = FALSE)
  }
  
  
  # Check for labels (sites and species) ----

  if (is.matrix(sites_species)) {
    
    if (is.null(rownames(sites_species))) {
      stop("The sites x species object must have row names (sites names)", 
           call. = FALSE)
    }
    
    if (is.null(colnames(sites_species))) {
      stop("The sites x species object must have column names (species names)",
           call. = FALSE)
    }
  }
  
  
  if (is.data.frame(sites_species)) {
    
    if (any(rownames(sites_species) %in% 1:nrow(sites_species))) {
      stop("The sites x species object must have row names (sites names)", 
           call. = FALSE)
    }
    
    if (any(colnames(sites_species) %in% paste0("V", 1:ncol(sites_species)))) {
      stop("The sites x species object must have column names (species names)", 
           call. = FALSE)
    }
    
    if (is.null(colnames(sites_species))) {
      stop("The sites x species object must have column names (species names)",
           call. = FALSE)
    }
  }
  
  if (!is.numeric(as.matrix(sites_species))) {
    stop("The sites x species object must contain only numeric values. Sites ", 
         "names must be provided as row names", call. = FALSE)
  }
  
  if (any(sites_species < 0, na.rm = TRUE)) {
    stop("The sites x species object cannot contain negative values", 
         call. = FALSE)
  }
  
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
  
  if (!is.data.frame(species_traits) & !is.matrix(species_traits)) {
    stop("The species x traits object must be a matrix or a data.frame", 
         call. = FALSE)
  }
  
  if (0 %in% dim(species_traits)) {
    stop("The species x traits object should have at least one row and ",
         "one column", call. = FALSE)
  }
  
  
  # Check for labels (sites and species) ----
  
  if (is.matrix(species_traits)) {
    
    if (is.null(rownames(species_traits))) {
      stop("The species x traits object must have row names (sites names)", 
           call. = FALSE)
    }
    
    if (is.null(colnames(species_traits))) {
      stop("The species x traits object must have column names (species names)",
           call. = FALSE)
    }
  }
  
  
  if (is.data.frame(species_traits)) {
    
    if (any(rownames(species_traits) %in% 1:nrow(species_traits))) {
      stop("The species x traits object must have row names (sites names)", 
           call. = FALSE)
    }
    
    if (any(colnames(species_traits) %in% 
            paste0("V", 1:ncol(species_traits)))) {
      stop("The species x traits object must have column names (species names)",
           call. = FALSE)
    }
    
    if (is.null(colnames(species_traits))) {
      stop("The species x traits object must have column names (species names)",
           call. = FALSE)
    }
  }
  
  invisible(NULL)
}



#' Check site x locations object format
#'
#' Errors if the object is not an `sf` object and returns NULL otherwise.
#' 
#' @param site_locations an `sf` object with each sites defined as points
#'
#' @return NULL
#' 
#' @noRd
check_site_locations <- function(site_locations) {
  
  if (!inherits(site_locations, "sf")) {
    stop("The site x locations object should be an 'sf' object", call. = FALSE)
  }
  
  invisible(NULL)
}

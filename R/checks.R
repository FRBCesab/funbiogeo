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
    
    if (any(rownames(sites_species) %in% seq_len(nrow(sites_species)))) {
      stop("The sites x species object must have row names (sites names)", 
           call. = FALSE)
    }
    
    if (any(colnames(sites_species) %in% 
            paste0("V", seq_len(nrow(sites_species))))) {
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
      stop("The species x traits object must have row names (species names)", 
           call. = FALSE)
    }
    
    if (is.null(colnames(species_traits))) {
      stop("The species x traits object must have column names (traits names)",
           call. = FALSE)
    }
  }
  
  
  if (is.data.frame(species_traits)) {
    
    if (any(rownames(species_traits) %in% seq_len(nrow(species_traits)))) {
      stop("The species x traits object must have row names (species names)", 
           call. = FALSE)
    }
    
    if (any(colnames(species_traits) %in% 
            paste0("V", seq_len(ncol(species_traits))))) {
      stop("The species x traits object must have column names (traits names)",
           call. = FALSE)
    }
    
    if (is.null(colnames(species_traits))) {
      stop("The species x traits object must have column names (traits names)",
           call. = FALSE)
    }
  }
  
  invisible(NULL)
}



#' Check sites x locations object format
#'
#' Errors if the object is not an `sf` object and returns NULL otherwise.
#' 
#' @param sites_locations an `sf` object with all sites.
#'
#' @return `NULL` if the object passes the test, errors otherwise 
#' 
#' @noRd

check_sites_locations <- function(sites_locations) {
  
  # Check object type ----
  
  if (!inherits(sites_locations, "sf")) {
    stop("The sites x locations object must be an 'sf' object", 
         call. = FALSE)
  }
  
  if (nrow(sites_locations) == 0) {
    stop("The sites x locations object should have at least one row", 
         call. = FALSE)
  }
  
  invisible(NULL)
}

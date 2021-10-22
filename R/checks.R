#' Check site x species object format
#'
#' @description
#' The object should be either a `data.frame` or a `matrix` with only positive
#' real numbers or `NA` (of the good type!).
#' 
#' @param site_species A site x species object (`data.frame` or a `matrix`).
#'
#' @return NULL
#' 
#' @noRd

check_site_species <- function(site_species) {

  
  # Check object type ----
  
  if (!is.data.frame(site_species) & !is.matrix(site_species)) {
    stop("The site x species object must be a matrix or a data.frame", 
         call. = FALSE)
  }
  
  if (0 %in% dim(site_species)) {
    stop("The site x species object should have at least one row and ",
         "one column", call. = FALSE)
  }
  
  
  # Check for labels (sites and species) ----

  if (is.matrix(site_species)) {
    
    if (is.null(rownames(site_species))) {
      stop("The site x species object must have row names (sites names)", 
           call. = FALSE)
    }
    
    if (is.null(colnames(site_species))) {
      stop("The site x species object must have column names (species names)",
           call. = FALSE)
    }
  }
  
  
  if (is.data.frame(site_species)) {
    
    if (any(rownames(site_species) %in% seq_len(nrow(site_species)))) {
      stop("The site x species object must have row names (sites names)", 
           call. = FALSE)
    }
    
    if (any(colnames(site_species) %in% 
            paste0("V", seq_len(nrow(site_species))))) {
      stop("The site x species object must have column names (species names)", 
           call. = FALSE)
    }
    
    if (is.null(colnames(site_species))) {
      stop("The site x species object must have column names (species names)",
           call. = FALSE)
    }
  }
  
  if (!is.numeric(as.matrix(site_species))) {
    stop("The site x species object must contain only numeric values. Sites ", 
         "names must be provided as row names", call. = FALSE)
  }
  
  if (any(site_species < 0, na.rm = TRUE)) {
    stop("The site x species object cannot contain negative values", 
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



#' Check site x locations object format
#'
#' Errors if the object is not an `sf` object and returns NULL otherwise.
#' 
#' @param site_locations an `sf` object with all sites.
#'
#' @return `NULL` if the object passes the test, errors otherwise 
#' 
#' @noRd

check_site_locations <- function(site_locations) {
  
  # Check object type ----
  
  if (!inherits(site_locations, "sf")) {
    stop("The site x locations object must be an 'sf' object", 
         call. = FALSE)
  }
  
  if (nrow(site_locations) == 0) {
    stop("The site x locations object should have at least one row", 
         call. = FALSE)
  }
  
  invisible(NULL)
}

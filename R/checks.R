#' Check site x species object format
#'
#' @description
#' The object should be a `data.frame` with only positive real numbers or `NA`
#' (of the good type!). The `data.frame` should also contain a column named
#' `"site"` containing site names.
#' 
#' @param site_species A site x species object (`data.frame`).
#'
#' @return `NULL` if object passes the check, errors otherwise.
#' 
#' @noRd

check_site_species <- function(site_species) {
  
  
  # Check missing --------------------------------------------------------------
  
  if (missing(site_species)) {
    stop(
      "Argument 'site_species' (site x species data frame) is required",
      call. = FALSE
    )
  }

  
  # Check object type ----------------------------------------------------------
  
  if (!is.data.frame(site_species)) {
    stop("The site x species object must be a data.frame", 
         call. = FALSE)
  }
  
  if (0 %in% dim(site_species)) {
    stop("The site x species object should have at least one row and ",
         "one column", call. = FALSE)
  }
  
  
  # Check for labels (sites and species) ---------------------------------------
  
  if (is.data.frame(site_species)) {
    
    if (is.null(colnames(site_species))) {
      stop("The site x species object must have column names (species names)",
           call. = FALSE)
    }
    
    if (!("site" %in% colnames(site_species))) {
      stop("The site x species object must contain the 'site' column", 
           call. = FALSE)
    }
  }
  
  
  # Check for negative values --------------------------------------------------
  
  if (any(site_species < 0, na.rm = TRUE)) {
    stop("The site x species object cannot contain negative values", 
         call. = FALSE)
  }
  
  invisible(NULL)
} 



#' Check species x traits object format
#' 
#' @description
#' The object should be a `data.frame` containing a column named `"species"`
#' containing species names.
#' 
#' @param species_traits A species x traits object (a `data.frame`)
#'
#' @return `NULL` if object passes the check, errors otherwise.
#'
#' @noRd

check_species_traits <- function(species_traits) {
  
  
  # Check missing --------------------------------------------------------------
  
  if (missing(species_traits)) {
    stop(
      "Argument 'species_traits' (species x traits data frame) is required",
      call. = FALSE
    )
  }
  
  
  # Check object type ----------------------------------------------------------
  
  if (!is.data.frame(species_traits)) {
    stop("The species x traits object must be a data.frame", 
         call. = FALSE)
  }
  
  if (0 %in% dim(species_traits)) {
    stop("The species x traits object should have at least one row and ",
         "one column", call. = FALSE)
  }
  
  
  # Check for labels (sites and species) ---------------------------------------
  
  if (is.data.frame(species_traits)) {
    
    if (is.null(colnames(species_traits))) {
      stop("The species x traits object must have column names (trait names)",
           call. = FALSE)
    }
    
    if (!("species" %in% colnames(species_traits))) {
      stop("The species x traits object must contain the 'species' column",
           call. = FALSE)
    }
  }
  
  invisible(NULL)
}



#' Check site x locations object format
#'
#' @description
#' Errors if the object is not an `sf` object and returns `NULL` otherwise.
#' 
#' @param site_locations an `sf` object with all sites.
#'
#' @return `NULL` if the object passes the test, errors otherwise.
#' 
#' @noRd

check_site_locations <- function(site_locations) {
  
  
  # Check missing --------------------------------------------------------------
  
  if (missing(site_locations)) {
    stop(
      "Argument 'sites_locations' (spatial sites 'sf' object) is required",
      call. = FALSE
    )
  }
  
  
  # Check object type ----------------------------------------------------------
  
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



#' Check given threshold
#' 
#' @description
#' This function will error if threshold is missing or if it is not numeric
#' or if it's below 0 or above 1.
#' It will remain silent otherwise.
#'
#' @param threshold a `numeric` of length 1. The threshold argument to be 
#'   checked.
#' 
#' @param type a `character` of length 1. The name of the type of the 
#'   threshold which is going to be reused in error messages.
#'
#' @noRd

check_threshold_proportion <- function(
    threshold, type = c("trait", "site", "species")
) {
  
  
  type <- match.arg(type)
  
  
  # Check missing --------------------------------------------------------------
  
  if (missing(threshold)) {
    stop(
      "Argument '", deparse(substitute(threshold)), "' (", type,
      " coverage) is required", call. = FALSE
    )
  }
  
  
  # Check object type ----------------------------------------------------------
  
  if (!is.numeric(threshold)) {
    stop(
      "Argument '", deparse(substitute(threshold)), "' (", type,
      " coverage proportion) must be numeric", call. = FALSE
    )
  }
  
  
  # Check values ---------------------------------------------------------------
  
  if (threshold < 0 | threshold > 1) {
    stop(
      "Argument '", deparse(substitute(threshold)), "' (", type,
      " coverage proportion) should be a numeric value >= 0 and <= 1",
      call. = FALSE
    )
  }
  
  invisible(NULL)
}



#' Check object names
#' 
#' @description
#' This function will error if an object name is missing or not provided
#' by its name.
#' It will remain silent otherwise.
#'
#' @param object a `character` of length 1. The name of the object.
#'
#' @noRd

check_object_name <- function(object) {
  

  # Check missing ----
  
  if (missing(object)) {
    stop("The argument '", deparse(substitute(object)), "' is required", 
         call. = FALSE)
  }
  
  
  # Check if it is name ----
  
  if (!is.character(object) || length(object) != 1) {
    stop("The argument '", deparse(substitute(object)), "' must be a ", 
         "character of length 1", call. = FALSE)
  }
  
  invisible(NULL)
}

#' Check 'species_categories' input
#' 
#' @description
#' This function will error if the provided input doesn't contain exactly 2
#' columns and isn't a data.frame
#'
#' @param species_categories 2-columns `data.frame` giving species categories
#'   `NULL` by default, with the first column describing the species name, and
#'   the second column giving their corresponding categories
#'
#' @noRd
check_species_categories <- function(species_categories) {
  
  if (
    !is.null(species_categories) &
    (!is.data.frame(species_categories) | sum(ncol(species_categories)) != 2)
  ) {
    stop("'species_categories' isn't a two-column data.frame", call. = FALSE)
  }
  
}

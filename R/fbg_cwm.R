#' Compute CWM
#'
#' @param site_sp 
#' @param traits 
#'
#' @return
#' @export
#'
#' @examples
fbg_cwm = function(site_sp, traits) {
  # Check input format of site-species matrix
  check_site_sp(site_sp)
  
  # Check input format of species-traits matrix
  check_traits(traits)
  
  # Get species in common between both matrices
  species = list_common_species(colnames(site_sp), rownames(traits))
  
  # Select quantitative traits for CWM
  quanti_traits = apply(traits, 2, is.numeric)
  traits = traits[, quanti_traits, drop = FALSE]
  
  if (identical(as.logical(traits), logical(0))) {
    stop("CWM can only be computed on numeric traits", call. = FALSE)
  }
  
  # Total sites abundances
  total_abund = rowSums(site_sp[, species, drop = FALSE])
  
  # Compute CWM
  cwm = (site_sp[, species, drop = FALSE]/total_abund) %*%
    traits[species,, drop = FALSE]
  
  # Reformat CWM in good way (tidy format)
  data.frame(
    site  = rownames(cwm),
    trait = colnames(cwm),
    cwm   = as.numeric(t(cwm))
  )
}
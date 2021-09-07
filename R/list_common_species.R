list_common_species = function(first_list, second_list) {
  common_species = intersect(first_list, second_list)
  
  if (length(common_species) == 0) {
    stop("No species found in common between inputs", call. = FALSE)
  }
  
  return(common_species)
}
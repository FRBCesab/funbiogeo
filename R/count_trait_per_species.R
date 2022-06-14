count_trait_per_species = function(species_traits_long) {
  
  # Count Number of Traits per Species
  number_trait_per_species = by(
    species_traits_long,
    species_traits_long$species,
    function(x) c(
      n_trait = sum(!is.na(x$trait_value)),
      prop_trait = sum(!is.na(x$trait_value))/nrow(x)
    )
  )
  
  number_trait_per_species = as.data.frame(
    do.call(rbind, number_trait_per_species)
  )
  number_trait_per_species$species = rownames(number_trait_per_species)
  number_trait_per_species = number_trait_per_species[
    order(number_trait_per_species$n_trait, decreasing = TRUE),
  ]
  
  return(number_trait_per_species)
}
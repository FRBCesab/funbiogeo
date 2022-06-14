count_species_per_trait = function(species_traits_long) {
  
  # Count Number of Species per Trait
  number_species_per_trait = by(
    species_traits_long,
    species_traits_long$trait_name,
    function(x) c(
      n_species = sum(!is.na(x$trait_value)),
      prop_species = sum(!is.na(x$trait_value))/nrow(x)
    )
  )
  
  number_species_per_trait = as.data.frame(
    do.call(rbind, number_species_per_trait)
  )
  number_species_per_trait$trait_name = rownames(number_species_per_trait)
  number_species_per_trait = number_species_per_trait[
    order(number_species_per_trait$n_species, decreasing = TRUE),
  ]
  
  # Label for plot
  number_species_per_trait$trait_label = with(
    number_species_per_trait,
    paste0(
      trait_name, "\n(",
      prettyNum(n_species/nrow(species_traits) * 100, digits = 3), "%)"
    )
  )
  
  return(number_species_per_trait)
}
# Script to generate figures for manuscript
# Author: Matthias Grenié
# Packages ---------------------------------------------------------------------
library("ggplot2")
pkgload::load_all()

# Data -------------------------------------------------------------------------
species_traits = data.frame(
  species = paste0("sp", 1:24),
  trait1 = c(NA, 1, NA, rep(1, 21)),
  trait2 = c(rep(1, 3),  NA, rep(1, 4), NA, rep(1, 4), NA, rep(1, 10)),
  trait3 = c(rep(NA, 3), 1, NA, 1, NA, NA, 1, NA, rep(1, 5), NA, rep(1, 8)),
  trait4 = c(rep(NA, 6), 1, 1, NA, 1, 1, NA, rep(1, 12)),
  trait5 = c(rep(NA, 9), rep(1, 4), NA, NA, rep(1, 5), NA, rep(1, 3))
)


# Figure on trait completeness -------------------------------------------------

plot_trait_completeness = fb_plot_species_traits_completeness(
  species_traits, all_traits = FALSE
)

fig_completeness_derived = plot_trait_completeness +
  labs(title = "Trait Diagnostics") +
  theme_bw(28) +
  theme(
    axis.ticks      = element_blank(),
    axis.text       = element_blank(),
    axis.title.x    = element_blank(),
    plot.title      = element_text(hjust = 0.5),
    legend.position = "bottom"
    )

fig_completeness_derived

ggsave("inst/misc/fig_completeness_derived.svg", fig_completeness_derived)


# Figure Map Functional Diversity ----------------------------------------------

selected_traits <- fb_filter_traits_by_species_coverage(
  woodiv_traits,
  threshold_species_proportion = 0.75
)

selected_species <- fb_filter_species_by_trait_coverage(
  selected_traits,
  threshold_traits_proportion = 1
)

filt_sites <- fb_filter_sites_by_trait_coverage(
  woodiv_site_species,
  selected_species,
  threshold_traits_proportion = 0.9
)

subset_traits <- selected_species

# Add row names
rownames(subset_traits) <- subset_traits[["species"]]

# Remove unused 'species' column
subset_traits <- subset_traits[, -1]

# Scale traits
scaled_traits <- mFD::tr.cont.scale(subset_traits)

formatted_site_species <- filt_sites

# Put site names as row names
rownames(formatted_site_species) <- filt_sites[["site"]]
formatted_site_species <- formatted_site_species[, -1]

# Keeping only species for which we have the traits
formatted_site_species <- formatted_site_species[, rownames(scaled_traits)]

formatted_site_species <- as.matrix(formatted_site_species)

woodiv_fdis <- mFD::alpha.fd.multidim(
  scaled_traits,
  formatted_site_species,
  ind_vect = "fdis",
  # remove all of the messages
  details_returned = FALSE,
  verbose = FALSE
)
woodiv_fdis <- woodiv_fdis$functional_diversity_indices

# Create 'site' column
woodiv_fdis[["site"]] <- rownames(woodiv_fdis)

# Remove row names
rownames(woodiv_fdis) <- NULL

# Move 'site' column as first column
woodiv_fdis <- woodiv_fdis[, c(5, 1:4)]

fig_map_diversity = fb_map_site_data(
  woodiv_locations, woodiv_fdis, "fdis", background = TRUE
)
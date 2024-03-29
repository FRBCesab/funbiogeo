% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fb_count_species_by_trait.R
\name{fb_count_species_by_trait}
\alias{fb_count_species_by_trait}
\title{Count Number of Species for Each Trait}
\usage{
fb_count_species_by_trait(species_traits)
}
\arguments{
\item{species_traits}{a \code{data.frame} with species in rows and
traits as columns. \strong{NOTE}: The first column should be named \strong{\code{"species"}}
and contain species names. The other columns should be named according
to trait names.}
}
\value{
A three-column \code{data.frame} with:
\itemize{
\item \code{trait}: the name of the trait;
\item \code{n_species}: the number of species with non-missing value for the trait;
\item \code{coverage}: the percentage of species with non-missing value for the trait.
}
}
\description{
For each trait computes the percentage of species without \code{NA} (missing
trait values).
}
\examples{
library("funbiogeo")

data("species_traits")

species_coverage_by_trait <- fb_count_species_by_trait(species_traits)
head(species_coverage_by_trait)
}

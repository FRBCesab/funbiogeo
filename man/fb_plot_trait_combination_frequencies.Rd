% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fb_plot_trait_combination_frequencies.R
\name{fb_plot_trait_combination_frequencies}
\alias{fb_plot_trait_combination_frequencies}
\title{Plot Present and Missing Trait Combinations Frequencies}
\usage{
fb_plot_trait_combination_frequencies(
  species_traits,
  species_categories = NULL,
  order_by = c("number", "complete")
)
}
\arguments{
\item{species_traits}{a \code{data.frame} with species in rows and
traits as columns. \strong{NOTE}: The first column should be named \strong{\code{"species"}}
and contain species names. The other columns should be named according
to trait names.}

\item{species_categories}{(default = \code{NULL}) 2-columns \code{data.frame} giving
species categories, with the first column describing the species name, and
the second column giving their corresponding categories}

\item{order_by}{{\code{character(1)} either \code{"number"} or \verb{"complete}}\cr{}
If \code{"number"} order rows by frequency so that most
frequent rows are at the bottom.
Otherwise order rows to order table by the number of
non-missing traits then by the frequency of combinations}
}
\value{
a \code{ggplot2} object
}
\description{
Display a figure of Present/Missing Trait Combinations where one square is
represents one trait. The y-axis gives the frequency of the row
(as well as its proportions compared to the total number of rows).
}
\examples{
# Ordered by number by default
fb_plot_trait_combination_frequencies(species_traits)

# Order by present traits
fb_plot_trait_combination_frequencies(species_traits, NULL, "complete")

}

is_ggridges_installed <- function() {
  isTRUE(requireNamespace("ggridges"))
}

#' @importFrom stats weighted.mean
#' @noRd
weighted_mean <- function(x, w, ..., na.rm = FALSE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    w <- w[keep]
    x <- x[keep]
  }
  weighted.mean(x, w, ..., na.rm = FALSE)
}

#' Function to split species traits data.frame into a list based on provided
#' species categories
#'
#' @noRd
split_species_categories <- function(
  species_traits,
  species_categories = NULL
) {
  species_traits_categories <- list(species_traits)

  if (!is.null(species_categories)) {
    species_traits_categories <- merge(
      species_traits,
      species_categories,
      by = "species"
    )

    if (nrow(species_traits_categories) == 0) {
      stop(
        "No species of 'species x traits' object found in the ",
        "'species x categories' object",
        call. = FALSE
      )
    }

    species_traits_categories <- split(
      species_traits_categories[,
        -ncol(species_traits_categories),
        drop = FALSE
      ],
      species_traits_categories[, ncol(species_traits_categories)]
    )
  }

  return(species_traits_categories)
}

#' Function to remove a column in a data.frame (e.g. 'site' or 'species')
#' without its position.
#'
#' @noRd

drop_column <- function(data, col_name, drop = FALSE) {
  data[, -which(colnames(data) == col_name), drop = drop]
}


#' Ask user
#' @noRd

ask_user <- function() {
  readline(
    paste(
      "funbiogeo will create a copy of your datasets in 'path/'. Do you want",
      "to proceed? [Y/n] "
    )
  )
}

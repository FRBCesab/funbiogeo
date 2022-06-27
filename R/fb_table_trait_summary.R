#' Summary Table on Traits (Missingness, Range, etc.)
#' 
#' This function outputs a `data.frame` that summarises the species by trait
#' table to have many information in a glance. This can then return a data.frame
#' or a nicely formatted [knitr::kable()] for inclusion in an Rmarkdown 
#' document.
#'
#' @inheritParams fb_get_trait_coverage_by_site 
#' @param kable `TRUE` or `FALSE` Should function returns a [knitr::kable()]?
#'              defaults `FALSE`
#'
#' @return a `data.frame` with the following columns:
#'   * `trait_name`: a `character` column with the trait name as indicated in
#'   `species_traits`
#'   * `trait_type`: the nature of the trait (`numeric`, `categorical`,
#'   or `ordered`)
#'   * `number_non_missing`: the total number of non-`NA` trait values
#'   * `proportion_non_missing`: the proportion of non-`NA` trait values
#'   * `trait_range`: for numerical traits, the range of values
#'   * `trait_mean_sd`: for numerical traits, the mean plus-minus the standard
#'   deviation
#'   * `number_distinct`: for non-numerical traits, the number of categories
#'   * `list_distinct`: for non-numerical traits, the list of categories
#'   
#' @export
#'
#' @examples
#' fb_table_trait_summary(species_traits)
fb_table_trait_summary <- function(species_traits, kable = FALSE) {
  
  # Checks ---------------------------------------------------------------------
  
  check_species_traits(species_traits)
  
  if (!is.logical(kable)) {
    stop("Argument 'kable' should be a logical")
  }
  
  
  # Compute different summaries ------------------------------------------------
  
  # Type
  trait_type <- lapply(
    species_traits[, -1, drop = FALSE], function(x) class(x)[1]
  )
  
  trait_type <- vapply(
    trait_type, switch, character(1),
    character = "categorical",
    numeric   = "numeric",
    integer   = "numeric",
    factor    = "categorical",
    ordered   = "ordered",
  )
  
  # Number Non-missing
  non_missing_number <- vapply(
    species_traits[, -1, drop = FALSE], function(x) {
      sum(!is.na(x))
    },
    numeric(1)
  )
  
  # Proportion Non-missing
  prop_non_missing <- paste0(
    round(non_missing_number/nrow(species_traits), 2) * 100, " %"
  )
  
  # (Numerical traits) range
  trait_range <- vapply(
    names(trait_type),
    function(trait_name) {
      given_type <- trait_type[[trait_name]]
      
      ifelse(
        given_type == "numeric",
        paste0(
          round(range(species_traits[[trait_name]], na.rm = TRUE), 2),
          collapse = "-"
        ),
        NA_character_
      )
    },
    character(1)
  )
  
  # (Numerical traits) mean ± sd
  trait_mean <- vapply(
    names(trait_type),
    function(trait_name) {
      given_type <- trait_type[[trait_name]]
      
      trait_mean <- ifelse(
        given_type == "numeric",
        mean(species_traits[[trait_name]], na.rm = TRUE),
        NA_real_
      )
      
      trait_sd <- ifelse(
        given_type == "numeric",
        stats::sd(species_traits[[trait_name]], na.rm = TRUE),
        NA_real_
      )
      
      ifelse(
        !is.na(trait_mean),
        paste0(round(trait_mean, 2), "\u0bB1", round(trait_sd, 2)),
        NA_character_
      )
    },
    character(1)
  )
  
  # (Non-numerical traits) number of distinct values
  number_distinct <- vapply(
    names(trait_type),
    function(trait_name) {
      given_type <- trait_type[[trait_name]]
      
      ifelse(
        given_type != "numeric",
        ifelse(
          given_type == "categorical",
          length(stats::na.omit(unique(species_traits[[trait_name]]))),
          length(stats::na.omit(levels(species_traits[[trait_name]])))
        ),
        NA_integer_
      )
    },
    integer(1)
  )
  
  # (Non-numerical traits) list of distinct values
  list_distinct <- vapply(
    names(trait_type),
    function(trait_name) {
      given_type <- trait_type[[trait_name]]
      
      ifelse(
        given_type != "numeric",
        ifelse(
          given_type == "categorical",
          paste0(
            stats::na.omit(unique(species_traits[[trait_name]])),
            collapse = ", "
          ),
          paste0(
            stats::na.omit(levels(species_traits[[trait_name]])),
            collapse = ", "
          )
        ),
        NA_character_
      )
    },
    character(1)
  )
  
  
  # Assemble Summary Table -----------------------------------------------------
  
  trait_summary_table <- data.frame(
    trait_name             = colnames(species_traits[, -1, drop = FALSE]),
    trait_type             = trait_type,
    number_non_missing     = non_missing_number,
    proportion_non_missing = prop_non_missing,
    trait_range            = trait_range,
    trait_mean_sd          = trait_mean,
    number_distinct        = number_distinct,
    list_distinct          = list_distinct
  )
  
  rownames(trait_summary_table) <- NULL
  
  # Extra-check in case of logical(0)
  if (isTRUE(kable)) {
    trait_summary_table <- knitr::kable(
      trait_summary_table,
      col.names = c(
        "Trait Name", "Nature of Trait", "Number of Non-Missing Values",
        "Proportion of Non-Missing Values", "Range of Trait",
        "Trait Mean \u0bB1 SD", "Number of Distinct Values",
        "List of Distinct Values"
      )
    )
  }
  
  trait_summary_table
}
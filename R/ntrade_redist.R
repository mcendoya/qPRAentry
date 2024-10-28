utils::globalVariables(c(
  "TIME_PERIOD", "geo", "values", "NUTS0",
  "values_redistribution", "proportion",
  "Ntrade_proportional", ":="
))
#' Ntrade redistribution
#'
#' Redistribution of the quantity of potentially infested commodities (\eqn{N_{trade}})
#' of each NUTS0 (country) among smaller territorial units (NUTS1, NUTS2 or NUTS3).
#'
#' NUTS stands for nomenclature of territorial units for statistics.
#' \eqn{N_{trade}} can be generated using the \code{\link{ntrade}} function.
#'
#' @param ntrade_data A data frame with the quantity of potentially infested commodities
#' imported from third countries where the pest is present. It can be calculated
#' using \code{\link{ntrade}} function.
#' @param ntrade_nuts_col Column name in \code{ntrade_data} with the NUTS0 codes.
#' @param ntrade_values_col Column name, or vector with the name of multiple columns,
#' in \code{ntrade_data} with the \eqn{N_{trade}} values
#' (quantity of potentially infested commodity imports) to be redistributed.
#' @param to_nuts Numeric. NUTS level for redistribution (1, 2 or 3).
#' @param redist_data Data frame to proportionally distribute \eqn{N_{trade}}.
#' By default NULL: redistribution based on population
#' (Eurostat data - <https://ec.europa.eu/eurostat/databrowser/product/page/demo_r_pjangrp3>).
#' @param redist_nuts_col Column name in \code{redist_data} with the destination
#' NUTS codes in the redistribution. Same NUTS level as set in \code{to_nuts}.
#' @param redist_values_col Column name in \code{redist_data} with the with the values
#' according to which to proportionally redistribute \eqn{N_{trade}}.
#' @param population_year Year of population data. By default 2023.
#' Available years can be found at
#' <https://ec.europa.eu/eurostat/databrowser/product/page/demo_r_pjangrp3>.
#' Multiple years can be entered by a numeric vector, the average population of
#' the different years will be used.
#'
#' @return A data frame with the quantity of commodity redistributed.
#' @export
#'
#' @examples
#' \dontrun{
#' Nt_r <- ntrade_redist(
#'   ntrade_data = Nt_df,
#'   ntrade_nuts_col = "country_IDs",
#'   ntrade_values_col = "mean",
#'   to_nuts = 2
#' )
#' }
ntrade_redist <- function(ntrade_data, ntrade_nuts_col, ntrade_values_col, 
                          to_nuts = 2, redist_data = "population", 
                          redist_nuts_col = NULL, redist_values_col = NULL,
                          population_year = 2023) {
  # check data.frame
  if (!is.data.frame(ntrade_data)) {
    stop("Error: 'ntrade_data' must be data.frame.")
  }
  # check to_nuts
  if (!to_nuts %in% c(1, 2, 3) || !is.numeric(to_nuts)) {
    stop("Error: 'to nuts' must be numeric, 1, 2 or 3 NUTS level for redistribution.")
  }
  # check value numeric
  if (!all(sapply(ntrade_data[, ntrade_values_col], is.numeric))) {
    stop("Error: 'ntrade_values_col' in 'ntrade_data' must be numeric.")
  }
  # check value not negative
  if (any(sapply(ntrade_data[, ntrade_values_col], function(x) x[!is.na(x)] < 0))) {
    stop("Error: Invalid 'value' detected. Negative values 'ntrade_values_col' in 'ntrade_data' not interpretable as quantities.")
  }

  if ("GR" %in% unique(ntrade_data[[ntrade_nuts_col]])) {
    ntrade_data[[ntrade_nuts_col]][ntrade_data[[ntrade_nuts_col]] == "GR"] <- "EL"
  }
  if ("GB" %in% unique(ntrade_data[[ntrade_nuts_col]])) {
    ntrade_data[[ntrade_nuts_col]][ntrade_data[[ntrade_nuts_col]] == "GB"] <- "UK"
  }
  # check country codes
  if (!all(ntrade_data[[ntrade_nuts_col]] %in% NUTS_CODES$CNTR_CODE)) {
    stop("Error: 'ntrade_nuts_col' in 'ntrade_data' does not contain NUTS Country codes (2-letter code country level).")
  }

  # Check redist_data
  if(is.vector(redist_data)){
    if(redist_data!="population"){
      stop("Error: 'redist_data' must be 'population' (default option) or a dataframe")
    }else{
      redist_data <- NULL
    }
  }
  if (!is.null(redist_data)) {
    # check data.frame
    if (!is.data.frame(redist_data)) {
      stop("Error: 'redist_data' must be data.frame.")
    }
    # check value numeric
    if (!all(sapply(redist_data[, redist_values_col], is.numeric))) {
      stop("Error: 'redist_values_col' in 'redist_data' must be numeric.")
    }
    # check value not negative
    if (any(sapply(redist_data[, redist_values_col], function(x) x[!is.na(x)] < 0))) {
      stop("Error: Invalid 'value' detected. Negative values 'redist_values_col' in 'redist_data'.")
    }
    # check nuts2 codes
    if (!all(redist_data[[redist_nuts_col]] %in% NUTS_CODES$NUTS2_CODE)) {
      stop("Error: 'redist_nuts_col' in 'redist_data' does not contain NUTS2 codes.")
    }
  }
  
  if (is.null(redist_data)) {
    redist_df <- cached_get_eurostat_data(to_nuts) %>%
      filter(TIME_PERIOD %in% population_year)
    if (length(unique(redist_df$TIME_PERIOD)) > 1) {
      redist_df <- redist_df %>%
        group_by(geo) %>%
        summarise(values_redistribution = mean(values, na.rm = TRUE))
    } else {
      redist_df <- redist_df %>%
        select(geo, values) %>%
        rename(values_redistribution = values)
    }
  } else {
    new_cols <- c(geo = redist_nuts_col, values_redistribution = redist_values_col)
    redist_df <- redist_data %>%
      rename(all_of(new_cols)) %>%
      select(geo, values_redistribution)
  }

  redist_df <- redist_df %>%
    mutate(NUTS0 = substr(geo, 1, 2)) %>%
    filter(NUTS0 %in% unique(ntrade_data[[ntrade_nuts_col]])) %>%
    group_by(NUTS0) %>%
    mutate(proportion = values_redistribution / sum(values_redistribution)) %>% # Proportion per NUTS0
    ungroup(NUTS0)

  df <- redist_df %>%
    left_join(select(ntrade_data, !!ntrade_nuts_col, !!ntrade_values_col),
      by = c("NUTS0" = ntrade_nuts_col)
    ) %>%
    mutate(across(all_of(ntrade_values_col),
      .fns = list(redist = ~ . * proportion)
    )) %>%
    rename(!!paste0("NUTS", to_nuts) := geo) %>%
    select(
      !!paste0("NUTS", to_nuts),
      NUTS0,
      proportion,
      ends_with("redist")
    ) %>%
    rename_with(~ sub("_redist$", "", .), ends_with("redist"))

  return(df)
}

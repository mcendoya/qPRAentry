utils::globalVariables(c(
  "TIME_PERIOD", "geo", "values", "NUTS0",
  "values_redistribution", "proportion",
  "Ntrade_proportional", ":="
))
#' Ntrade redistribution
#'
#' Redistribution of the quantity of potentially infested commodities (\eqn{N_{trade}})
#' from country-level (NUTS0) data to smaller territores (NUTS1, NUTS2 or NUTS3).
#' See \link[https://ec.europa.eu/eurostat/web/nuts]{NUTS - Nomenclature of territorial 
#' units for statistics}.
#'
#' This function enables redistribution of trade data from national-level NUTS0 
#' to smaller territorial units (NUTS1, NUTS2, or NUTS3), either proportionally 
#' based on population data from Eurostat or using user-supplied redistribution proportions. 
#' Population data for redistribution is automatically fetched for the specified time 
#' period from the Eurostat database.
#'
#' @param ntrade_data A data frame containing the quantity of potentially infested 
#' commodities imported from third countries where the pest is present. This data can 
#' be generated using \code{\link{ntrade}} function.
#' @param ntrade_nuts_col A string specifying the column name in \code{ntrade_data} 
#' with the NUTS0 codes.
#' @param ntrade_values_col A string or vector specifying the column name(s) in 
#' \code{ntrade_data} with the \eqn{N_{trade}} values (quantity of potentially 
#' infested commodity imports) to be redistributed.
#' @param to_nuts A numeric value (1, 2, or 3) specifying the NUTS level for redistribution.
#' Default 2, indicating redistribution to NUTS2.
#' @param redist_data A data frame to provide custom proportions for redistributing 
#' \eqn{N_{trade}}. Default \code{"population"}, indicating redistribution based on 
#' \link[https://ec.europa.eu/eurostat/databrowser/product/page/demo_r_pjangrp3]{population 
#' data from Eurostat}.
#' @param redist_nuts_col A string specifying the column name in \code{redist_data} 
#' that contains the destination NUTS codes. The NUTS level should correspond to the 
#' value specified in  \code{to_nuts}. \code{NULL} (default) if a data frame is not 
#' incorporated in \code{redist_data}.
#' @param redist_values_col A string specifying the column name in \code{redist_data} 
#' with the values for proportional redistribution. This will define the weights 
#' used for redistributing \eqn{N_{trade}}.
#' \code{NULL} (default) if a data frame is not incorporated in \code{redist_data}.
#' @param population_year A numeric value specifying the year of population data 
#' to use in the redistribution. only necessary if \code{"population"} is specified in 
#' \code{redist_data} (default is 2023). If multiple years are provided, the average 
#' population across those years will be used. Available years can be found at
#' \link[https://ec.europa.eu/eurostat/databrowser/product/page/demo_r_pjangrp3]{population 
#' data from Eurostat population data}.
#'
#' @return A data frame with the redistributed quantity of potentially infested 
#' commodities across the specified NUTS level.
#' 
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

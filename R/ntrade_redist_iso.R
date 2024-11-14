utils::globalVariables(c(
  "TIME_PERIOD", "geo", "values",
  "values_redistribution", "proportion",
  "Ntrade_proportional", ":="
))
#' Ntrade redistribution ISO 3166 Codes
#'
#' Redistribution of the quantity of potentially infested commodities (\eqn{N_{trade}})
#' from country-level (ISO 3166-1) data to principal subdivisions (ISO 3166-2).
#' See \link[https://www.iso.org/iso-3166-country-codes.html]{ISO 3166 Maintenance Agency}.
#'
#' This function enables redistribution of trade data from national-level  
#' to smaller territorial units (NUTS1, NUTS2, or NUTS3), proportionally 
#' to user-supplied redistribution proportions.
#'
#' @param ntrade_data A data frame containing the quantity of potentially infested 
#' commodities imported from third countries where the pest is present. This data can 
#' be generated using \code{\link{ntrade}} function.
#' @param ntrade_iso_col A string specifying the column name in \code{ntrade_data} 
#' with the ISO 3166-1 codes.
#' @param ntrade_values_col A string or vector specifying the column name(s) in 
#' \code{ntrade_data} with the \eqn{N_{trade}} values (quantity of potentially 
#' infested commodity imports) to be redistributed.
#' @param redist_data A data frame to provide custom proportions for redistributing 
#' \eqn{N_{trade}}. 
#' @param redist_iso_col A string specifying the column name in \code{redist_data} 
#' that contains the destination ISO 3166-2 codes.
#' @param redist_values_col A string specifying the column name in \code{redist_data} 
#' with the values for proportional redistribution. This will define the weights 
#' used for redistributing \eqn{N_{trade}}.
#'
#' @return A data frame with the redistributed quantity of potentially infested 
#' commodities across the specified subnational level.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' Nt_r <- ntrade_redist(
#'   ntrade_data = Nt_df,
#'   ntrade_iso_col = "country_IDs",
#'   ntrade_values_col = "mean",
#'   redist_data = df_population,
#'   redist_iso_col = "ISO_3166_2",
#'   redist_values_col = "population"
#' )
#' }
ntrade_redist_iso <- function(ntrade_data, ntrade_iso_col, ntrade_values_col, 
                              redist_data, redist_iso_col, redist_values_col) {
  # check data.frame
  if (!is.data.frame(ntrade_data)) {
    stop("Error: 'ntrade_data' must be data.frame.")
  }
  # check value numeric
  if (!all(sapply(ntrade_data[, ntrade_values_col], is.numeric))) {
    stop("Error: 'ntrade_values_col' in 'ntrade_data' must be numeric.")
  }
  # check value not negative
  if (any(sapply(ntrade_data[, ntrade_values_col], function(x) x[!is.na(x)] < 0))) {
    stop("Error: Invalid 'value' detected. Negative values 'ntrade_values_col' in 'ntrade_data' not interpretable as quantities.")
  }
  
  # # check country codes
  # if (!all(ntrade_data[[ntrade_nuts_col]] %in% NUTS_CODES$CNTR_CODE)) {
  #   stop("Error: 'ntrade_nuts_col' in 'ntrade_data' does not contain NUTS Country codes (2-letter code country level).")
  # }
  
  # Check redist_data
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
    # # check nuts2 codes
    # if (!all(redist_data[[redist_nuts_col]] %in% NUTS_CODES$NUTS2_CODE)) {
    #   stop("Error: 'redist_nuts_col' in 'redist_data' does not contain NUTS2 codes.")
    # }
  }else{
    stop("Error: 'redist_data' must be a dataframe")
  }
  
  new_cols <- c(ISO_code = redist_iso_col, values_redistribution = redist_values_col)
  redist_df <- redist_data %>%
    rename(all_of(new_cols)) %>%
    select(ISO_code, values_redistribution)
  
  
  redist_df <- redist_df %>%
    mutate(ISO_1 = substr(ISO_code, 1, 2)) %>%
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

utils::globalVariables(c("TIME_PERIOD", "geo", "values", "NUTS0", 
                         "values_redistribution", "proportion",
                         "Ntrade_proportional", ":="))
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
#' @param nuts_column Column name in \code{ntrade_data} with the NUTS0 codes.
#' @param values_column Column name in \code{ntrade_data} with the \eqn{N_{trade}} values
#' (quantity of potentially infested commodity imports) to be redistributed.
#' @param to_nuts Numeric. NUTS level for redistribution (1, 2 or 3).
#' @param prop_data Data frame to proportionally distribute \eqn{N_{trade}}.
#' By default NULL: redistribution based on population
#' (Eurostat data - <https://ec.europa.eu/eurostat/databrowser/product/page/demo_r_pjangrp3>).
#' @param prop_nuts_column Column name in \code{prop_data} with the destination
#' NUTS codes in the redistribution. Same NUTS level as set in \code{to_nuts}.
#' @param prop_values_column Column name in \code{prop_data} with the with the values
#' according to which to proportionally redistribute \eqn{N_{trade}}.
#' @param time_period Year of population data. By default 2023.
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
#' ntrade_data = Nt_df,
#' nuts_column = "IDs",
#' values_column = "mean",
#' to_nuts = 2
#' )
#' }
ntrade_redist <- function(ntrade_data, nuts_column, values_column,
                          to_nuts = 2,
                          prop_data = NULL,
                          prop_nuts_column = NULL,
                          prop_values_column = NULL,
                          time_period = 2023) {
  # check data.frame
  if(!is.data.frame(ntrade_data)){
    stop("Error: 'ntrade_data' must be data.frame.")
  }
  #check to_nuts
  if(!to_nuts%in%c(1,2,3)||!is.numeric(to_nuts)){
    stop("Error: 'to nuts' must be numeric, 1, 2 or 3 NUTS level for redistribution.")
  }
  #check value numeric
  if(!all(sapply(ntrade_data[,values_column], is.numeric))){
    stop("Error: 'values_column' in 'ntrade_data' must be numeric.")
  }
  # check value not negative
  if(any(sapply(ntrade_data[,values_column], function(x) x[!is.na(x)]<0))){
    stop("Error: Invalid 'value' detected. Negative values 'values_column' in 'ntrade_data' not interpretable as quantities.")
  }
  
  if ("GR" %in% unique(ntrade_data[[nuts_column]])) {
    ntrade_data[[nuts_column]][ntrade_data[[nuts_column]] == "GR"] <- "EL"
  }
  if ("GB" %in% unique(ntrade_data[[nuts_column]])) {
    ntrade_data[[nuts_column]][ntrade_data[[nuts_column]] == "GB"] <- "UK"
  }
  #check country codes
  if(!all(ntrade_data[[nuts_column]] %in% NUTS_CODES$CNTR_CODE)){
    stop("Error: 'nuts_column' in 'ntrade_data' does not contain NUTS Country codes (2-letter code country level).")
  }
  
  if(!is.null(prop_data)){
    # check data.frame
    if(!is.data.frame(prop_data)){
      stop("Error: 'prop_data' must be data.frame.")
    }
    #check value numeric
    if(!is.numeric(prop_data[[prop_values_column]])){
      stop("Error: 'prop_values_column' in 'prop_data' must be numeric.")
    }
    # check value not negative
    if(any(prop_data[[prop_values_column]][!is.na(prop_data[[prop_values_column]])]<0)){
      stop("Error: Invalid 'value' detected. Negative values 'prop_values_column' in 'prop_data'.")
    }
    # check nuts2 codes
    if(!all(prop_data[[prop_nuts_column]] %in% NUTS_CODES$NUTS2_CODE)){
      stop("Error: 'prop_nuts_column' in 'prop_data' does not contain NUTS2 codes.")
    }
  }


  n0 <- unique(ntrade_data[[nuts_column]])
  
  if (is.null(prop_data)) {
    prop_df <- cached_get_eurostat_data(to_nuts) %>%
      filter(TIME_PERIOD %in% time_period)
    if(length(unique(prop_df$TIME_PERIOD))>1){
      prop_df <- prop_df %>%
        group_by(geo) %>%
        summarise(values_redistribution = mean(values, na.rm=TRUE))
    }else{
      prop_df <- prop_df %>%
        select(geo, values) %>%
        rename(values_redistribution = values)
    }
  }else{
    new_cols <- c(geo = prop_nuts_column, values_redistribution = prop_values_column)
    prop_df <- prop_data %>%
      rename(all_of(new_cols)) %>% 
      select(geo, values_redistribution)
  }

  prop_df <- prop_df %>%
    mutate(NUTS0 = substr(geo, 1, 2)) %>%
    filter(NUTS0 %in% n0) %>%
    group_by(NUTS0) %>%
    mutate(proportion = values_redistribution / sum(values_redistribution)) %>% # Proportion per NUTS0
    ungroup(NUTS0)

  df <- prop_df %>%
    left_join(select(ntrade_data, !!nuts_column, !!values_column),
              by = c("NUTS0"=nuts_column)) %>%
    mutate(across(all_of(values_column),
                  .fns = list(NUTS2 = ~. * proportion),
                  .names = !!paste0("{col}_NUTS", to_nuts))) %>%
    rename(!!paste0("NUTS", to_nuts) := geo) %>%
    select(!!paste0("NUTS", to_nuts), NUTS0,
           ends_with(paste0("_NUTS", to_nuts)))
  
  return(df)
}

utils::globalVariables(c(
  "TIME_PERIOD", "geo", "values", "NUTS0",
  "values_redistribution", "proportion",
  ":="
))
#' Data redistribution to NUTS subdivisions
#'
#' Redistribution of country-level (NUTS0) data value to smaller territories 
#' (i.e., NUTS1, NUTS2 or NUTS3). See 
#' [NUTS - Nomenclature of territorial units for statistics](https://ec.europa.eu/eurostat/web/nuts).
#' 
#' @details 
#' This function enables redistribution of values from national-level NUTS0 
#' to smaller territorial units (i.e., NUTS1, NUTS2, or NUTS3), either proportionally 
#' based on 
#' [population data from Eurostat](https://ec.europa.eu/eurostat/databrowser/product/page/demo_r_pjangrp3) 
#' or using user-supplied redistribution proportions. 
#' Population data for redistribution is automatically fetched for the specified time 
#' period from the Eurostat database.
#' 
#' Note that more than one column of values provided in the dataframe \code{data} 
#' can be redistributed at the same time. The values in columns \code{values_col} 
#' and \code{redist_values_col} must be numeric and positive.
#' 
#' ### Common uses
#' In the context of quantitative pest risk assessment (qPRA) at the entry step, 
#' this function can be applied to redistribute the quantity of potentially infested 
#' commodities (\eqn{N_{trade}}, see [ntrade()]) or the number of potential 
#' founder populations (\eqn{NPFP}, see [pathway_model()]). For this purpose, 
#' population or consumption data from subdivisions are often used for redistribution.
#'
#' @param data A data frame containing the data at the country-level to 
#' redistribute.
#' @param nuts_col A string specifying the column name in \code{data} 
#' with the NUTS0 codes.
#' @param values_col A string or vector specifying the column name(s) in \code{data} 
#' with the values to be redistributed.
#' @param to_nuts A numeric value (1, 2, or 3) specifying the NUTS level for redistribution.
#' Default 2, indicating redistribution to NUTS2.
#' @param redist_data A data frame containing values for each subdivision that will be 
#' used as the basis for proportional redistribution. By default, this is set to 
#' \code{"population"}, indicating redistribution based on 
#' [population data from Eurostat](https://ec.europa.eu/eurostat/databrowser/product/page/demo_r_pjangrp3).
#' @param redist_nuts_col A string specifying the column name in \code{redist_data} 
#' that contains the destination NUTS codes. The NUTS level should correspond to the 
#' value specified in  \code{to_nuts}. \code{NULL} (default) if a data frame is not 
#' incorporated in \code{redist_data} (i.e., \code{redist_data = "population"}).
#' @param redist_values_col A string specifying the column name in \code{redist_data} 
#' with the values for proportional redistribution. These values will serve as the 
#' weights for the redistribution process. \code{NULL} (default) if a data frame 
#' is not incorporated in \code{redist_data} (i.e., \code{redist_data = "population"}).
#' @param population_year A numeric value specifying the year(s) of the human population 
#' data to be used for redistribution. Only necessary if \code{"population"} is specified in 
#' \code{redist_data} (default is 2023). If multiple years are provided, the average 
#' population across those years will be used. Available years can be found at
#' [population data from Eurostat](https://ec.europa.eu/eurostat/databrowser/product/page/demo_r_pjangrp3).
#' @param nuts_year Year of NUTS classification. Accepted values are '2003','2006','2010','2013',
#' '2016' (default),'2021', or '2024'. See [NUTS - History](https://ec.europa.eu/eurostat/web/nuts/history).
#'
#' @return A data frame with the redistributed values across the specified NUTS 
#' level. The dataframe contains the columns \code{NUTSX} with the codes at the 
#' selected NUTS level, \code{NUTS0} with the codes at country level, \code{proportion} with the 
#' proportion according to which the values have been redistributed, and the columns 
#' corresponding to the redistributed values with the same name specified in \code{values_col}.
#' 
#' @seealso [ntrade()], [pathway_model()]
#' 
#' @export
#'
#' @examples
#' ## Example of data redistribution to NUTS2 using population data
#' data("datatrade_EU")
#' # extract NUTS0 codes (country level)
#' nuts0 <- unique(datatrade_EU$internal_production$reporter)
#' # simulate values for each country
#' nuts0_data <- data.frame(nuts0 = nuts0,
#'                          value = abs(rnorm(length(nuts0), 30000, 10000)))
#' 
#' # Redistribution
#' data_redist <- redist_nuts(data = nuts0_data,
#'                            nuts_col = "nuts0",
#'                            values_col = "value",
#'                            to_nuts = 2,
#'                            redist_data = "population",
#'                            population_year = c(2017, 2018, 2019))
#' head(data_redist)
#' 
#' # Plot
#' plot_nuts(data = data_redist,
#'           nuts_level = 2,
#'           nuts_col = "NUTS2", 
#'           values_col = "value")
#' 
#' ## Example of data redistribution to NUTS1 using custom data
#' # consumption data at NUTS1 level
#' nuts1_data <- datatrade_EU$consumption_nuts1
#' 
#' # Redistribution
#' data_redist <- redist_nuts(data = nuts0_data,
#'                            nuts_col = "nuts0",
#'                            values_col = "value",
#'                            to_nuts = 1,
#'                            redist_data = nuts1_data,
#'                            redist_nuts_col = "NUTS_ID",
#'                            redist_values_col = "value")
#' 
#' head(data_redist)
#' 
#' # Plot
#' plot_nuts(data = data_redist,
#'           nuts_level = 1,
#'           nuts_col = "NUTS1", 
#'           values_col = "value")
#' 
redist_nuts <- function(data, nuts_col, values_col, 
                        to_nuts = 2, redist_data = "population", 
                        redist_nuts_col = NULL, redist_values_col = NULL,
                        population_year = 2023, nuts_year = 2016) {
  # check sf class and remove geometry
  if(any(class(data) == "sf")){
    data <- data %>% st_drop_geometry()
  }
  if(any(class(redist_data) == "sf")){
    redist_data <- redist_data %>% st_drop_geometry()
  }
  # check data.frame
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be data.frame.")
  }
  
  # Check if the specified columns exist in the dataframe
  if (!all(c(nuts_col, values_col) %in% names(data))) {
    stop(paste(strwrap("Error: The dataframe 'data' must contain the columns 
                       specified in 'nuts_col' and 'values_col'."), collapse=" "))
  }
  
  # check nuts year
  if (!nuts_year %in% c('2003','2006','2010','2013','2016','2021','2024')) {
    stop(paste(strwrap("Error: nuts_year not available. Try '2003', '2006', '2010', 
                       '2013', '2016', '2021', or '2024'"), collapse=" "))
  }
  
  # check to_nuts
  if (!to_nuts %in% c(1, 2, 3) || !is.numeric(to_nuts)) {
    stop("Error: 'to nuts' must be numeric, 1, 2 or 3 NUTS level for redistribution.")
  }
  NUTS_CODES <- cached_get_EUmap(year = nuts_year, nuts = to_nuts) %>%
    st_drop_geometry()
  
  # check value numeric
  if (!all(sapply(data[, values_col], is.numeric))) {
    stop("Error: 'values_col' in 'data' must be numeric.")
  }
  # check value not negative
  if (any(sapply(data[, values_col], function(x) x[!is.na(x)] < 0))) {
    stop(paste(strwrap("Error: Invalid 'value' detected. Negative values 'values_col' 
                       in 'data' not interpretable as quantities."), collapse =" "))
  }
  
  if ("GR" %in% unique(data[[nuts_col]])) {
    data[[nuts_col]][data[[nuts_col]] == "GR"] <- "EL"
  }
  if ("GB" %in% unique(data[[nuts_col]])) {
    data[[nuts_col]][data[[nuts_col]] == "GB"] <- "UK"
  }
  # check country codes
  if (!any(data[[nuts_col]] %in% NUTS_CODES$CNTR_CODE)) {
    stop(paste(strwrap("Error: 'nuts_col' in 'data' does not contain NUTS0 Country 
                       codes (2-letter code country level)."), collapse=" "))
  }else if (!all(data[[nuts_col]] %in% NUTS_CODES$CNTR_CODE)){
    data_codes_missing <- data[[nuts_col]][!data[[nuts_col]] %in% NUTS_CODES$CNTR_CODE]
    warning(paste(
      "The following NUTS0 codes are invalid or not available: ",
      paste(data_codes_missing, collapse = ", "), 
      paste(strwrap("Please, check column 'nuts_col' in 'data' or the NUTS 
                    classification year argument 'nuts_year'"), collapse=" "),
      sep = "\n"))
  }
  
  # Check redist_data
  if(!is.data.frame(redist_data) && redist_data != "population"){
    stop(paste(strwrap("Error: 'redist_data' must be 'population' (default option) 
                       or a data.frame."), collapse = " "))
  }
  
  if(is.data.frame(redist_data)){
    if (!all(c(redist_nuts_col, redist_values_col) %in% names(redist_data))) {
      stop(paste(strwrap("The dataframe 'redist_data' must contain the columns specified 
                         in 'redist_nuts_col' and 'redist_values_col'"), 
                 collapse=" "))
    }
    # check value numeric
    if (!all(sapply(redist_data[, redist_values_col], is.numeric))) {
      stop("Error: 'redist_values_col' in 'redist_data' must be numeric.")
    }
    # check value not negative
    if (any(sapply(redist_data[, redist_values_col], function(x) x[!is.na(x)] < 0))) {
      stop(paste(strwrap("Error: Invalid 'value' detected. Negative values 
                         'redist_values_col' in 'redist_data'."), collapse=" "))
    }
    # check nuts codes
    if (!any(redist_data[[redist_nuts_col]] %in% NUTS_CODES$NUTS_ID)) {
      stop("Error: 'redist_nuts_col' in 'redist_data' does not contain NUTS codes.")
    }else if (!all(redist_data[[redist_nuts_col]] %in% NUTS_CODES$NUTS_ID)){
      redist_codes_missing <- 
        redist_data[[redist_nuts_col]][!redist_data[[redist_nuts_col]] %in% NUTS_CODES$NUTS_ID]
      warning(paste(
        "The following NUTS codes are invalid or not available: ",
        paste(redist_codes_missing, collapse = ", "), 
        paste(strwrap("Please, check column 'redist_nuts_col' in 'redist_data' 
                      or the NUTS classification year argument 'nuts_year'"), 
              collapse=" "),
        sep = "\n"))
    }
  }
  
  if (!is.data.frame(redist_data) && redist_data=="population") {
    redist_df <- cached_get_eurostat_data(nuts_level=to_nuts, 
                                          nuts_filter = NUTS_CODES$NUTS_ID)
    # check population_year
    if(!all(population_year%in%unique(redist_df$TIME_PERIOD))){
      stop(paste(
        paste(
          strwrap("Error: The years specified in population_year are not available.
            Available years for population data are: "), collapse=" "),
        paste(unique(redist_df$TIME_PERIOD), collapse=", "), collapse=" ")
      )
    }
    redist_df <- redist_df %>% filter(TIME_PERIOD %in% population_year)
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
    filter(NUTS0 %in% unique(data[[nuts_col]])) %>%
    group_by(NUTS0) %>%
    mutate(proportion = values_redistribution / sum(values_redistribution)) %>%
    ungroup(NUTS0)
  
  df <- redist_df %>%
    left_join(select(data, !!nuts_col, !!values_col),
              by = c("NUTS0" = nuts_col)
    ) %>%
    mutate(across(all_of(values_col),
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

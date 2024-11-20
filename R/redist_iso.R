utils::globalVariables(c(
  "TIME_PERIOD", "geo", "values",
  "values_redistribution", "proportion",
  ":=", "ISO_2", "ISO_1"
))
#' Data redistribution to country subdivisions
#'
#' Value redistribution from country-level (ISO 3166-1 alpha-2 codes) to principal 
#' subdivisions (ISO 3166-2 codes). See 
#' [ISO 3166 Maintenance Agency](https://www.iso.org/iso-3166-country-codes.html).
#'
#' @details
#' This function enables redistribution of values from country-level  
#' to principal subdivisions (e.g., provinces or states), proportionally 
#' to user-supplied redistribution proportions. 
#' 
#' Note that more than one column of values provided in the dataframe data can be 
#' redistributed at the same time. The values in columns \code{values_col} and 
#' \code{redist_values_col} must be numeric and positive.
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
#' @param iso_col A string specifying the column name in \code{data} 
#' with the ISO 3166-1 (alpha-2) country codes.
#' @param values_col A string or vector specifying the column name(s) in 
#' \code{data} with the values to be redistributed.
#' @param redist_data A data frame with values for each subdivision on which the 
#' redistribution is to be performed.
#' @param redist_iso_col A string specifying the column name in \code{redist_data} 
#' that contains the destination ISO 3166-2 codes.
#' @param redist_values_col A string specifying the column name in \code{redist_data} 
#' with the values for proportional redistribution. This will define the weights 
#' used for the redistribution.
#'
#' @return A data frame with the redistributed values across the specified subnational 
#' level. The dataframe contains the columns \code{ISO_1} with the codes at country 
#' level, \code{ISO_2} with the codes at subdivision level, \code{proportion} with the 
#' proportion according to which the values have been redistributed, and the columns 
#' corresponding to the redistributed values with the same name specified in \code{values_col}.
#' 
#' @seealso [ntrade()], [pathway_model()]
#' 
#' @export
#'
#' @examples
#' ## Example of data redistribution in Northern American countries
#' data(datatrade_NorthAm)
#' # Selection of internal production data from January to March to be proportionally 
#' # redistributed based on sub-national consumption data
#' data_ip <- datatrade_NorthAm$internal_production
#' data_ip <- data_ip[data_ip$time_period=="January-March",]
#' # consumption data at sub-national level
#' data_sub <- datatrade_NorthAm$consumption_iso2
#' 
#' # Redistribution
#' data_redist <- redist_iso(data = data_ip,
#'                           iso_col = "reporter",
#'                           values_col = "value",
#'                           redist_data = data_sub,
#'                           redist_iso_col = "iso_3166_2",
#'                           redist_values_col = "value")
#' 
#' head(data_redist)
#'
redist_iso <- function(data, iso_col, values_col, 
                       redist_data, redist_iso_col, redist_values_col) {
  # check sf class and remove geometry
  if(any(class(data) == "sf")){
    data <- data %>% st_drop_geometry()
  }
  if(any(class(redist_data) == "sf")){
    redist_data <- redist_data %>% st_drop_geometry()
  }
  # Check if the specified columns exist in the dataframe
  if (!all(c(iso_col, values_col) %in% names(data))) {
    stop(paste(strwrap("The dataframe 'data' must contain the columns specified in 
                       'iso_col' and 'values_col'"), collapse=" "))
  }
  if (!all(c(redist_iso_col, redist_values_col) %in% names(redist_data))) {
    stop(paste(strwrap("The dataframe 'redist_data' must contain the columns specified 
                       in 'redist_iso_col' and 'redist_values_col'"), collapse=" "))
  }
  # check data.frame
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be data.frame.")
  }
  if (!is.data.frame(redist_data)) {
    stop("Error: 'redist_data' must be data.frame.")
  }
  # check value numeric
  if (!all(sapply(data[, values_col], is.numeric))) {
    stop("Error: 'values_col' in 'data' must be numeric.")
  }
  if (!all(sapply(redist_data[, redist_values_col], is.numeric))) {
    stop("Error: 'redist_values_col' in 'redist_data' must be numeric.")
  }
  # check value not negative
  if (any(sapply(data[, values_col], function(x) x[!is.na(x)] < 0))) {
    stop(paste(strwrap("Error: Invalid 'value' detected. Negative values 'values_col' 
                       in 'data' not interpretable as quantities."), collapse=" "))
  }
  if (any(sapply(redist_data[, redist_values_col], function(x) x[!is.na(x)] < 0))) {
    stop(paste(strwrap("Error: Invalid 'value' detected. Negative values 
                       'redist_values_col' in 'redist_data'."), collapse=" "))
  }
  # check ISO 3166-1 in redist_data
  missing_from_redist <- data[[iso_col]][!data[[iso_col]] %in% 
                                           substr(redist_data[[redist_iso_col]], 1, 2)]
  if (length(missing_from_redist) > 0) {
    warning(paste(
      paste(strwrap("ISO 3166-2 code (subdivisions) has not been found in 'redist_data' 
                    for the following ISO 3166-1 codes (country) of 'data':"), 
            collapse = " "),
      paste(missing_from_redist, collapse = ", "), sep = "\n"))
  }
  # check first characters of ISO 3166-2 in data
  missing_from_data <- unique(
    redist_data[[redist_iso_col]][!substr(redist_data[[redist_iso_col]], 1, 2) %in% 
                                    data[[iso_col]]]
  )
  if (length(missing_from_data) > 0) {
    warning(paste(
      paste(strwrap("ISO 3166-1 code (country) has not been found in 'data' for the 
                    following ISO 3166-2 codes (subdivisions) of 'redist_data':"), 
            collapse=" "),
      paste(missing_from_data, collapse = ", "), sep="\n"))
  }
  
  
  new_cols <- c(ISO_2 = redist_iso_col, values_redistribution = redist_values_col)
  redist_df <- redist_data %>%
    rename(all_of(new_cols)) %>%
    select(ISO_2, values_redistribution)
  
  redist_df <- redist_df %>%
    mutate(ISO_1 = substr(ISO_2, 1, 2)) %>%
    filter(ISO_1 %in% unique(data[[iso_col]])) %>%
    group_by(ISO_1) %>%
    mutate(proportion = values_redistribution / sum(values_redistribution)) %>% 
    ungroup(ISO_1)
  
  df <- redist_df %>%
    left_join(select(data, !!iso_col, !!values_col),
              by = c("ISO_1" = iso_col)) %>%
    mutate(across(all_of(values_col),
                  .fns = list(redist = ~ . * proportion))) %>%
    select(
      ISO_1,
      ISO_2,
      proportion,
      ends_with("redist")
    ) %>%
    rename_with(~ sub("_redist$", "", .), ends_with("redist"))
  
  return(df)
}

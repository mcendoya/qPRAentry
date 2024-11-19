utils::globalVariables(c(".", "NUTS_CODES", "NUTS_ID", "CNTR_CODE", "CNTR_ID",
                         "NAME_ENGL"))
#' Data preparation
#'
#' @param data data.frame
#' @param select_period A vector specifying the time periods to be selected (from time_period column).
#' By default, it is set to NULL, meaning all periods in the data frames will be considered.
#' @param reporter Logical. If TRUE (default) values are grouped by reporter.
#' @param partner Logical. If TRUE values are grouped by partner (default is FALSE).
#'
#' @return A data frame with the values grouped by ID for each selected time period.
#' ID corresponds to reporter if TRUE or to partner if TRUE.
#' If reporter and partner are TRUE the data frame contains both columns.
#' 
#' @noRd
#' @keywords internal
summarise_data <- function(data, select_period = NULL, reporter = TRUE, partner = FALSE) {
  time_period <- value <- NULL
  data <- data %>%
    filter(if (!is.null(select_period)) time_period %in% select_period else TRUE)
  if(any(is.na(data$value))){
    message("Note: The input data contains missing values, these will be considered as zeros.")
  }
  if(!reporter & !partner){
    stop("At least one of the arguments 'reporter' or 'partner' must be TRUE.")
  }else if(reporter & !partner){
    data <- data %>%
      group_by(reporter, time_period) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>%
      rename(IDs = reporter)
  }else if(!reporter & partner){
    data <- data %>%
      group_by(partner, time_period) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "keep") %>%
      rename(IDs = partner)
  }else{
    data <- data %>%
      group_by(reporter, partner, time_period) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "keep")
  }
  return(data)
}

#' Identify and IDs without available data
#'
#' This internal function checks for time periods within the specified 
#' dataframe where the `total_available` values are zero. It prints a warning 
#' message for each ID that has zero values in the selected time periods, 
#' advising the user to select other time periods if they want to include 
#' these IDs in the analysis. The function then returns a vector of IDs that 
#' have zero values in the specified time periods.
#'
#' @param df1 A dataframe containing columns `reporter`and `time_period`
#' 
#' @param df2 A dataframe containing columns `reporter`and `time_period`
#' 
#' @param df3 A dataframe containing columns `reporter`and `time_period`
#' 
#' @param IDs A vector specifying the time periods to be selected. 
#' 
#' @param time_period A vector specifying the time periods to be selected. 
#'
#' @return A vector of IDs that have zero values
#' 
#' @noRd
#' @keywords internal
check_missing_ids <- function(df1, df2, df3, IDs, time_period) {
  ID <- tps <- NULL
  missing_info <- data.frame(ID = character(), 
                             tp = integer(), 
                             stringsAsFactors = FALSE)
  for (tp in time_period) {
    for (id in IDs) {
      id_in_df1 <- any(df1$reporter == id & df1$time_period == tp)
      id_in_df2 <- any(df2$reporter == id & df2$time_period == tp)
      id_in_df3 <- any(df3$reporter == id & df3$time_period == tp & df3$value != 0)
      
      if (!id_in_df1 & !id_in_df2 & !id_in_df3) {
        missing_info <- rbind(missing_info, data.frame(ID = id, tp = tp, stringsAsFactors = FALSE))
      }
    }
  }
  
  if (nrow(missing_info) > 0) {
    warning_messages <- missing_info %>%
      group_by(ID) %>%
      summarise(tps = paste(unique(tp), collapse = ", ")) %>%
      mutate(message = paste(ID, "in time periods", tps)) %>%
      pull(message)
    
    missing_IDs <- unique(missing_info$ID)
    warning_message <- 
      paste("Warning: No available data for:", paste(warning_messages, collapse = "; "),
            ". Therefore,", paste(missing_IDs, collapse = ", "), 
            paste(strwrap("will be excluded from the analysis.\nPlease select other 
                          time periods if you want to include"), collapse=" "),
            paste(missing_IDs, collapse = ", "), "in the analysis.")

  }else{
    missing_IDs <- c()
    warning_message <- c()
  }
  
  return(list(missing_IDs = missing_IDs,
              warning_message = warning_message))
}

#' Identify and fill missing intra-partner combinations in a data frame
#'
#' This internal function identifies missing intra-partner combinations in a given
#' data frame and fills them with zero values. It is designed to be used within other functions.
#'
#' @param data A data frame containing partner and reporter columns.
#' @param IDs A vector specifying the IDs used for generating combinations.
#'
#' @return A data frame with missing intra-partner combinations filled with zero values.
#' 
#' @noRd
#' @keywords internal
missing_intra <- function(data, IDs) {
  IDi <- IDj <- NULL
  df <- data.frame(IDi = IDs) %>%
    expand(IDi=IDi, IDj=IDi) %>%
    filter(IDj!=IDi)
  if (!all(paste(df$IDi, df$IDj) %in% paste(data$partner, data$reporter))) {
    missing_combinations <- anti_join(df, data[,c("partner", "reporter")],
                                      by = c("IDi"="partner",
                                             "IDj"="reporter"))
    missing_combinations$value <- 0
    data <- data %>%
      bind_rows(missing_combinations %>% rename(partner = IDi, reporter=IDj))
  }
  return(data)
}


#' Population data
#' 
#' This internal function download Eurostat population data
#'
#' @param nuts_level Numeric, NUTS level to obtain population data
#' @param nuts_filter Character, NUTS codes to filter
#'
#' @return A data frame with population data for each NUTS level
#' 
#' @noRd
#' @keywords internal
get_population_data <- function(nuts_level, nuts_filter=NULL) {
  sex <- age <- NULL
  length_nuts <- nuts_level + 2 # NUTS characters
  df <- eurostat::get_eurostat("demo_r_pjangrp3", time_format = "num") %>%
    filter(
      sex == "T" &
      unit == "NR" &
      age == "TOTAL" &
      nchar(geo) == length_nuts
    )
  if(!is.null(nuts_filter)){
    df <- df %>% filter(geo %in% nuts_filter)
  }

  return(df)
}

#' Cached function to memoize the retrieval of Eurostat data.
#'
#' This function uses memoization to cache the results of `get_population_data`,
#' avoiding repeated downloads of the data.
#'
#' @details
#' The function `cached_get_eurostat_data` memoizes the `get_population_data`
#' function using `memoise::memoise`, ensuring that the data is retrieved
#' from Eurostat only once and subsequent calls retrieve the cached results.
#'
#' @return A memoized version of `get_population_data`.
#' 
#' @noRd
#' @keywords internal
cached_get_eurostat_data <- memoise::memoise(get_population_data)

# EU map (from giscoR pkg)
get_EUmap <- function(year, nuts) {
  suppressWarnings(
    suppressMessages(
      giscoR::gisco_get_nuts(year = year, nuts_level = nuts) %>%
        select(NUTS_ID, CNTR_CODE) %>% 
        left_join(select(
          giscoR::gisco_get_countries() %>% st_drop_geometry(), 
          CNTR_ID, NAME_ENGL),
          by = join_by(CNTR_CODE == CNTR_ID)) %>% 
        rename(CNTR_NAME = NAME_ENGL)
    ))
}

cached_get_EUmap <- memoise::memoise(get_EUmap)
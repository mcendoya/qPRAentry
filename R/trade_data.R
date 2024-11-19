#' Prepare Trade Databases
#'
#' Prepares trade data for each country of interest based on the provided data.
#' This function generates objects of class \code{TradeData} required to be used 
#' in the [ntrade()] function of the [qPRAentry] package.
#' 
#' @param extra_total A data frame containing the total quantity of commodity imported
#' from third countries (pest-free and pest-present countries). It must contain the 
#' following columns: \code{reporter} (importing country), \code{partner} (exporting country), 
#' \code{value} (quantity of commodity) and \code{time_period} (time period of the trade activity).
#' @param extra_pest A data frame containing the quantity of commodity imported 
#' from third countries where the pest is present. It must contain the following columns: 
#' \code{reporter} (importing country), \code{partner} (exporting country), 
#' \code{value} (quantity of commodity) and \code{time_period} (time period of the trade activity).
#' The quantity of imported commodity detailed in this data frame must also be included 
#' in the \code{extra_total} data frame.
#' @param intra_trade A data frame containing the quantity of commodity traded between 
#' countries of interest. It must contain the following columns: 
#' \code{reporter} (importing country), \code{partner} (exporting country), 
#' \code{value} (quantity of commodity) and \code{time_period} (time period of the trade activity).
#' @param internal_production A data frame containing the quantity of the commodity 
#' produced domestically within each country of interest. It must contain the following columns: 
#' \code{reporter} (producing country), \code{value} (quantity of commodity) and 
#' \code{time_period} (time period of production).
#' @param filter_IDs A vector containing the country IDs to filter (identification codes 
#' of the countries of interest). By default, it is set to \code{NULL}, meaning all 
#' \code{reporter} countries in the data frames will be considered.
#' @param filter_period A vector specifying the time periods to filter, based on 
#' the \code{time_period} column. By default, it is set to \code{NULL}, meaning 
#' all time periods in the data frames will be considered.
#' 
#'
#' @details 
#' The function combines external imports from third countries, internal trade between 
#' the countries of interest and internal production data. It calculates the total amount 
#' of product available per country in each time period as the sum of external imports 
#' (from pest-free and pest-present countries) and internal production. 
#' 
#' ### Data columns:
#' Note that the data to be incorporated must contain the columns \code{reporter}, 
#' \code{partner} (except for \code{internal_production}), \code{value}, and \code{time_period}.
#' The trade flow is considered from partner to reporter, i.e., reporter as importer 
#' and partner as exporter. 
#' 
#' ### IDs - country identification codes:
#' For the IDs of countries of interest (i.e., in the the columns 
#' \code{reporter} of the four trade dataframes and in the column \code{partner} 
#' of \code{intra_trade}) it is recommended to use the the ISO 3166-1 (alpha-2) codes 
#' ([ISO 3166 Maintenance Agency](https://www.iso.org/iso-3166-country-codes.html)) 
#' or NUTS0 codes in case of European countries 
#' ([NUTS - Nomenclature of territorial units for statistics](https://ec.europa.eu/eurostat/web/nuts)) 
#' for subsequent compatibility with other functions of the [qPRAentry] package.
#' 
#' ### Time periods:
#' Time periods can be specified in any way, both numeric and character formatting is supported. 
#' For example, it can be expressed as years, months, specific periods, seasons, etc.
#' 
#' ### Trade adjustments:
#' Trade imbalances are adjusted, so that in case the internal export 
#' for a given country exceeds the total quantity available in that country, the internal 
#' export is recalculated proportionally based on the total available. 
#' Missing values are treated as zeros.
#'
#'
#' @return An object of class \code{TradeData} is returned containing the following list of dataframes:
#'
#'\itemize{
#' \item \code{total_trade} \verb{ } A data frame with one row for each ID and each time period with 9 variables:
#'
#' \tabular{lll}{
#'   \code{country_IDs} \tab \verb{ } \tab IDs of the countries of interest.\cr
#'   \tab \cr
#'   \code{time_period} \tab \verb{ } \tab Time period.\cr
#'   \tab \cr
#'   \code{extra_total} \tab \verb{ } \tab Total imports from third countries. \cr
#'   \tab \cr
#'   \code{extra_pest} \tab \verb{ } \tab Imports from third countries where the pest of 
#'   interest is present.\cr
#'   \tab \cr
#'   \code{intra_import} \tab \verb{ } \tab Internal import from the countries of 
#'   interest.\cr
#'   \tab \cr
#'   \code{intra_export} \tab \verb{ } \tab Internal export to the countries of 
#'   interest.\cr
#'   \tab \cr
#'   \code{internal_production} \tab \verb{ } \tab Internal production in the countries 
#'   of interest.\cr
#'   \tab \cr
#'   \code{total_available} \tab \verb{ } \tab Total available quantity in the countries 
#'   of interest.\cr
#'   \tab \cr
#'   \code{export_prop} \tab \verb{ } \tab Proportion of internal export to the total 
#'   available commodity. A value of 1 indicates that internal export is less than 
#'   or equal to the total available commodity; a value less than 1 [0, 1) indicates 
#'   that internal export exceeds the total available.\cr
#' }
#' }
#' \itemize{
#' \item \code{intra_trade} \verb{ } A data frame with values of trade commodity between 
#' countries of interest:
#'
#' \tabular{lll}{
#'   \code{reporter} \tab \verb{ } \tab Importing country ID.\cr
#'   \tab \cr
#'   \code{partner} \tab \verb{ } \tab Exporting country ID.\cr
#'   \tab \cr
#'   \code{time_period} \tab \verb{ } \tab Time period.\cr
#'   \tab \cr
#'   \code{value} \tab \verb{ } \tab Quantity of the commodity traded.\cr
#'   \tab \cr
#'   \code{export_prop} \tab \verb{ } \tab Proportion of internal export to the total 
#'   available commodity for each trading partner according to the proportion for 
#'   each partner (\code{export_prop} in \code{total_trade}).\cr
#' }
#' }
#' 
#' @seealso [load_csv()], [ntrade()]
#' 
#' @export
#'
#' @examples
#' ## Example with simulated trade data for Northern America
#' library(dplyr)
#' # Load data
#' data("datatrade_NorthAm")
#' # Total extra-import data: data contains imports from 5 third countries (column partner). 
#' extra_total <- datatrade_NorthAm$extra_import
#' # Extra-import data from countries where the pest is present (e.g., CNTR_1 and CNTR_2)
#' CNTR_pest <- c("CNTR_1", "CNTR_2")
#' extra_pest <- datatrade_NorthAm$extra_import %>% filter(partner%in%CNTR_pest)
#' # Intra-trade data
#' intra_trade  <- datatrade_NorthAm$intra_trade
#' # Internal production data
#' internal_production  <- datatrade_NorthAm$internal_production
#' # Generate trade data (TradeData object)
#' trade_NorthAm <- trade_data(extra_total = extra_total,
#'                             extra_pest = extra_pest,
#'                             intra_trade = intra_trade,
#'                             internal_production = internal_production)
#' head(trade_NorthAm$total_trade)
#' head(trade_NorthAm$intra_trade)
#' # Plot the total available quantity of commodity available in each country
#' library(ggplot2)
#' plot_countries(data = trade_NorthAm$total_trade,
#'                iso_col = "country_IDs", 
#'                values_col = "total_available") +
#'   xlim(-180,-20) + ylim(0,90)
#' 
#' ## Example with simulated trade data for Europe 
#' # with selected countries and a specific time period
#' # Load data
#' data("datatrade_EU")
#' # Total extra-import data: the total import is identified as partner "Extra_Total"
#' extra_total <- datatrade_EU$extra_import %>% filter(partner=="Extra_Total")
#' # Extra-import data from countries where the pest is present
#' extra_pest <- datatrade_EU$extra_import %>% filter(partner!="Extra_Total")
#' # Intra-trade data
#' intra_trade  <- datatrade_EU$intra_trade
#' # Internal production data
#' internal_production  <- datatrade_EU$internal_production
#' # Sample 5 countries from data
#' filter_IDs <- sample(unique(extra_total$reporter), 5)
#' # Generate trade data (TradeData object)
#' trade_EU <- trade_data(extra_total = extra_total,
#'                        extra_pest = extra_pest,
#'                        intra_trade = intra_trade,
#'                        internal_production = internal_production,
#'                        filter_IDs = filter_IDs,
#'                        filter_period = 2020)
#' # Plot the total available quantity of commodity available in each country
#' plot_countries(data = trade_EU$total_trade, 
#'                iso_col = "country_IDs", 
#'                values_col = "total_available") +
#'   xlim(-30,50) + ylim(25,70)
#'   
trade_data <- function(extra_total, extra_pest, intra_trade, internal_production,
                       filter_IDs = NULL, filter_period = NULL){
  reporter <- partner <- intra_export <- value <- export_prop <- time_period <- 
    total_available <- country_IDs <- NULL
  # check data.frames
  if(!all(is.data.frame(extra_total), is.data.frame(extra_pest), 
          is.data.frame(intra_trade), is.data.frame(internal_production))){
    no_df <- c()
    if(!is.data.frame(extra_total)){no_df <- c(no_df, "extra_total")}
    if(!is.data.frame(extra_pest)){no_df <- c(no_df, "extra_pest")}
    if(!is.data.frame(intra_trade)){no_df <- c(no_df, "intra_trade")}
    if(!is.data.frame(internal_production)){no_df <- c(no_df, "internal_production")}
    stop(paste0("Error: ", paste(no_df, collapse=", "), " must be data.frame."))
  }
  
  #check column names
  if (!all(c("reporter", "partner", "value", "time_period") %in% colnames(extra_total))) {
    stop(paste(strwrap("Error: extra_total must contain the columns 'reporter', 
                       'partner', 'value' and 'time_period'."), collapse=" "))
  }
  if (!all(c("reporter", "partner", "value", "time_period") %in% colnames(extra_pest))) {
    stop(paste(strwrap("Error: extra_pest must contain the columns 'reporter', 
                       'partner', 'value' and 'time_period'."), collapse=" "))
  }
  if (!all(c("reporter", "partner", "value", "time_period") %in% colnames(intra_trade))) {
    stop(paste(strwrap("Error: intra_trade must contain the columns 'reporter', 
                       'partner', 'value' and 'time_period'."), collapse=" "))
  }
  if (!all(c("reporter", "value", "time_period") %in% colnames(internal_production))) {
    stop(paste(strwrap("Error: internal_production must contain the columns 'reporter', 
                       'value' and 'time_period'."), collapse=" "))
  }
  
  #check value numeric
  if(!all(is.numeric(extra_total$value), is.numeric(extra_pest$value), 
          is.numeric(intra_trade$value), is.numeric(internal_production$value))){
    no_num <- c()
    if(!is.numeric(extra_total$value)){no_num <- c(no_num, "extra_total")}
    if(!is.numeric(extra_pest$value)){no_num <- c(no_num, "extra_pest")}
    if(!is.numeric(intra_trade$value)){no_num <- c(no_num, "intra_trade")}
    if(!is.numeric(internal_production$value)){no_num <- c(no_num, "internal_production")}
    stop(paste0("Error: 'value' in ", paste(no_num, collapse=", "), " must be numeric."))
  }
  
  # check value not negative
  if(!all(extra_total$value[!is.na(extra_total$value)]>=0, 
          extra_pest$value[!is.na(extra_pest$value)]>=0,
          intra_trade$value[!is.na(intra_trade$value)]>=0, 
          internal_production$value[!is.na(internal_production$value)]>=0)){
    neg_val <- c()
    if(any(extra_total$value[!is.na(extra_total$value)]<0)){
      neg_val <- c(neg_val, "extra_total")}
    if(any(extra_pest$value[!is.na(extra_pest$value)]<0)){
      neg_val <- c(neg_val, "extra_pest")}
    if(any(intra_trade$value[!is.na(intra_trade$value)]<0)){
      neg_val <- c(neg_val, "intra_trade")}
    if(any(internal_production$value[!is.na(internal_production$value)]<0)){
      neg_val <- c(neg_val, "internal_production")}
    stop(paste("Error: Invalid 'value' detected. Negative values in:", 
                paste(neg_val, collapse=", "), collapse=" "))
  }

  #selected IDs
  IDs <- filter_IDs
  if(!is.null(IDs)){
    extra_total <- extra_total %>%
      filter(reporter%in%IDs)
    extra_pest <- extra_pest %>%
      filter(reporter%in%IDs)
    intra_trade <- intra_trade %>%
      filter(reporter%in%IDs,
             partner%in%IDs)
    internal_production <- internal_production %>%
      filter(reporter%in%IDs)
  }else{
    IDs <- unique(c(extra_total$reporter, 
                    intra_trade$reporter, 
                    internal_production$reporter))
  }
  
  tp <- if(!is.null(filter_period)){
    filter_period
  }else{
    unique(c(extra_total$time_period, 
             intra_trade$time_period, 
             internal_production$time_period))
  }

  IDs_excluded <- check_missing_ids(extra_total, intra_trade, internal_production,
                                    IDs, tp)
  
  if(!is.null(IDs_excluded$missing_IDs)){
    IDs <- IDs[!IDs%in%IDs_excluded$missing_IDs]
    extra_total <- extra_total %>% 
      filter(!reporter%in%IDs_excluded$missing_IDs)
    extra_pest <- extra_pest %>% 
      filter(!reporter%in%IDs_excluded$missing_IDs)
    intra_trade <- intra_trade %>% 
      filter(!reporter%in%IDs_excluded$missing_IDs,
             !partner%in%IDs_excluded$missing_IDs)
    internal_production <- internal_production %>% 
      filter(!reporter%in%IDs_excluded$missing_IDs)
  }

  dataframes_list <- list("extra_total" = summarise_data(extra_total, filter_period),
                          "extra_pest" = summarise_data(extra_pest, filter_period),
                          "intra_import" = summarise_data(intra_trade, filter_period),
                          "intra_export" = summarise_data(intra_trade, filter_period,
                                                          reporter = FALSE, 
                                                          partner = TRUE),
                          "internal_production" = summarise_data(internal_production, 
                                                                 filter_period)
  )
  dataframes_list <- imap(dataframes_list, ~rename(.x, !!.y := "value"))
  
  total_trade <- data.frame(IDs = rep(IDs, each = length(tp)),
                            time_period = rep(tp, length(IDs)))
  
  total_trade <-reduce(
    dataframes_list,
    function(left, right) {
      full_join(left, right, by=join_by(IDs, time_period))
    },
    .init = total_trade) %>%
    replace(is.na(.), 0) %>%
    rowwise() %>%
    mutate(total_available = (extra_total + internal_production)) %>%
    # when intra export of an ID is greater than the total available
    # the export to each ID proportional to the total available
    mutate(export_prop = case_when(
      intra_export > total_available ~ {
        total_available / intra_export
      },
      .default = 1
    )) %>% 
    rename(country_IDs = IDs)
  
  error_ExtraPest <- total_trade %>%
    filter(isTRUE(all.equal(extra_pest, extra_total, tolerance = 1e-6)) == FALSE,
           extra_pest > extra_total) %>%
    select(country_IDs, time_period) %>%
    distinct()
  
  if (nrow(error_ExtraPest) > 0) {
    stop(paste(strwrap("Error: There are cases where the extra-pest import is higher 
                       than the extra-total import. The extra-total import must include 
                       the extra-pest import."), collapse=" "))
  }
  
  warning_cases <- total_trade %>%
    filter(isTRUE(all.equal(intra_export, total_available, tolerance = 1e-6)) == FALSE,
           intra_export > total_available) %>%
    select(country_IDs, time_period) %>%
    distinct()
  
  if (nrow(warning_cases) > 0) {
    message(
      paste(strwrap("Note: For countries where intra-export is greater than total 
                    available (extra-import + internal production), intra-export 
                    is considered proportional to the total available."), 
            collapse=" "))
  }
  
  if(!is.null(IDs_excluded$warning_message)){
    message(IDs_excluded$warning_message)
  }

  intra_trade <- summarise_data(intra_trade, filter_period,
                                reporter = TRUE, partner = TRUE) %>%
    left_join(select(total_trade, country_IDs, time_period, export_prop),
              by=c("partner" = "country_IDs", "time_period")) %>%
    mutate(value = value*export_prop) # proportional to the total available

  trade_df <- list("total_trade" = as.data.frame(total_trade), 
                   "intra_trade" = as.data.frame(intra_trade))
  class(trade_df) <- "TradeData"
  return(trade_df)
}

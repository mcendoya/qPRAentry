#' Ntrade calculation
#'
#' This function calculates the quantity of potentially infested imported commodity
#' (\eqn{N_{trade}}) from third countries where the pest is present, based on the 
#' provided trade datasets (output of the \code{\link{trade_data}} function). 
#'
#' The \eqn{N_{trade}} value represents the amount of potentially infested commodity left 
#' within each country after adjusting for both direct imports from pest-affected 
#' third countries and intra-country trade. The calculation of \eqn{N_{trade_i}} for each 
#' country of interest \eqn{i} is based on the equation:
#' \deqn{N_{trade_i} = ExtraPest_i - ExtraPest_i \sum_{j \neq i} R_{ij} + 
#' \sum_{j \neq i} ExtraPest_j R_{ji},}
#' where:
#' \itemize{
#' \item \eqn{N_{trade_i}}: quantity of commodity from third countries remaining in 
#' country \eqn{i}, taking into account the direct importation from third countries 
#' where the pest is present, the re-exportation to other countries of interest, 
#' and the indirect importation of the commodity from other countries of interest.
#' \item \eqn{ExtraPest_i} and \eqn{ExtraPest_j}: quantity of commodity imported by  
#' country \eqn{i} and country \eqn{j} from third countries where the pest is present 
#' (direct import), during the period of time considered.
#' \item \eqn{R_{ij}} and \eqn{R_{ji}}: proportion of intra-regional trade relative 
#' to the total available quantity in the exporting country defined as: 
#' \deqn{R_{ij} = IntraExp_{ij}/(IP_i + ExtraTotal_i), \\
#' R_{ji} = IntraExp_{ji}/(IP_j + ExtraTotal_j).}
#' Specifically, \eqn{R_{ij}} indicates the proportion of the commodity that is exported 
#' from country \eqn{i} to country \eqn{j} (\eqn{IntraExp_{ij}}), while \eqn{R_{ji}} 
#' indicates the proportion exported from country \eqn{j} to country \eqn{i} (\eqn{IntraExp_{ji}}), 
#' in both cases out of the total available commodity in the exporter country. The total 
#' available quantity is considered as tha sum of the internal production of the country 
#' (\eqn{IP}) and the total quantity imported from third countries (\eqn{ExtraTotal}).
#' Thus, the quantity of \eqn{ExtraPest_i} re-exported from country \eqn{i} to all countries 
#' \eqn{j} is approximated by \eqn{ExtraPest_i \sum_{j \neq i} R_{ij}}, and the quantity 
#' of \eqn{ExtraPest_j} re-exported from all countries \eqn{j} to country \eqn{i} as
#' \eqn{\sum_{j \neq i} ExtraPest_j R_{ji}}.
#' }
#'
#' @param trade_data An object of class \code{TradeData} that can be the output of \code{\link{trade_data}}.
#' @param filter_IDs A vector containing the country IDs to filter (identification codes 
#' of the countries of interest). By default, it is set to \code{NULL}, meaning all 
#' \code{reporter} countries in the data frames will be considered.
#' @param filter_period A vector specifying the time periods to filter, based on 
#' the \code{time_period} column. By default, it is set to \code{NULL}, meaning 
#' all time periods in the data frames will be considered.
#' @param summarise_result A character vector specifying functions to summarise the 
#' \eqn{N_{trade}} result for the selected time periods (\code{filter_period}). 
#' It accepts the expressions \code{"mean"} for the mean, \code{"sd"} for the standard 
#' deviation, \code{"median"} for the median value and \code{"quantile(p)"} where 
#' \code{p} is the probability for the quantiles to the given probabilities. See examples.
#'
#' @return A data frame with the quantity of commodity imported by each country of interest 
#' (\code{country_IDs}) from countries or regions where the pest is present. The result 
#' is returned for each time period if \code{summarise_result} is not specified 
#' (default is \code{NULL}). If a summary function is specified, the result will be 
#' summarised accordingly.
#'
#' @export
#'
#' @examples
#' ## Example with simulated trade data for Northern America
#' library(dplyr)
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
#' # Calculation of the Ntrade for each time period
#' ntrade_NorthAm <- ntrade(trade_data = trade_NorthAm)
#' head(ntrade_NorthAm)
#' # Ntrade summary for the time periods
#' ntrade_NorthAm_summary <- ntrade(trade_data = trade_NorthAm,
#'                                  summarise_result = c("mean", "sd", 
#'                                                       "quantile(0.025)", 
#'                                                       "median",
#'                                                       "quantile(0.975)"))
#' head(ntrade_NorthAm_summary)
#' # Plot the median of Ntrade
#' library(ggplot2)
#' plot_countries(data = ntrade_NorthAm_summary,
#'                iso_col = "country_IDs", 
#'                values_col = "median") +
#'   xlim(-180,-20) + ylim(0,90)
#' 
#' ## Example with simulated trade data for Europe 
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
#' # Generate trade data (TradeData object)
#' trade_EU <- trade_data(extra_total = extra_total,
#'                        extra_pest = extra_pest,
#'                        intra_trade = intra_trade,
#'                        internal_production = internal_production)
#' # Ntrade mean and sd for the time periods
#' ntrade_EU <- ntrade(trade_data = trade_EU,
#'                     summarise_result = c("mean", "sd"))
#' # Plot Ntrade mean
#' plot_countries(data = ntrade_EU, 
#'                iso_col="country_IDs", 
#'                values_col="mean") +
#'   xlim(-40,50) + ylim(25,70)
#' # Ntrade for selected countries and a specific time period
#' # Sample 5 countries from trade data
#' country_IDs <- sample(unique(trade_EU$total_trade$country_IDs), 5)
#' ntrade_EU_s <- ntrade(trade_data = trade_EU,
#'                       filter_IDs = country_IDs,
#'                       filter_period = 2020)
#' head(ntrade_EU_s)
#' # Plot Ntrade result
#' plot_countries(data = ntrade_EU_s, 
#'                iso_col="country_IDs", 
#'                values_col="Ntrade_2020") +
#'   xlim(-40,50) + ylim(25,70)
#' 
ntrade <- function(trade_data, filter_IDs = NULL, filter_period=NULL, summarise_result = NULL){
  reporter <- partner <- IDi <- Rij <- NULL
  # Check if trade is of class TradeData and has required elements
  if (!inherits(trade_data, "TradeData")) {
    stop("Error: 'trade_data' must be an object of class 'TradeData'. See ?trade_data.")
  }
  
  # Check if summarise_result is valid
  valid_functions <- c("mean", "sd", "median")
  valid_quantile <- grepl("^quantile\\(0\\.\\d+\\)$", summarise_result)
  valid_summarise <- all(summarise_result %in% valid_functions | valid_quantile)
  
  if (!is.null(summarise_result) && !valid_summarise) {
    stop(paste0("Error: 'summarise_result' must be a character vector specifying valid functions:\n",
                "'mean', 'sd', 'median', or 'quantile(p)' where p is a probability between 0 and 1."))
  }
  
  trade_df <- trade_data$total_trade
  intra_df <- trade_data$intra_trade
  country_IDs <- filter_IDs
  if(!is.null(country_IDs)){
    if(!all(country_IDs%in% trade_df$country_IDs) || !all(country_IDs%in%intra_df$reporter)){
      stop("Error: The selected 'filter_IDs' must be in 'country_IDs' in trade data")
    }
    trade_df <- trade_df %>% 
      filter(country_IDs %in% country_IDs)
    intra_df <- intra_df %>% 
      filter(reporter %in% country_IDs,
             partner %in% country_IDs)
  }else{
    country_IDs <- unique(trade_df$country_IDs)
  }
  
  Nt_calc <- function(country_IDs, intra_df, trade_df){
    dfR <- data.frame(IDi = country_IDs) %>%
      expand(IDi=IDi, IDj=IDi) %>%
      filter(IDj!=IDi)
    dfR$Rij <- 0
    for(i in country_IDs){
      total_i <- trade_df$total_available[trade_df$country_IDs == i]
      IDj <- country_IDs[country_IDs != i]
      for(j in IDj){
        intraExp_ij <- intra_df$value[intra_df$partner==i & intra_df$reporter==j]
        if(total_i>0){
          dfR$Rij[dfR$IDi==i & dfR$IDj == j] <- intraExp_ij/total_i
        }
      }
    }
    dfR <- dfR %>%
      mutate(Rji = Rij[match(paste(IDi, IDj), paste(IDj, IDi))])

    df_Nt <- data.frame(country_IDs = country_IDs, Ntrade = 0)

    for(i in country_IDs){
      IDj <- country_IDs[country_IDs != i]
      sum_Pest_j <- 0

      for(j in IDj){
        Pest_j <- trade_df$extra_pest[trade_df$country_IDs == j]
        if(length(Pest_j) == 0){
          sum_Pest_j <- sum_Pest_j
        }else{
          R_ji <- dfR$Rji[dfR$IDi ==i & dfR$IDj == j]
          sum_Pest_j <- sum_Pest_j + (Pest_j * R_ji)
        }
      }

      Pest_i <- trade_df$extra_pest[trade_df$country_IDs == i]
      if(length(Pest_i) == 0){
        df_Nt$Ntrade[df_Nt$country_IDs==i] <- sum_Pest_j
      }else{
        sum_Rij <- sum(dfR$Rij[dfR$IDi == i])
        df_Nt$Ntrade[df_Nt$country_IDs==i] <- Pest_i - (Pest_i * sum_Rij) + sum_Pest_j
      }
    }
    return(df_Nt)
  }

  tp <- if (!is.null(filter_period)) {
    if(!all(filter_period%in%trade_df$time_period) || !all(filter_period%in%intra_df$time_period)){
      stop("Error: The selected period 'filter_period' must be in 'time_period' in trade data.")
    }
    filter_period
  } else {
    unique(trade_df$time_period)
  }
  
  Nt_list <- map(tp, ~ {
    t <- .x
    df_Nt <- Nt_calc(
      country_IDs = country_IDs,
      intra_df = intra_df %>%
        filter(time_period==t) %>%
        missing_intra(country_IDs),
      trade_df = trade_df %>% 
        filter(time_period==t)
    )
    df_Nt <- df_Nt %>% 
      rename_with(.fn = ~paste0("Ntrade_", t), .cols = "Ntrade")
  })

  df_Nt <- reduce(Nt_list, left_join, by = "country_IDs")

  if(!is.null(summarise_result)){
    summarise_fns <- summarise_result[!grepl("quantile", summarise_result)]
    if (any(grepl("quantile", summarise_result))) {
      quantiles <- as.numeric(gsub("quantile\\((\\d+\\.?\\d*)\\)", "\\1",
                                   summarise_result[grepl("quantile", summarise_result)]))
      fns_names <- c(summarise_fns, paste0("q",quantiles))
    }else{
      quantiles <- NULL
      fns_names <- summarise_fns
    }
    fns <- append(map(summarise_fns, ~eval(parse(text = .x))),
                  map(quantiles, ~partial(quantile, probs = .x, na.rm=TRUE)))
    apply_functions <- function(...) {
      values <- c(...)
      setNames(map(fns, ~.x(values)), fns_names)
    }
    result <- df_Nt %>%
      select(starts_with("Ntrade")) %>%
      pmap_dfr(apply_functions)

    result <- cbind(country_IDs = df_Nt[,1], result)
  }else{
    result <- df_Nt
  }

return(result)
}

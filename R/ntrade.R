#' Ntrade calculation
#'
#' This function calculates the quantity of potentially infested imported commodity
#' (\eqn{N_{trade}}) from third countries where the pest is present.
#'
#' The calculation of \eqn{N_{trade_i}} for each country of interest \eqn{i}
#' is based on the equation:
#' \deqn{N_{trade_i} = ExtraPest_i - ExtraPest_i \sum \frac{IntraExp_{ij}}{Total_i}
#' + \sum ExtraPest_j \frac{IntraExp_{ji}}{Total_j},}
#' where:
#' \itemize{
#' \item \eqn{N_{trade_i}}: quantity of commodity from third countries remaining in
#' Member State \eqn{i} (\eqn{MS_i}), taking into account the direct importation from
#' a non-EU country where the pest is present, the re-exportation to other member states,
#' the indirect importation of the non-EU commodity from other member states.
#' \item \eqn{ExtraPest_i}: quantity of non-EU commodity imported (during the period
#' of time considered) by \eqn{MS_i} from countries where the pest is present (direct import).
#' \item \eqn{ExtraPest_j}: quantity of non-EU commodity imported (during the period
#' of time considered) by \eqn{MS_j} from countries where the pest is present (direct import).
#' \item \eqn{IntraExp_{ij}}: the quantity of commodity exported from \eqn{MS_i} to
#' \eqn{MS_j} (all origins confounded).
#' \item \eqn{IntraExp_{ji}}: the quantity of commodity exported from \eqn{MS_j} to
#' \eqn{MS_i} (all origins confounded).
#' \item \eqn{Total_i} or \eqn{Total_j}: total quantity of commodities available
#' in the \eqn{MS_i} or \eqn{MS_j}.
#' }
#'
#' @param trade An object of class `TradeData` that can be the output of \code{\link{trade_data}}.
#' @param IDs A vector containing the identification codes of the countries of interest.
#' @param select_period A vector specifying the time periods to be selected (from the \code{time_period} column).
#'   By default, it is set to NULL, meaning all periods in the trade data will be considered.
#' @param summarize_ntrade A character vector specifying functions to summarize the \eqn{N_{trade}} result
#'   for the selected time periods (\code{select_period}). It accepts the expressions \code{"mean"} for the mean, 
#'   \code{"sd"} for the standard deviation, "median" for the median value and
#'   \code{"quantile(p)"} where \code{p} is the probability for the quantiles to the given probabilities. See examples.
#'
#' @return A data frame with the quantity of commodity imported by each ID from countries or regions
#'   where the pest is present. The result is returned for each time period if \code{summarize_ntrade} is not specified
#'   (default is NULL). If a summary function is specified, the result will be summarized accordingly.
#'
#' @export
#'
#' @examples
#' ## Example with simulated trade data for Northern America
#' data("datatrade_NA")
#' library(dplyr)
#' # Total extra-import data: data contains imports from 5 third countries (column partner). 
#' extra_total <- datatrade_NA$extra_import
#' # Extra-import data from countries where the pest is present (e.g., CNTR_1 and CNTR_2)
#' CNTR_pest <- c("CNTR_1", "CNTR_2")
#' extra_pest <- datatrade_NA$extra_import %>% filter(partner%in%CNTR_pest)
#' # Intra-trade data
#' intra_trade  <- datatrade_NA$intra_trade
#' # Internal production data
#' internal_production  <- datatrade_NA$internal_production
#' # Generate trade data (TradeData object)
#' trade_NA <- trade_data(extra_total = extra_total,
#'                        extra_pest = extra_pest,
#'                        intra_trade = intra_trade,
#'                        internal_production = internal_production)
#' # Calculation of the Ntrade for each time period
#' ntrade_NA <- ntrade(trade = trade_NA)
#' head(ntrade_NA)
#' # Ntrade summary for the time periods
#' ntrade_NA_summary <- ntrade(trade = trade_NA,
#'                             summarize_ntrade = c("mean", "sd", 
#'                                                  "quantile(0.025)", 
#'                                                  "median",
#'                                                  "quantile(0.975)"))
#' head(ntrade_NA_summary)
#' # Plot the median of Ntrade
#' library(ggplot2)
#' plot_countries(data = ntrade_NA_summary,
#'                IDs_column = "IDs", 
#'                values_column = "median") +
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
#' ntrade_EU <- ntrade(trade = trade_EU,
#'                     summarize_ntrade = c("mean", "sd"))
#' # Plot Ntrade mean
#' plot_countries(data = ntrade_EU, 
#'                IDs_column="IDs", 
#'                values_column="mean") +
#'   xlim(-40,50) + ylim(25,70)
#' # Ntrade for selected countries and a specific time period
#' # Sample 5 countries from trade data
#' IDs <- sample(unique(trade_EU$total_trade$IDs), 5)
#' ntrade_EU_s <- ntrade(trade = trade_EU,
#'                       IDs = IDs,
#'                       select_period = 2020)
#' head(ntrade_EU_s)
#' # Plot Ntrade result
#' plot_countries(data = ntrade_EU_s, 
#'                IDs_column="IDs", 
#'                values_column="Ntrade_2020") +
#'   xlim(-40,50) + ylim(25,70)
#' 
ntrade <- function(trade, IDs = NULL, select_period=NULL, summarize_ntrade = NULL){
  reporter <- partner <- IDi <- Rij <- NULL
  # Check if trade is of class TradeData and has required elements
  if (!inherits(trade, "TradeData")) {
    stop("Error: 'trade' must be an object of class 'TradeData'. See ?trade_data.")
  }
  
  # Check if summarize_ntrade is valid
  valid_functions <- c("mean", "sd", "median")
  valid_quantile <- grepl("^quantile\\(0\\.\\d+\\)$", summarize_ntrade)
  valid_summarize <- all(summarize_ntrade %in% valid_functions | valid_quantile)
  
  if (!is.null(summarize_ntrade) && !valid_summarize) {
    stop(paste0("Error: 'summarize_ntrade' must be a character vector specifying valid functions:\n",
                "'mean', 'sd', 'median', or 'quantile(p)' where p is a probability between 0 and 1."))
  }
  
  trade_df <- trade$total_trade
  intra_df <- trade$intra_trade
  
  if(!is.null(IDs)){
    if(!all(IDs%in% trade_df$IDs) || !all(IDs%in%intra_df$reporter)){
      stop("Error: The selected 'IDs' must be in 'trade' data IDs.")
    }
    IDs_select <- IDs
    trade_df <- trade_df %>% 
      filter(IDs %in% IDs_select)
    intra_df <- intra_df %>% 
      filter(reporter %in% IDs_select,
             partner %in% IDs_select)
  }else{
    IDs <- unique(trade_df$IDs)
  }
  
  Nt_calc <- function(IDs, intra_df, trade_df){
    dfR <- data.frame(IDi = IDs) %>%
      expand(IDi=IDi, IDj=IDi) %>%
      filter(IDj!=IDi)
    dfR$Rij <- 0
    for(i in IDs){
      total_i <- trade_df$total_available[trade_df$IDs == i]
      IDj <- IDs[IDs != i]
      for(j in IDj){
        intraExp_ij <- intra_df$value[intra_df$partner==i & intra_df$reporter==j]
        dfR$Rij[dfR$IDi==i & dfR$IDj == j] <- intraExp_ij/total_i
      }
    }
    dfR <- dfR %>%
      mutate(Rji = Rij[match(paste(IDi, IDj), paste(IDj, IDi))])

    df_Nt <- data.frame(IDs = IDs, Ntrade = 0)

    for(i in IDs){
      IDj <- IDs[IDs != i]
      sum_Pest_j <- 0

      for(j in IDj){
        Pest_j <- trade_df$extra_pest[trade_df$IDs == j]
        if(length(Pest_j) == 0){
          sum_Pest_j <- sum_Pest_j
        }else{
          R_ji <- dfR$Rji[dfR$IDi ==i & dfR$IDj == j]
          sum_Pest_j <- sum_Pest_j + (Pest_j * R_ji)
        }
      }

      Pest_i <- trade_df$extra_pest[trade_df$IDs == i]
      if(length(Pest_i) == 0){
        Nt$Ntrade[Nt$IDs==i] <- sum_Pest_j
      }else{
        sum_Rij <- sum(dfR$Rij[dfR$IDi == i])
        df_Nt$Ntrade[df_Nt$IDs==i] <- Pest_i - (Pest_i * sum_Rij) + sum_Pest_j
      }
    }
    return(df_Nt)
  }

  tp <- if (!is.null(select_period)) {
    if(!all(select_period%in%trade_df$time_period) || !all(select_period%in%intra_df$time_period)){
      stop("Error: The selected period 'select_period' must be in 'time_period' in trade data.")
    }
    select_period
  } else {
    unique(trade_df$time_period)
  }
  
  Nt_list <- map(tp, ~ {
    t <- .x
    df_Nt <- Nt_calc(
      IDs = IDs,
      intra_df = intra_df %>%
        filter(time_period==t) %>%
        missing_intra(IDs),
      trade_df = trade_df %>% filter(time_period==t)
    )
    df_Nt <- df_Nt %>% rename_with(.fn = ~paste0("Ntrade_", t), .cols = "Ntrade")
  })

  df_Nt <- reduce(Nt_list, left_join, by = "IDs")

  if(!is.null(summarize_ntrade)){
    summarize_fns <- summarize_ntrade[!grepl("quantile", summarize_ntrade)]
    if (any(grepl("quantile", summarize_ntrade))) {
      quantiles <- as.numeric(gsub("quantile\\((\\d+\\.?\\d*)\\)", "\\1",
                                   summarize_ntrade[grepl("quantile", summarize_ntrade)]))
      fns_names <- c(summarize_fns, paste0("q",quantiles))
    }else{
      quantiles <- NULL
      fns_names <- summarize_fns
    }
    fns <- append(map(summarize_fns, ~eval(parse(text = .x))),
                  map(quantiles, ~partial(quantile, probs = .x, na.rm=TRUE)))
    apply_functions <- function(...) {
      values <- c(...)
      setNames(map(fns, ~.x(values)), fns_names)
    }
    result <- df_Nt %>%
      select(starts_with("Ntrade")) %>%
      pmap_dfr(apply_functions)

    result <- cbind(IDs = df_Nt[,1], result)
  }else{
    result <- df_Nt
  }

return(result)
}

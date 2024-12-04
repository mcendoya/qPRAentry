test_that("trade_data should return a list of data frames", {
  skip_on_cran()
  test_data <- datatrade_EU
  expect_message(
    result <- trade_data(extra_total = test_data$extra_import %>% 
                           filter(partner=="Extra_Total"), 
                         extra_pest = test_data$extra_import %>% 
                           filter(partner!="Extra_Total"),
                         intra_trade = test_data$intra_trade, 
                         internal_production = test_data$internal_production),
    paste(strwrap("Note: For countries where intra-export is greater than total 
                    available (extra-import + internal production), intra-export 
                    is considered proportional to the total available."), 
          collapse=" "), 
    fixed=TRUE
  )
  
  # Check if the result is a list and class TradeData
  expect_type(result, "list")
  expect_s3_class(result, "TradeData")
  
  # Check if the list contains the expected data frames
  expect_named(result, c("total_trade", "intra_trade"))
  expect_s3_class(result$total_trade, "data.frame")
  expect_s3_class(result$intra_trade, "data.frame")
})


test_that("trade_data messages", {
  skip_on_cran()
  test_data <- datatrade_EU
  internal_production <- test_data$internal_production
  internal_production$value[10] <- NA
  expect_message(
    expect_message(
      trade_data(extra_total = test_data$extra_import %>% 
                   filter(partner=="Extra_Total"), 
                 extra_pest = test_data$extra_import %>% 
                   filter(partner!="Extra_Total"),
                 intra_trade = test_data$intra_trade, 
                 internal_production = internal_production),
      "Note: The input data contains missing values, these will be considered as zeros."
    ),
    paste(strwrap("Note: For countries where intra-export is greater than total 
                    available (extra-import + internal production), intra-export 
                    is considered proportional to the total available."), 
          collapse=" "), 
    fixed=TRUE)

  expect_message(
    expect_message( #AT not available in 2020
      trade_data(extra_total = test_data$extra_import %>%
                   filter(partner=="Extra_Total",
                          !(reporter=="AT" & time_period==2020)),
                 extra_pest = test_data$extra_import %>%
                   filter(partner!="Extra_Total",
                          !(reporter=="AT" & time_period==2020)),
                 intra_trade = test_data$intra_trade %>%
                   filter(!(reporter=="AT" & time_period==2020)),
                 internal_production = test_data$internal_production %>%
                   filter(!(reporter=="AT" & time_period==2020))),
      paste("Warning: No available data for:",
            paste("AT in time periods 2020", collapse = "; "),
            ". Therefore,", paste("AT", collapse = ", "),
            paste(strwrap("will be excluded from the analysis.\nPlease select other
                          time periods if you want to include"), collapse=" "),
            paste("AT", collapse = ", "), "in the analysis."),
      fixed=TRUE
    ),
    paste(strwrap("Note: For countries where intra-export is greater than total
                    available (extra-import + internal production), intra-export
                    is considered proportional to the total available."),
          collapse=" "),
    fixed=TRUE)
})


test_that("trade_data errors", {
  skip_on_cran()
  test_data <- datatrade_EU
  
  #no numeric value
  expect_error(
    trade_data(extra_total = test_data$extra_import %>%
                 mutate(value=as.character(value)),
               extra_pest = test_data$extra_import %>%
                 mutate(value=as.character(value)),
               intra_trade = test_data$intra_trade %>%
                 mutate(value=as.character(value)),
               internal_production = test_data$internal_production %>%
                 mutate(value=as.character(value))),  
    "Error: 'value' in extra_total, extra_pest, intra_trade, internal_production must be numeric."
  )
  
  #negative values
  expect_error(
    trade_data(extra_total = test_data$extra_import %>% 
                 filter(partner=="Extra_Total") %>%
                 mutate(value=value-mean(value)),
               extra_pest = test_data$extra_import %>% 
                 filter(partner!="Extra_Total") %>%
                 mutate(value=value-mean(value)),
               intra_trade = test_data$intra_trade %>%
                 mutate(value=value-mean(value)),
               internal_production = test_data$internal_production %>%
                 mutate(value=value-mean(value))),  
    paste("Error: Invalid 'value' detected. Negative values in:", 
          paste(c("extra_total", "extra_pest", "intra_trade", "internal_production"), 
                collapse=", "), collapse=" ")
  )
  
  # extra_pest > extra_total
  expect_error(
    trade_data(extra_total = test_data$extra_import %>% filter(partner=="Extra_Total"),
               extra_pest = test_data$extra_import,
               intra_trade = test_data$intra_trade,
               internal_production = test_data$internal_production),
    paste(strwrap("Error: There are cases where the extra-pest import is higher 
                  than the extra-total import. The extra-total import must include 
                  the extra-pest import."), collapse=" ")
  )
})

test_that("Error when entries are not data frame", {
  skip_on_cran()
  # 1. none is data frame
  expect_error(
    trade_data(extra_total = list(), extra_pest = list(), 
               intra_trade = list(), internal_production = list()),
    "Error: extra_total, extra_pest, intra_trade, internal_production must be data.frame."
  )
  
  # 2. only one is not data frame
  expect_error(
    trade_data(extra_total = data.frame(), extra_pest = list(), 
               intra_trade = data.frame(), internal_production = data.frame()),
    "Error: extra_pest must be data.frame."
  )
  
  # 3. several are data frames
  expect_error(
    trade_data(extra_total = list(), extra_pest = data.frame(), 
               intra_trade = list(), internal_production = data.frame()),
    "Error: extra_total, intra_trade must be data.frame."
  )
})

test_that("missing columns", {
  skip_on_cran()
  test_data <- datatrade_EU
  
  expect_error(
    trade_data(extra_total = test_data$extra_import %>% select(!time_period),
               extra_pest = test_data$extra_import %>% filter(partner!="Extra_Total"),
               intra_trade = test_data$intra_trade, 
               internal_production = test_data$internal_production),
    paste(strwrap("Error: extra_total must contain the columns 'reporter', 'partner', 
                  'value' and 'time_period'."), collapse=" ")
  )
  expect_error(
    trade_data(extra_total = test_data$extra_import %>% filter(partner=="Extra_Total"),
               extra_pest = test_data$extra_import  %>% select(!value),
               intra_trade = test_data$intra_trade, 
               internal_production = test_data$internal_production),
    paste(strwrap("Error: extra_pest must contain the columns 'reporter', 'partner', 
                  'value' and 'time_period'."), collapse=" ")
  )
  expect_error(
    trade_data(extra_total = test_data$extra_import %>% 
                 filter(partner=="Extra_Total"),
               extra_pest = test_data$extra_import %>% 
                 filter(partner!="Extra_Total"),
               intra_trade = test_data$intra_trade %>% 
                 select(!partner), 
               internal_production = test_data$internal_production),
    paste(strwrap("Error: intra_trade must contain the columns 'reporter', 'partner', 
                  'value' and 'time_period'."), collapse=" ")
  )
  expect_error(
    trade_data(extra_total = test_data$extra_import %>% 
                 filter(partner=="Extra_Total"),
               extra_pest = test_data$extra_import %>% 
                 filter(partner!="Extra_Total"),
               intra_trade = test_data$intra_trade, 
               internal_production = test_data$internal_production %>% 
                 select(!reporter)),
    paste(strwrap("Error: internal_production must contain the columns 'reporter', 
                  'value' and 'time_period'."), collapse=" ")
  )
})

test_that("filter IDs and period", {
  skip_on_cran()
  test_data <- datatrade_EU
  
  filter_IDs <- c("AT","BE")
  res <- suppressMessages(
    trade_data(extra_total = test_data$extra_import %>% 
                 filter(partner=="Extra_Total"),
               extra_pest = test_data$extra_import %>% 
                 filter(partner!="Extra_Total"),
               intra_trade = test_data$intra_trade, 
               internal_production = test_data$internal_production,
               filter_IDs = filter_IDs))
  expect_setequal(res$total_trade$country_IDs, filter_IDs)
  
  filter_period <- 2020
  res <- suppressMessages(
    trade_data(extra_total = test_data$extra_import %>% 
                 filter(partner=="Extra_Total"),
               extra_pest = test_data$extra_import %>% 
                 filter(partner!="Extra_Total"),
               intra_trade = test_data$intra_trade, 
               internal_production = test_data$internal_production,
               filter_period = filter_period))
  expect_equal(unique(res$total_trade$time_period), filter_period)
})

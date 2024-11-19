test_that("trade_data should return a list of data frames", {
  skip_on_cran()
  test_data <- datatrade_EU
  expect_message(
    result <- trade_data(extra_total = test_data$extra_import %>% filter(partner=="Extra_Total"), 
                         extra_pest = test_data$extra_import %>% filter(partner!="Extra_Total"),
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
      trade_data(extra_total = test_data$extra_import %>% filter(partner=="Extra_Total"), 
                 extra_pest = test_data$extra_import %>% filter(partner!="Extra_Total"),
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
    expect_message( #AL not available in 2020
      trade_data(extra_total = test_data$extra_import %>% 
                   filter(partner=="Extra_Total",
                          !(reporter=="AL" & time_period==2020)),
                 extra_pest = test_data$extra_import %>% 
                   filter(partner!="Extra_Total",
                          !(reporter=="AL" & time_period==2020)),
                 intra_trade = test_data$intra_trade %>% 
                   filter(!(reporter=="AL" & time_period==2020)), 
                 internal_production = test_data$internal_production %>% 
                   filter(!(reporter=="AL" & time_period==2020))),
      paste("Warning: No available data for:", 
            paste("AL in time periods 2020", collapse = "; "),
            ". Therefore,", paste("AL", collapse = ", "), 
            paste(strwrap("will be excluded from the analysis.\nPlease select other 
                          time periods if you want to include"), collapse=" "),
            paste("AL", collapse = ", "), "in the analysis."),
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
  
  expect_error(
    trade_data(extra_total = test_data$extra_import %>% select(!time_period), #missing column
               extra_pest = test_data$extra_import %>% filter(partner!="Extra_Total"),
               intra_trade = test_data$intra_trade, 
               internal_production = test_data$internal_production),
    paste(strwrap("Error: extra_total must contain the columns 'reporter', 'partner', 
                  'value' and 'time_period'."), collapse=" ")
  )
  
  expect_error(
    trade_data(extra_total = test_data$extra_import %>% filter(partner=="Extra_Total"),
               extra_pest = test_data$extra_import %>% filter(partner!="Extra_Total"),
               intra_trade = test_data$intra_trade,
               internal_production = test_data$internal_production %>%
                 mutate(value=as.character(value))),  #no numeric value
    "Error: 'value' in internal_production must be numeric."
  )
  
  expect_error(
    trade_data(extra_total = test_data$extra_import %>% filter(partner=="Extra_Total"),
               extra_pest = test_data$extra_import %>% filter(partner!="Extra_Total"),
               intra_trade = test_data$intra_trade,
               internal_production = test_data$internal_production %>%
                 mutate(value=value-mean(value))),  #negative values
    paste("Error: Invalid 'value' detected. Negative values in:", 
          paste("internal_production", collapse=", "), collapse=" ")
  )
  
  expect_error(
    trade_data(extra_total = test_data$extra_import %>% filter(partner=="Extra_Total"),
               extra_pest = test_data$extra_import, # > extra_total
               intra_trade = test_data$intra_trade,
               internal_production = test_data$internal_production),
    paste(strwrap("Error: There are cases where the extra-pest import is higher 
                  than the extra-total import. The extra-total import must include 
                  the extra-pest import."), collapse=" ")
  )
})

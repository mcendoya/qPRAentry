test_that("ntrade_redist should return a data frame", {
  skip_on_cran()
  test_data <- datatrade_EU
  suppressMessages(
    trade <- trade_data(extra_total = test_data$extra_import %>% filter(partner=="Extra_Total"), 
                        extra_pest = test_data$extra_import %>% filter(partner!="Extra_Total"),
                        intra_trade = test_data$intra_trade, 
                        internal_production = test_data$internal_production))
  suppressMessages(
    nt <- ntrade(trade, 
                 summarize_ntrade = "median")
  )
  
  expect_s3_class(suppressMessages(ntrade_redist(ntrade_data=nt, 
                                                 nuts_column="IDs", 
                                                 values_column="median")),
                  "data.frame")
})

test_that("ntrade_redist errors", {
  skip_on_cran()
  test_data <- datatrade_EU
  suppressMessages(
    trade <- trade_data(extra_total = test_data$extra_import %>% filter(partner=="Extra_Total"), 
                        extra_pest = test_data$extra_import %>% filter(partner!="Extra_Total"),
                        intra_trade = test_data$intra_trade, 
                        internal_production = test_data$internal_production))
  suppressMessages(
    nt <- ntrade(trade, 
                 summarize_ntrade = "median")
  )
  
  df_redist <- NUTS_CODES %>% 
    mutate(values=rgamma(nrow(NUTS_CODES),0.5,0.5),
           values_neg=values-100)
  expect_error(ntrade_redist(ntrade_data=nt, 
                             nuts_column="IDs", 
                             values_column="median",
                             to_nuts = 2,
                             prop_data = df_redist,
                             prop_nuts_column = "CNTR_CODE", #wrong column
                             prop_values_column = "values"),
               "Error: 'prop_nuts_column' in 'prop_data' does not contain NUTS2 codes.")
  expect_error(ntrade_redist(ntrade_data=nt,
                             nuts_column="IDs",
                             values_column="median",
                             to_nuts = 2,
                             prop_data = df_redist,
                             prop_nuts_column = "NUTS2_CODE",
                             prop_values_column = "values_neg"),#negative values
               "Error: Invalid 'value' detected. Negative values 'prop_values_column' in 'prop_data'.")
  expect_error(ntrade_redist(ntrade_data=nt,
                             nuts_column="median",
                             values_column="median",
                             to_nuts = 2,
                             prop_data = df_redist,
                             prop_nuts_column = "NUTS2_CODE",
                             prop_values_column = "values"),
               "Error: 'nuts_column' in 'ntrade_data' does not contain NUTS Country codes (2-letter code country level).",
               fixed=TRUE)
})

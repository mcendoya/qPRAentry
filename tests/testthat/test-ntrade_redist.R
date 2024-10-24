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
                 summarise_result = "median")
  )
  
  expect_s3_class(suppressMessages(ntrade_redist(ntrade_data=nt, 
                                                 ntrade_nuts_col="country_IDs", 
                                                 ntrade_values_col="median")),
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
                 summarise_result = "median")
  )
  
  df_redist <- NUTS_CODES %>% 
    mutate(values=rgamma(nrow(NUTS_CODES),0.5,0.5),
           values_neg=values-100)
  expect_error(ntrade_redist(ntrade_data=nt, 
                             ntrade_nuts_col="country_IDs", 
                             ntrade_values_col="median",
                             to_nuts = 2,
                             redist_data = df_redist,
                             redist_nuts_col = "CNTR_CODE", #wrong column
                             redist_values_col = "values"),
               "Error: 'redist_nuts_col' in 'redist_data' does not contain NUTS2 codes.")
  expect_error(ntrade_redist(ntrade_data=nt,
                             ntrade_nuts_col="country_IDs",
                             ntrade_values_col="median",
                             to_nuts = 2,
                             redist_data = df_redist,
                             redist_nuts_col = "NUTS2_CODE",
                             redist_values_col = "values_neg"),#negative values
               "Error: Invalid 'value' detected. Negative values 'redist_values_col' in 'redist_data'.")
  expect_error(ntrade_redist(ntrade_data=nt,
                             ntrade_nuts_col="median",
                             ntrade_values_col="median",
                             to_nuts = 2,
                             redist_data = df_redist,
                             redist_nuts_col = "NUTS2_CODE",
                             redist_values_col = "values"),
               "Error: 'ntrade_nuts_col' in 'ntrade_data' does not contain NUTS Country codes (2-letter code country level).",
               fixed=TRUE)
})

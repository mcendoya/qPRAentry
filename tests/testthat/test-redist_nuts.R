test_that("redist_nuts should return a data frame", {
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
  
  expect_s3_class(suppressMessages(redist_nuts(data=nt, 
                                               nuts_col="country_IDs", 
                                               values_col="median")),
                  "data.frame")
})

test_that("redist_nuts with redist_data should return a data frame", {
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
  
  df_redist <- gisco_get_nuts(nuts_level=2) %>% 
    select(NUTS_ID) %>% 
    mutate(values=rgamma(nrow(.),0.5,0.5))
  
  expect_s3_class(suppressMessages(redist_nuts(data=nt, 
                                               nuts_col="country_IDs", 
                                               values_col="median",
                                               to_nuts = 2,
                                               redist_data = df_redist,
                                               redist_nuts_col = "NUTS_ID",
                                               redist_values_col = "values")),
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
  
  df_redist <- gisco_get_nuts(nuts_level=2) %>% 
    mutate(values=rgamma(nrow(.),0.5,0.5),
           values_neg=values-100)
  expect_error(redist_nuts(data=nt,
                           nuts_col="country_IDs",
                           values_col="median",
                           to_nuts = 2,
                           redist_data = df_redist,
                           redist_nuts_col = "CNTR_CODE", #wrong column
                           redist_values_col = "values"),
               "Error: 'redist_nuts_col' in 'redist_data' does not contain NUTS codes.")
  expect_error(redist_nuts(data=nt,
                           nuts_col="country_IDs",
                           values_col="median",
                           to_nuts = 2,
                           redist_data = df_redist,
                           redist_nuts_col = "NUTS_ID",
                           redist_values_col = "values_neg"),#negative values
               "Error: Invalid 'value' detected. Negative values 'redist_values_col' in 'redist_data'.")
  expect_error(redist_nuts(data=nt,
                           nuts_col="median",
                           values_col="median",
                           to_nuts = 2,
                           redist_data = df_redist,
                           redist_nuts_col = "NUTS_ID",
                           redist_values_col = "values"),
               "Error: 'nuts_col' in 'data' does not contain NUTS0 Country codes (2-letter code country level).",
               fixed=TRUE)
})

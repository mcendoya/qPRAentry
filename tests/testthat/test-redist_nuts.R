test_that("redist_nuts should return a data frame", {
  skip_on_cran()
  test_data <- datatrade_EU
  suppressMessages(
    trade <- trade_data(extra_total = test_data$extra_import %>% 
                          filter(partner=="Extra_Total"), 
                        extra_pest = test_data$extra_import %>% 
                          filter(partner!="Extra_Total"),
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
    trade <- trade_data(extra_total = test_data$extra_import %>% 
                          filter(partner=="Extra_Total"), 
                        extra_pest = test_data$extra_import %>% 
                          filter(partner!="Extra_Total"),
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
    trade <- trade_data(extra_total = test_data$extra_import %>% 
                          filter(partner=="Extra_Total"), 
                        extra_pest = test_data$extra_import %>% 
                          filter(partner!="Extra_Total"),
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
               paste(strwrap("Error: Invalid 'value' detected. Negative values 
                             'redist_values_col' in 'redist_data'."), collapse=" ")
  )
  expect_error(redist_nuts(data=nt,
                           nuts_col="median",
                           values_col="median",
                           to_nuts = 2,
                           redist_data = df_redist,
                           redist_nuts_col = "NUTS_ID",
                           redist_values_col = "values"),
               paste(strwrap("Error: 'nuts_col' in 'data' does not contain NUTS0 
                             Country codes (2-letter code country level)."), 
                     collapse=" "),
               fixed=TRUE)
  expect_error(redist_nuts(data=nt,
                           nuts_col="country_IDs",
                           values_col="median",
                           to_nuts = 2,
                           redist_data = df_redist,
                           redist_nuts_col = "NUTS_ID",
                           redist_values_col = "values",
                           nuts_year = 2018),
               paste(strwrap("Error: nuts_year not available. Try '2003', '2006', 
               '2010', '2013', '2016', '2021', or '2024'"), collapse=" "),
               fixed=TRUE)
  expect_error(redist_nuts(data=nt,
                           nuts_col="country_IDs",
                           values_col="median",
                           to_nuts = 2,
                           redist_data = "pop"),
               paste(strwrap("Error: 'redist_data' must be 'population' (default option) 
                             or a dataframe"), collapse=" "),
               fixed=TRUE)
  expect_error(redist_nuts(data=nt,
                           nuts_col="country_IDs",
                           values_col="median",
                           to_nuts = 2,
                           population_year = c(2010, 2014)),
               paste(strwrap("Error: The years specified in population_year 
                             are not available. Available years for population data 
                             are: "), collapse=" "))
})

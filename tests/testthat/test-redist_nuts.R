test_that("redist_nuts should return a data frame", {
  skip_on_cran()
  test_data <- datatrade_EU$internal_production %>% 
    filter(time_period==2020)
  expect_s3_class(suppressMessages(redist_nuts(data=test_data, 
                                               nuts_col="reporter", 
                                               values_col="value")),
                  "data.frame")
  df_redist <- datatrade_EU$consumption_nuts1
  expect_s3_class(suppressMessages(redist_nuts(data=test_data, 
                                               nuts_col="reporter", 
                                               values_col="value",
                                               to_nuts = 1,
                                               redist_data = df_redist,
                                               redist_nuts_col = "NUTS_ID",
                                               redist_values_col = "value")),
                  "data.frame")
})

# data errors
test_that("data errors", {
  skip_on_cran()
  test_data <- datatrade_EU$internal_production %>% 
    filter(time_period==2020)
  
  expect_error(redist_nuts(data=test_data,
                           nuts_col="value",
                           values_col="value"),
               paste(strwrap("Error: 'nuts_col' in 'data' does not contain NUTS0 
                             Country codes (2-letter code country level)."), 
                     collapse=" "),
               fixed=TRUE)
  expect_error(redist_nuts(data=list()),
               "Error: 'data' must be data.frame.")
  expect_error(redist_nuts(data=test_data,
                           nuts_col = "nuts_col",
                           values_col = "value"),
               paste(strwrap("Error: The dataframe 'data' must contain the columns 
                       specified in 'nuts_col' and 'values_col'."), collapse=" "))
  test_data <- test_data %>% mutate(neg_val = -value)
  expect_error(redist_nuts(data=test_data,
                           nuts_col = "reporter",
                           values_col = "neg_val"),
               paste(strwrap("Error: Invalid 'value' detected. Negative values 
                             'values_col' in 'data' not interpretable as quantities."), 
                     collapse=" "))
  test_data <- test_data %>% mutate(value_ch = as.character(value))
  
  expect_error(redist_nuts(data = test_data,
                           nuts_col = "reporter",
                           values_col = "value_ch"),
               "Error: 'values_col' in 'data' must be numeric.")
})

# check EL and UK changes
test_that("errors years", {
  skip_on_cran()
  test_data <- datatrade_EU$internal_production %>% 
    filter(time_period==2020) %>% 
    mutate(reporter = case_when(reporter == "EL" ~ "GR",
                                reporter == "UK" ~ "GB",
                                .default = reporter))
  res <- suppressMessages(
    redist_nuts(data=test_data,
                nuts_col="reporter",
                values_col="value",
                population_year = 2016))
  expect_true(all(c("EL", "UK") %in% res$NUTS0))
})

# errors years or population
test_that("errors years", {
  skip_on_cran()
  test_data <- datatrade_EU$internal_production %>% 
    filter(time_period==2020)
  expect_error(suppressMessages(
    redist_nuts(data=test_data,
                nuts_col="reporter",
                values_col="value",
                nuts_year = 2018),
    paste(strwrap("Error: nuts_year not available. Try '2003', '2006', 
               '2010', '2013', '2016', '2021', or '2024'"), collapse=" "),
    fixed=TRUE))
  
  expect_error(redist_nuts(data=test_data,
                           nuts_col="reporter",
                           values_col="value",
                           to_nuts = 2,
                           population_year = c(2010, 2014)),
               paste(strwrap("Error: The years specified in population_year 
                             are not available. Available years for population data 
                             are: "), collapse=" "))
  expect_error(redist_nuts(data=test_data,
                           nuts_col="reporter",
                           values_col="value",
                           nuts_year = 2000),
               paste(strwrap("Error: nuts_year not available. Try '2003', '2006', '2010', 
                       '2013', '2016', '2021', or '2024'"), collapse=" "))
})

# redist_data errors
test_that("redist_data errors", {
  skip_on_cran()
  test_data <- datatrade_EU$internal_production %>% 
    filter(time_period==2020)
  
  expect_error(redist_nuts(data=test_data,
                           nuts_col="reporter",
                           values_col="value",
                           redist_data = "pop"),
               paste(strwrap("Error: 'redist_data' must be 'population' (default option) 
                             or a data.frame."), collapse=" "),
               fixed=TRUE)
  
  df_redist <- datatrade_EU$consumption_nuts1
  
  expect_error(redist_nuts(data=test_data,
                           nuts_col="reporter",
                           values_col="value",
                           redist_data = list(df_redist)),
               paste(strwrap("Error: 'redist_data' must be 'population' (default option)
                             or a data.frame."), collapse = " "),
               fixed=TRUE)
  expect_error(redist_nuts(data=test_data,
                           nuts_col="reporter",
                           values_col="value",
                           to_nuts = 1,
                           redist_data = df_redist,
                           redist_nuts_col = "otro", #
                           redist_values_col = "value"),
               paste(strwrap("The dataframe 'redist_data' must contain the columns 
                             specified in 'redist_nuts_col' and 'redist_values_col'"), 
                     collapse=" "))
  expect_error(redist_nuts(data=test_data,
                           nuts_col="reporter",
                           values_col="value",
                           to_nuts = 1,
                           redist_data = df_redist,
                           redist_nuts_col = "value", #wrong column
                           redist_values_col = "value"),
               "Error: 'redist_nuts_col' in 'redist_data' does not contain NUTS codes.")
  
  df_redist <- df_redist %>% mutate(values_neg = -value)
  expect_error(redist_nuts(data=test_data,
                           nuts_col="reporter",
                           values_col="value",
                           to_nuts = 1,
                           redist_data = df_redist,
                           redist_nuts_col = "NUTS_ID",
                           redist_values_col = "values_neg"),#negative values
               paste(strwrap("Error: Invalid 'value' detected. Negative values
                             'redist_values_col' in 'redist_data'."), 
                     collapse=" "))
  
  df_redist <- df_redist %>% mutate(values_ch = as.character(value))
  expect_error(redist_nuts(data=test_data,
                           nuts_col="reporter",
                           values_col="value",
                           to_nuts = 1,
                           redist_data = df_redist,
                           redist_nuts_col = "NUTS_ID",
                           redist_values_col = "values_ch"),#
               "Error: 'redist_values_col' in 'redist_data' must be numeric.")
})

# Test that invalid NUTS codes trigger a warning
test_that("redist_nuts warns for invalid NUTS codes", {
  skip_on_cran()
  test_data <- datatrade_EU$internal_production %>%
    filter(time_period == 2020) %>%
    mutate(reporter = ifelse(reporter == "FR", "INVALID", reporter))
  
  expect_warning(
    suppressMessages(redist_nuts(data = test_data,
                                 nuts_col = "reporter",
                                 values_col = "value")),
    "The following NUTS0 codes are invalid or not available"
  )
  
  test_data <- datatrade_EU$internal_production %>%
    filter(time_period == 2020)
  df_redist <- datatrade_EU$consumption_nuts1 %>% 
    mutate(NUTS_ID = ifelse(NUTS_ID == "PT2", "INVALID", NUTS_ID))
  expect_warning(redist_nuts(data = test_data,
                             nuts_col = "reporter",
                             values_col = "value",
                             redist_data = df_redist,
                             redist_nuts_col = "NUTS_ID",
                             redist_values_col = "value",
                             to_nuts=1),
                 "The following NUTS codes are invalid or not available"
  )
})


# Test that the function correctly handles sf objects
test_that("redist_nuts handles sf objects by dropping geometry", {
  skip_on_cran()
  test_sf <- datatrade_EU$internal_production %>%
    filter(time_period == 2020) %>%
    mutate(longitude = runif(n(), min = -10, max = 40),
           latitude = runif(n(), min = 35, max = 60)) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  df_redist <- datatrade_EU$consumption_nuts1 %>% 
    mutate(longitude = runif(n(), min = -10, max = 40),
           latitude = runif(n(), min = 35, max = 60)) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  result <- suppressMessages(redist_nuts(data = test_sf,
                                         nuts_col = "reporter",
                                         values_col = "value",
                                         redist_data = df_redist,
                                         redist_nuts_col = "NUTS_ID",
                                         redist_values_col = "value",
                                         to_nuts = 1))
  expect_s3_class(result, "data.frame")
  expect_false("geometry" %in% names(result)) # Geometry should be dropped
})

# Test that the function correctly aggregates when multiple years are specified
test_that("redist_nuts aggregates population data across years", {
  skip_on_cran()
  test_data <- datatrade_EU$internal_production %>%
    filter(time_period == 2020)
  
  result <- suppressMessages(redist_nuts(data = test_data,
                                         nuts_col = "reporter",
                                         values_col = "value",
                                         population_year = c(2014, 2015)))
  
  # Check that the resulting data is aggregated (average of values)
  expect_true("value" %in% colnames(result))
  expect_true(all(result$value >= 0)) # Ensure no invalid negative values
})

# Test behavior when NUTS level requested is unsupported
test_that("redist_nuts errors for unsupported NUTS levels", {
  skip_on_cran()
  test_data <- datatrade_EU$internal_production %>%
    filter(time_period == 2020)
  
  expect_error(redist_nuts(data = test_data,
                           nuts_col = "reporter",
                           values_col = "value",
                           to_nuts = 4),
               paste(strwrap("Error: 'to nuts' must be numeric, 1, 2 or 3 NUTS 
                             level for redistribution."), collapse=" ")
  )
})


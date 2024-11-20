# Test that the function correctly handles sf objects
test_that("redist_iso handles sf objects by dropping geometry", {
  skip_on_cran()
  test_sf <- datatrade_NorthAm$internal_production %>%
    filter(time_period == "January-March") %>%
    mutate(longitude = runif(n(), min = -10, max = 40),
           latitude = runif(n(), min = 35, max = 60)) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  df_redist <- datatrade_NorthAm$consumption_iso2 %>% 
    mutate(longitude = runif(n(), min = -10, max = 40),
           latitude = runif(n(), min = 35, max = 60)) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  result <- redist_iso(data = test_sf,
                       iso_col = "reporter",
                       values_col = "value",
                       redist_data = df_redist,
                       redist_iso_col = "iso_3166_2",
                       redist_values_col = "value")
  expect_s3_class(result, "data.frame")
  expect_false("geometry" %in% names(result)) # Geometry should be dropped
})
###

test_that("redist_iso should return a data frame", {
  skip_on_cran()
  test_data <- datatrade_NorthAm$internal_production %>% 
    filter(time_period == "January-March")
  df_redist <- datatrade_NorthAm$consumption_iso2
  expect_s3_class(suppressMessages(redist_iso(data=test_data, 
                                              iso_col="reporter", 
                                              values_col="value",
                                              redist_data = df_redist,
                                              redist_iso_col = "iso_3166_2",
                                              redist_values_col = "value")),
                  "data.frame")
})

# data errors
test_that("data errors", {
  skip_on_cran()
  test_data <- datatrade_NorthAm$internal_production %>% 
    filter(time_period == "January-March")
  df_redist <- datatrade_NorthAm$consumption_iso2
  
  expect_error(redist_iso(data=list()),
               "Error: 'data' must be data.frame.")
  
  expect_error(redist_iso(data=test_data,
                          iso_col = "iso_col",
                          values_col = "value",
                          redist_data = df_redist,
                          redist_iso_col = "iso_3166_2",
                          redist_values_col = "value"),
               paste(strwrap("The dataframe 'data' must contain the columns specified 
                             in 'iso_col' and 'values_col'."), collapse=" "))
  
  test_data <- test_data %>% 
    mutate(value_ch = as.character(value),
           value_neg = -value)
  expect_error(redist_iso(data = test_data,
                          iso_col = "reporter",
                          values_col = "value_ch",
                          redist_data = df_redist,
                          redist_iso_col = "iso_3166_2",
                          redist_values_col = "value"),
               "Error: 'values_col' in 'data' must be numeric.")
  expect_error(redist_iso(data=test_data,
                          iso_col = "reporter",
                          values_col = "value_neg",
                          redist_data = df_redist,
                          redist_iso_col = "iso_3166_2",
                          redist_values_col = "value"),
               paste(strwrap("Error: Invalid 'value' detected. Negative values
                             'values_col' in 'data' not interpretable as quantities."),
                     collapse=" "))
})

# redist_data errors
test_that("redist_data errors", {
  skip_on_cran()
  test_data <- datatrade_NorthAm$internal_production %>% 
    filter(time_period == "January-March")
  df_redist <- datatrade_NorthAm$consumption_iso2
  
  expect_error(redist_iso(data=test_data,
                          iso_col = "reporter",
                          values_col = "value",
                          redist_data = list()),
               "Error: 'redist_data' must be data.frame.")
  
  expect_error(redist_iso(data=test_data,
                          iso_col = "reporter",
                          values_col = "value",
                          redist_data = df_redist,
                          redist_iso_col = "iso",
                          redist_values_col = "value"),
               paste(strwrap("The dataframe 'redist_data' must contain the columns 
                             specified in 'redist_iso_col' and 'redist_values_col'."), 
                     collapse=" "))
  
  df_redist <- df_redist %>% 
    mutate(value_ch = as.character(value),
           value_neg = -value)
  expect_error(redist_iso(data = test_data,
                          iso_col = "reporter",
                          values_col = "value",
                          redist_data = df_redist,
                          redist_iso_col = "iso_3166_2",
                          redist_values_col = "value_ch"),
               "Error: 'redist_values_col' in 'redist_data' must be numeric.")
  expect_error(redist_iso(data=test_data,
                          iso_col = "reporter",
                          values_col = "value",
                          redist_data = df_redist,
                          redist_iso_col = "iso_3166_2",
                          redist_values_col = "value_neg"),
               paste(strwrap("Error: Invalid 'value' detected. Negative values
                             'redist_values_col' in 'redist_data'."), collapse=" "))
})

# Test that invalid ISO codes trigger a warning
test_that("redist_iso warns for invalid ISO codes", {
  skip_on_cran()
  test_data <- datatrade_NorthAm$internal_production %>%
    filter(time_period == "January-March") %>%
    mutate(reporter = ifelse(reporter == "BM", "INVALID", reporter))

  df_redist <- datatrade_NorthAm$consumption_iso2 %>%
    filter(!iso_3166_2%in%iso_3166_2[substr(iso_3166_2, 1, 2)=="BM"])

  expect_warning(
    redist_iso(data = test_data,
               iso_col = "reporter",
               values_col = "value",
               redist_data = df_redist,
               redist_iso_col = "iso_3166_2",
               redist_values_col = "value"),
    paste(strwrap("ISO 3166-2 code (subdivisions) has not been found in 'redist_data'
                  for the following ISO 3166-1 codes (country) of 'data'"),
          collapse = " "), fixed=TRUE)

test_data <- datatrade_NorthAm$internal_production %>%
  filter(time_period == "January-March") %>% 
  filter(reporter!="US")

df_redist <- datatrade_NorthAm$consumption_iso2 %>%
  mutate(reporter = ifelse(iso_3166_2 == "US-WA", "INVALID", iso_3166_2))

expect_warning(
  redist_iso(data = test_data,
             iso_col = "reporter",
             values_col = "value",
             redist_data = df_redist,
             redist_iso_col = "iso_3166_2",
             redist_values_col = "value"),
  paste(strwrap("ISO 3166-1 code (country) has not been found in 'data' for the
                following ISO 3166-2 codes (subdivisions) of 'redist_data':"),
        collapse=" "), fixed=TRUE)
})


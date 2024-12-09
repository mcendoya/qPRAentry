test_that("ntrade should return a data frame", {
  skip_on_cran()
  test_data <- datatrade_EU
  suppressMessages(
    trade <- trade_data(extra_total = test_data$extra_import %>% 
                          filter(partner=="Extra_Total"), 
                        extra_pest = test_data$extra_import %>% 
                          filter(partner!="Extra_Total"),
                        intra_trade = test_data$intra_trade, 
                        internal_production = test_data$internal_production)
  )

  expect_no_error(
    nt <- ntrade(trade, 
                 filter_IDs = c("AT", "ES", "IT"), 
                 filter_period=NULL, 
                 summarise_result = NULL)
  )
  expect_equal(nrow(nt), 3)
  expect_s3_class(nt, "data.frame")
})

test_that("ntrade errors", {
  skip_on_cran()
  test_data <- datatrade_EU
  suppressMessages(
    trade <- trade_data(extra_total = test_data$extra_import %>% 
                          filter(partner=="Extra_Total"), 
                         extra_pest = test_data$extra_import %>% 
                          filter(partner!="Extra_Total"),
                         intra_trade = test_data$intra_trade, 
                         internal_production = test_data$internal_production)
  )
  expect_error(
    ntrade(trade, 
           filter_IDs = 1:3, 
           filter_period=NULL, 
           summarise_result = NULL),
    "Error: The selected 'filter_IDs' must be in 'country_IDs' in trade data"
  )
  expect_no_error(
    nt <- ntrade(trade, 
                 filter_IDs = c("AT", "ES", "IT"), 
                 filter_period=NULL, 
                 summarise_result = NULL)
  )
  expect_equal(nrow(nt), 3)
  expect_error(
    ntrade(trade,
           filter_period=1:3,
           summarise_result = NULL),
    paste(strwrap("Error: The selected period 'filter_period' must be in 'time_period' 
                  in trade data."), collapse=" ")
  )
  expect_error(
    ntrade(trade,
           summarise_result = "Mean"),
    paste(strwrap("Error: 'summarise_result' must be a character vector specifying 
                       valid functions: 'mean', 'sd', 'median', or 'quantile(p)' 
                       where p is a probability between 0 and 1."), collapse=" "),
    fixed=TRUE
  )
  
  expect_error(
    ntrade(data.frame(a=1, b=2)),
    "Error: 'trade_data' must be an object of class 'TradeData'. See ?trade_data.",
    fixed=TRUE
  )
})

test_that("ntrade errors", {
  skip_on_cran()
  test_data <- datatrade_EU
  suppressMessages(
    trade <- trade_data(extra_total = test_data$extra_import %>% 
                          filter(partner=="Extra_Total"), 
                        extra_pest = test_data$extra_import %>% 
                          filter(partner!="Extra_Total"),
                        intra_trade = test_data$intra_trade, 
                        internal_production = test_data$internal_production)
  )
    # filter period
    res <- ntrade(trade, 
           filter_period=2020)
    expect_true("Ntrade_2020" %in% names(res))
    
    # summarise
    res <- ntrade(trade, 
                  summarise_result = c("quantile(0.5)", "median"))
    expect_true(all(c("q0.5", "median") %in% names(res)))
    
    res <- ntrade(trade, 
                  summarise_result = c("median"))
    expect_false(all(c("q0.5", "median") %in% names(res)))
})


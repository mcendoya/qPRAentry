test_that("ntrade should return a data frame", {
  skip_on_cran()
  test_data <- datatrade_EU
  suppressMessages(
    trade <- trade_data(extra_total = test_data$extra_import %>% filter(partner=="Extra_Total"), 
                        extra_pest = test_data$extra_import %>% filter(partner!="Extra_Total"),
                        intra_trade = test_data$intra_trade, 
                        internal_production = test_data$internal_production)
  )

  expect_no_error(
    nt <- ntrade(trade, 
                 filter_IDs = c("AL", "ES", "IT"), 
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
    trade <- trade_data(extra_total = test_data$extra_import %>% filter(partner=="Extra_Total"), 
                         extra_pest = test_data$extra_import %>% filter(partner!="Extra_Total"),
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
                 filter_IDs = c("AL", "ES", "IT"), 
                 filter_period=NULL, 
                 summarise_result = NULL)
  )
  expect_equal(nrow(nt), 3)
  expect_error(
    ntrade(trade,
           filter_period=1:3,
           summarise_result = NULL),
    "Error: The selected period 'filter_period' must be in 'time_period' in trade data."
  )
  expect_error(
    ntrade(trade,
           summarise_result = "Mean"),
    paste0("Error: 'summarise_result' must be a character vector specifying valid functions:\n",
           "'mean', 'sd', 'median', or 'quantile(p)' where p is a probability between 0 and 1."),
    fixed=TRUE
  )
  
  expect_error(
    ntrade(data.frame(a=1, b=2)),
    "Error: 'trade_data' must be an object of class 'TradeData'. See ?trade_data.",
    fixed=TRUE
  )
})

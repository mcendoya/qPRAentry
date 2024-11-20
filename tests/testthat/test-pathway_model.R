test_that("pathway_model ntrade_data errors", {
  skip_on_cran()
  
  expect_error(pathway_model(ntrade_data = list()),
               "Error: 'ntrade_data' must be a data.frame.")
  
  test_data <- datatrade_EU$consumption_nuts1
  expect_error(pathway_model(test_data, "NUTS_ID", "xx"),
               paste(strwrap("Error: 'ntrade_data' must contain the columns specified 
                             in 'IDs_col' and 'values_col'."), collapse=" "))
})

test_that("pathway_model parameters errors", {
  skip_on_cran()
  
  test_data <- datatrade_EU$consumption_nuts1
  expression <- "p1*p2"
  expect_error(pathway_model(test_data, "NUTS_ID", "value",
                             expression, parameters = c("p1", "p2")),
               "Error: 'parameters' must be a list.")
  
  parameters <- list(p1 = 1, p2 = 2)
  expect_error(
    pathway_model(test_data, "NUTS_ID", "value", expression, parameters),
    paste("Error: Each element within 'parameters' must be a list.",
          "Example: parameters = list(param1 = list(), param2 = list()).", 
          sep = "\n"), fixed=TRUE)
  
  parameters <- list(p1 = list(), p3 = list())
  expect_error(pathway_model(test_data, "NUTS_ID", "value",
                             expression, parameters),
               paste(strwrap("Error: The parameters in 'expression' do not match
                             those specified in 'parameters'."), collapse=" "))
  
  parameters <- list(p1 = list("narm", 0, 1), p2 = list("norm", 0, 1))
  expect_error(pathway_model(test_data, "NUTS_ID", "value",
                             expression, parameters),
               "Error: The distribution function narm is not valid.")
})

test_that("pathway_model return data.frame", {
  skip_on_cran()
  
  test_data <- datatrade_EU$consumption_nuts1
  expression <- "p1*p2"
  parameters <- list(p1 = list("unif", 0, 10), p2 = list("unif", 0, 10))
  result <- pathway_model(test_data, "NUTS_ID", "value",
                          expression, parameters)
  expect_s3_class(result, "data.frame")
})

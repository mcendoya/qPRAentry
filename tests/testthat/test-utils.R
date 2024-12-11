test_that("summarise_data error", {
  expect_error(
    summarise_data(datatrade_EU$extra_import,
                   reporter = FALSE),
    "At least one of the arguments 'reporter' or 'partner' must be TRUE."
  )
})

test_that("missing_intra IDs", {
    res <- missing_intra(datatrade_EU$intra_trade[-1,],
                         datatrade_EU$intra_trade$reporter)
    expect_true("BE" %in% res$reporter[res$partner=="AT"])
})

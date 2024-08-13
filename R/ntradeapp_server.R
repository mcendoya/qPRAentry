#' The ntrade application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' 
#' @noRd
ntradeapp_server <- function(input, output, session) {
  data_vals <- mod_ntrade_data_server("ntrade_data")

  Nt_result <- mod_ntrade_results_server("ntrade_results",
                                         trade_done = data_vals$trade_done,
                                         time_period = data_vals$time_period,
                                         units = data_vals$units,
                                         TradeData = data_vals$TradeData)
  mod_ntrade_redistribution_server("ntrade_redistribution",
                                   Nt = Nt_result,
                                   time_period = data_vals$time_period,
                                   units = data_vals$units)
  shinyjs::disable(selector = 'a[data-value="tab2"]')
  shinyjs::disable(selector = 'a[data-value="tab3"]')
  shinyjs::runjs("document.querySelector('a[data-value=\"tab2\"]').classList.add('disabled-tab');")
  shinyjs::runjs("document.querySelector('a[data-value=\"tab3\"]').classList.add('disabled-tab');")
  
  observeEvent(data_vals$trade_done(),{
    shinyjs::enable(selector = 'a[data-value="tab2"]')
    shinyjs::enable(selector = 'a[data-value="tab3"]')
    
    # Remove CSS class from enable tabs
    shinyjs::runjs("document.querySelector('a[data-value=\"tab2\"]').classList.remove('disabled-tab');")
    shinyjs::runjs("document.querySelector('a[data-value=\"tab3\"]').classList.remove('disabled-tab');")
    updateTabsetPanel(session, "trade_tabs", selected = "tab2" )
  })

  session$onSessionEnded(function() { stopApp() })

}

#' The pathway application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' 
#' @noRd
pathwayapp_server <- function(input, output, session) {
  model <- mod_pathway_model_server("pathway_model")

  parameters <- mod_pathway_parameters_server("pathway_parameters",
                                              ntrade_data = model$ntrade_data, 
                                              nuts = model$nuts, 
                                              values = model$values, 
                                              model_done = model$model_done, 
                                              parameters = model$parameters)
  mod_pathway_results_server("pathway_results",
                             dist_done = parameters$dist_done, 
                             n_iter = parameters$n_iter,
                             model_def = model$model_def,
                             dist_result = parameters$dist_result,
                             ntrade_df = model$ntrade_df)

  shinyjs::disable(selector = 'a[data-value="tab2"]')
  shinyjs::disable(selector = 'a[data-value="tab3"]')
  shinyjs::runjs("document.querySelector('a[data-value=\"tab2\"]').classList.add('disabled-tab');")
  shinyjs::runjs("document.querySelector('a[data-value=\"tab3\"]').classList.add('disabled-tab');")

  observeEvent(model$model_done(),{
    shinyjs::enable(selector = 'a[data-value="tab2"]')
    shinyjs::runjs("document.querySelector('a[data-value=\"tab2\"]').classList.remove('disabled-tab');")
  })
  
  observeEvent(model$go_parameters(),{
    updateTabsetPanel(session, "pathway_tabs", selected = "tab2" )
  })
  
  observeEvent(parameters$dist_done(),{
    shinyjs::enable(selector = 'a[data-value="tab3"]')
    shinyjs::runjs("document.querySelector('a[data-value=\"tab3\"]').classList.remove('disabled-tab');")
  })
  
  observeEvent(parameters$go_results(),{
    updateTabsetPanel(session, "pathway_tabs", selected = "tab3")
  })
  
  observeEvent(c(model$ntrade_data(), model$nuts(), model$values(), model$model_done()),{
    shinyjs::disable(selector = 'a[data-value="tab3"]')
    shinyjs::runjs("document.querySelector('a[data-value=\"tab3\"]').classList.add('disabled-tab');")
  })

  session$onSessionEnded(function() { stopApp() })

}

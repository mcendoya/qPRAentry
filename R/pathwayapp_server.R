#' The pathway application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' 
#' @noRd
pathwayapp_server <- function(input, output, session) {
  model <- mod_pathway_model_server("pathway_model")
  ntrade_data <- mod_pathway_ntrade_server("pathway_ntrade")
  parameters <- mod_pathway_parameters_server("pathway_parameters",
                                              ntrade_data = ntrade_data$ntrade_data, 
                                              nuts = ntrade_data$nuts, 
                                              values = ntrade_data$values, 
                                              model_done = model$model_done, 
                                              parameters = model$parameters)
  mod_pathway_results_server("pathway_results",
                             dist_done = parameters$dist_done,
                             n_iter = parameters$n_iter,
                             model_def = model$model_def,
                             param_names = model$parameters,
                             par_settings = parameters$par_settings,
                             dist_result = parameters$dist_result,
                             ntrade_df = ntrade_data$ntrade_df)

  shinyjs::disable(selector = 'a[data-value="tab2"]')
  shinyjs::disable(selector = 'a[data-value="tab3"]')
  shinyjs::disable(selector = 'a[data-value="tab4"]')
  shinyjs::runjs("document.querySelector('a[data-value=\"tab2\"]').classList.add('disabled-tab');")
  shinyjs::runjs("document.querySelector('a[data-value=\"tab3\"]').classList.add('disabled-tab');")
  shinyjs::runjs("document.querySelector('a[data-value=\"tab4\"]').classList.add('disabled-tab');")

  observeEvent(model$model_done(),{
    shinyjs::enable(selector = 'a[data-value="tab2"]')
    shinyjs::runjs("document.querySelector('a[data-value=\"tab2\"]').classList.remove('disabled-tab');")
  })
  
  observeEvent(ntrade_data$data_done(),{
    shinyjs::enable(selector = 'a[data-value="tab3"]')
    shinyjs::runjs("document.querySelector('a[data-value=\"tab3\"]').classList.remove('disabled-tab');")
  })

  observeEvent(parameters$dist_done(),{
    shinyjs::enable(selector = 'a[data-value="tab4"]')
    shinyjs::runjs("document.querySelector('a[data-value=\"tab4\"]').classList.remove('disabled-tab');")
  })

  observeEvent(c(ntrade_data$ntrade_data(), ntrade_data$nuts(), ntrade_data$values(), 
                 model$model_done(), ntrade_data$data_done()),{
    shinyjs::disable(selector = 'a[data-value="tab4"]')
    shinyjs::runjs("document.querySelector('a[data-value=\"tab4\"]').classList.add('disabled-tab');")
  })

  session$onSessionEnded(function() { stopApp() })

}

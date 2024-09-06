#' pathway_parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pathway_parameters_ui <- function(id){
  ns <- NS(id)
  tagList(
    mainPanel(
      fluidRow(
        column(11,
               br(),
               uiOutput(ns("help_data"))
        )),
      br(),
      sidebarPanel(width=12,
                   numericInput(ns("n_iter"), "Number of iterations",
                                value=1000,
                                min=1, step = 100, width = "25%")
      ),
      br(),
      uiOutput(ns("pars")),
      actionButton(ns("dist_done"), "Done", class="enable"),
      shinyjs::disabled(actionButton(ns("go_results"), "Results >>"))
    )
  )
}

#' pathway_parameters Server Functions
#'
#' @noRd
mod_pathway_parameters_server <- function(id, ntrade_data, nuts, values, model_done,
                                          parameters){
  n_iter <- NULL
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$pars <- renderUI({
      pn <- parameters()
      n <- length(pn)
      sI <- lapply(1:n, function(i) {
        tagList(
          h4(withMathJax(pn[i]), style = "border-bottom:2px solid grey"),
          fluidRow(
            column(6,
                   selectInput(ns(paste0("dist",i)),
                               "Distribution:",
                               choices = distribution_names,
                               selected = "beta"),
                   distribution_panel(ns, i)
            ),
            column(6,
                   plotOutput(ns(paste0("plot_pars",i)), width = "75%", height = "250px")
            )
          )
        )
      })
    })

    dist_result <- eventReactive(input$dist_done,{
      pn <- parameters()
      parameter_samples <- list()
      for(i in 1:length(pn)){
        d <- input[[paste0("dist",i)]]
        pars <- as.character(input[[paste0("par_",d,"_",i)]])
        parameter_samples[[i]] <- n_from_dist(d, pars, input$n_iter)
      }
      names(parameter_samples) <- pn
      parameter_samples
    })

    observeEvent(input$dist_done,{
      parameter_samples <- dist_result()
      n <- length(parameter_samples)

      lapply(1:n, function(i){
        plotname <- paste0("plot_pars",i)
        d <- input[[paste0("dist",i)]]
        pars <- as.character(input[[paste0("par_",d,"_",i)]])
        output[[plotname]] <- renderPlot({
          hist(parameter_samples[[i]], main = paste0(d,"(",pars,")"), xlab = "", prob = T)
        })
      })
    })
    
    go_results_state <- reactiveValues(is_enabled = FALSE)
    
    observeEvent(input$dist_done,{
      shinyjs::enable("go_results")
      addClass("go_results", class="enable")
      go_results_state$is_enabled <- TRUE
    })

    observeEvent(c(ntrade_data(), nuts(), values(), model_done()),{
      shinyjs::disable("go_results")
      removeClass("go_results", class="enable")
      go_results_state$is_enabled <- FALSE
    })
    
    output$help_data <- renderUI({
      if(go_results_state$is_enabled){
        text_parametersDone
      }else{
        text_parameters
      }
    })

    return(
      list(
        dist_done = reactive(input$dist_done),
        n_iter = reactive(input$n_iter),
        go_results = reactive(input$go_results),
        dist_result = dist_result
      )
    )
  })
}

## To be copied in the UI
# mod_pathway_parameters_ui("pathway_parameters_1")

## To be copied in the server
# mod_pathway_parameters_server("pathway_parameters_1")

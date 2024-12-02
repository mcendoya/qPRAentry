#' pathway_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pathway_model_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      # help text
      fluidRow(
        column(11,
               br(),
               uiOutput(ns("help_data")),
               br()
        )),
      br(),
      sidebarPanel(width = 12,
                   h3("Model parameters", style = "color:#327FB0"),
                   fluidRow(
                     column(6, 
                            style = "background-color: white; margin-right: 50px;",
                            h4(strong("Default parameters:")),
                            column(4,
                                   br(),
                                   checkboxGroupInput(
                                     ns("parameters"), label=NULL,
                                     choiceNames = lapply(
                                       1:length(default_parameters),
                                       function(x) HTML(paste0(
                                         '<span style="font-size:17px;">',
                                         default_parameters_names[x],
                                         '</span><br><br>'))),
                                     choiceValues = default_parameters,
                                     selected = default_parameters)
                            ),
                            column(8,
                                   br(),
                                   lapply(seq_along(default_parameters), function(i){
                                     fluidRow(
                                       column(6,
                                              style = "margin-left: -50px;",
                                              shinyWidgets::pickerInput(
                                                inputId = ns(paste0("symbol_", i)),
                                                label = HTML("in the equation as &nbsp;"),
                                                choices = c("+","-","*","/"),
                                                selected = "*",
                                                multiple = FALSE,
                                                width ="fit")
                                       ),
                                       column(6,
                                              style = "margin-left: -40px;",
                                              HTML(paste0('<span style="font-size:17px;">$', 
                                                          default_parameters[i], 
                                                          '$</span><br>')))
                                     )
                                   })
                            )
                     ),
                     column(5, style = "background-color: white;",
                            h4(strong("Add other parameters:")),
                            br(),       
                            fluidRow(class="inline",
                                     numericInput(
                                       ns("extra_parameters"),
                                       label = "Number of parameters to add:",
                                       value = 0, min = 0, step = 1)
                            ),
                            br(),
                            uiOutput(ns("par_dynamic"))
                     )
                   ),
                   br(),
                   fluidRow(
                     column(11, style = "background-color: white; margin: 20px;",
                            h4(strong("Pathway model:")),
                            uiOutput(ns("pathway_model")),
                            div(class="warn",
                                verbatimTextOutput(ns("message"))
                            )
                     )
                   ),
                   br(),
                   actionButton(ns("model_done"), "Done", class="enable",
                                style='width:100px; font-size:17px')
      )#sidebarPanel
    )
  )
}

#' pathway_model Server Functions
#'
#' @noRd
mod_pathway_model_server <- function(id){
  NUTS_ID <- NULL
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$help_data <- renderUI({
      if(input$model_done==0){
        text_pathwaymodel
      }else{
        text_model_done
      }
    })
    
    # Name extra parameters
    output$par_dynamic <- renderUI({
      if(input$extra_parameters>0){
        LL <- vector("list",input$extra_parameters)
        for(i in 1:input$extra_parameters){
          LL[[i]] <- list(
            fluidRow(
              column(4, class="inline",
                     textInput(inputId = ns(paste0("p",i)),
                               paste0("p", i, " Name"),
                               value = paste0("p_", i),
                               width="100%")
              ),
              
              column(4, style='margin-left:-10px;',
                     shinyWidgets::pickerInput(
                       inputId = ns(paste0("symbol_p_", i)),
                       label = HTML("In the equation as &nbsp;"),
                       choices = c("+","-","*","/"),
                       selected = "*",
                       multiple = FALSE,
                       width ="fit")
              ),
              column(4, style='margin-left:-30px;',
                     textInput(inputId = ns(paste0("p_eq",i)),
                               label = NULL,
                               value = paste0("p_", i),
                               width="100%")
              )
            )
          )
        }
        return(LL)
      }
    })
    
    # Model equation
    model_def <- eventReactive(input$model_done,{
      output$message <- renderText({NULL})
      wrong_pars <- c()
      model <- "NPFP = N_{trade}"
      selected_parameters <- input$parameters
      if (is.null(selected_parameters)){
        model <- "NPFP = N_{trade}"
      }
      for (param in selected_parameters) {
        param_index <- which(default_parameters == param)
        symbol <- input[[paste0("symbol_", param_index)]]
        if (!is.null(symbol)) {
          model <- paste0(model, " ", symbol, " ", param)
        }
      }
      if(input$extra_parameters>0){
        for(i in 1:input$extra_parameters){
          par_names <- input[[paste0("p_eq", i)]]
          if(!grepl(input[[paste0("p", i)]], par_names, fixed = TRUE)){
            wrong_pars <- c(wrong_pars, i)
          }
          symbol_p <- input[[paste0("symbol_p_", i)]]
          model <- paste0(model, " ", symbol_p, " ", par_names)
        }
      }
      tryCatch({
      if(input$extra_parameters>0 & length(wrong_pars>0)){
        stop(paste(
        paste(strwrap("Error: The parameter name in the equation does not 
                           exactly match the one entered under 'Parameter Name' 
                           in the parameters:"), collapse=" "), 
        paste(wrong_pars, collapse = ", ")
      ))
      }
      return(model)
      }, error = function(e) {
        output$message <- renderText({e$message})
        return(NULL)
      })
    })
    
    model_eq <- reactiveVal(
      "NPFP = N_{trade} * (1/U_{weight}) * P_{prevalence} * (1 - P_{sorting}) * 
      (1 - RRO_{effectiveness})"
    )
    
    observeEvent(input$model_done,{
      model_eq(model_def())
    })
    
    # Print model
    output$pathway_model <- renderUI({
      withMathJax(paste0("$$", model_eq(),"$$"))
    })
    
    observeEvent(input$parameters,{
      if(!"(1/U_{weight})"%in%input$parameters){
        showNotification(
          withMathJax("A a parameter must be added to convert units from $N_{trade}$
                       (weight units) to $NPFP$ (number of potential founder populations)"),
          type = "warning")
      }
    })
    
    get_parameters <- eventReactive(input$model_done,{
      par_names <- default_parameters_names[default_parameters %in% input$parameters]
      if(input$extra_parameters>0){
        extra_par <- c()
        for(i in 1:input$extra_parameters){
          extra_par[i] <- paste0("$", input[[paste0("p", i)]], "$")
        }
        par_names <- c(par_names, extra_par)
      }
      
      return(par_names)
    })
    
    return(
      list(
        model_done = reactive(input$model_done),
        parameters = get_parameters,
        model_def = model_def
      )
    )
  })
}

## To be copied in the UI
# mod_pathway_model_ui("pathway_model_1")

## To be copied in the server
# mod_pathway_model_server("pathway_model_1")

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
               br(),
               div(class="warn",
                   verbatimTextOutput(ns("message"))
               )
        )),
      br(),
      tagList(sidebarLayout(
             sidebarPanel(width = 6,
                          h3("$N_{trade}$ data", style = "color:#327FB0"),
                          fileInput(ns("ntrade_data"),
                                    p("$N_{trade}$ data file (CSV):"),
                                    accept = c('.csv'),
                                    width = "50%"),
                          h4("Column names:", style = "color:#327FB0"),
                          fluidRow(
                            column(9,
                            column(6,
                                   shinyWidgets::pickerInput(
                                     inputId = ns("nuts"),
                                     label = "NUTS CODES:",
                                     choices = c("Data must be uploaded"),
                                     multiple = FALSE,
                                     width ="fit")
                                   ),
                            column(6,
                                   shinyWidgets::pickerInput(
                                     inputId = ns("values"),
                                     label = "Ntrade Values:",
                                     choices = c("Data must be uploaded"),
                                     multiple = FALSE,
                                     width ="fit")
                                   )
                            )
                          )
             ),#sidebarPanel
             mainPanel(width=6, 
                       fluidRow(
                         div(class="table-container", style="height:300px;",
                             DT::dataTableOutput(ns("data_table"))
                         )
                       )
             )
        )),
      
             sidebarPanel(width = 12,
                          h3("Model parameters", style = "color:#327FB0"),
                          fluidRow(
                            column(6, style = "background-color: white; margin-right: 50px;",
                                   h4(strong("Default parameters:")),
                              column(4,
                                     br(),
                                     checkboxGroupInput(ns("parameters"), label=NULL,
                                                        choiceNames =lapply(1:length(default_parameters),
                                                                            function(x) HTML(paste0('<span style="font-size:17px;">',
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
                                              HTML(paste0('<span style="font-size:17px;">$', default_parameters[i], '$</span><br>')))
                                       )
                                     })
                                     )
                            ),
                          column(5, style = "background-color: white;",
                                 h4(strong("Add other parameters:")),
                          br(),       
                          fluidRow(class="inline",
                                   numericInput(ns("extra_parameters"),
                                                label = "Number of parameters to add:",
                                                value = 0)
                          ),
                          br(),
                          uiOutput(ns("par_dynamic"))
                          )
                          ),
                          br(),
                          fluidRow(
                            column(11, style = "background-color: white; margin: 20px;",
                                   h4(strong("Pathway model:")),
                                   uiOutput(ns("pathway_model"))
                                   )
                          ),
                          br(),
                          shinyjs::disabled(actionButton(ns("model_done"), "Done")),
                          shinyjs::disabled(actionButton(ns("go_parameters"), "Parameters >>"))
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

    session$userData$ntrade_reactive <- eventReactive(input$ntrade_data,{
      output$message <- renderText({NULL})
      df <- tryCatch({read_file(input$ntrade_data$datapath)
      }, error = function(e) {
        output$message <- renderText({e$message})
        return(NULL)
      })
      return(df)
    })
    
    # Column names
    observeEvent(session$userData$ntrade_reactive(),{
      df <- session$userData$ntrade_reactive()
      updatePickerInput(session = session,
                        inputId = "nuts",
                        selected = character(0),
                        choices = sort(colnames(df)))
      updatePickerInput(session = session,
                        inputId = "values",
                        selected = character(0),
                        choices = sort(colnames(df)))
    })
    
    data_complete <- reactiveVal(FALSE)
    observe({
      if (!is.null(input$ntrade_data)) {
        nuts_not_empty <- !is.null(input$nuts) && input$nuts != ""
        values_not_empty <- !is.null(input$values) && input$values != ""
        nuts_valid <- input$nuts != "Data must be uploaded"
        values_valid <- input$values != "Data must be uploaded"
        
        if (nuts_not_empty && values_not_empty && nuts_valid && values_valid) {
          data_complete(TRUE)
        } else {
          data_complete(FALSE)
        }
      } else {
        data_complete(FALSE)
      }
    })
    
    observeEvent(data_complete(),{
      if(data_complete()){
        shinyjs::enable("model_done")
        addClass("model_done", class="enable")
        shinyjs::enable("go_parameters")
        addClass("go_parameters", class="enable")
      }
    })
    
    #data_errors
    data_message <- function(df){
      m <- c()
      nuts <- unique(nchar(df$NUTS0))
      if(length(nuts)>1){
        "Error"
      }
      if(nuts==2){
        if(!all(df$NUTS_ID %in% NUTS_CODES$CNTR_CODE)){
          "Error"
        }
      }else if(nuts == 4){
        if(!all(df$NUTS_ID %in% NUTS_CODES$NUTS2_CODE)){
          "Error"
        }
      }else{
        "Error"
      }
      if(!is.numeric(df$value)){
        m <- c(m, data_errors$values_num)
      }
      if(any(df$value[!is.na(df$value)]<0)){
        m <- c(m, data_errors$values_neg)
      }
      if(length(m)>0){
        mss <- paste(m, collapse = "\n")
      }else{
        mss <- NULL
      }
      return(mss)
    }
    
    # read data and rename colnames
    ntrade_df <- eventReactive(c(session$userData$ntrade_reactive(),input$nuts,input$values),{
      if(data_complete()){
        tryCatch({
      df <- session$userData$ntrade_reactive()
      user_list <- c(NUTS_ID = input$nuts,
                     values = input$values)
      df <- df %>%
        select(c(input$nuts, input$values)) %>%
        rename(all_of(user_list))
      #data_errors
      m <- data_message(df)
      if (!is.null(m)) { stop(m) }
      df <- df %>%
        group_by(NUTS_ID) %>%
        summarise(values = sum(values, na.rm = TRUE))
      return(df)
        }, error = function(e) {
          output$message <- renderText({e$message})
          return(NULL)
        })
      }
    })
    
    output$help_data <- renderUI({
      if(!data_complete()){
        text_ntrade_data
      }else if(input$model_done==0){
        text_pathwaymodel
      }else{HTML('<p class="custom-text">Note: If you make any changes in trade data
        (new data, columns) or in the pathway model, you must press <strong>Done</strong> again
        to apply the changes.<br></p>')}
    })
    
    output$data_table <- DT::renderDataTable({
      req(!is.null(input$ntrade_data))
      df <- read_file(input$ntrade_data$datapath)
      numeric_columns <- names(df)[which(sapply(df, is.numeric))]
      DT::datatable(df, options = list(dom = 't', pageLength = -1)) %>%
        DT::formatRound(columns = numeric_columns, digits=2)
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
                               h5(paste0("p", i," name")),
                               value = paste0("p_", i),
                               width="100%")
              ),
              
              column(4, style='margin-left:-10px;',
                     shinyWidgets::pickerInput(
                       inputId = ns(paste0("symbol_p_", i)),
                       label = HTML("in the equation as &nbsp;"),
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
      model <- "N_{inf} = N_{trade}"
      selected_parameters <- input$parameters
      if (is.null(selected_parameters)){
        model <- "N_{inf} = N_{trade}"
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
          symbol_p <- input[[paste0("symbol_p_", i)]]
          model <- paste0(model, " ", symbol, " ", par_names)
        }
      }
      return(model)
    })

    model_eq <- reactiveVal(
      "N_{inf} = N_{trade} * (1/U_{weight}) * P_{prevalence} * (1 - P_{sorting}) * (1 - RRO_{effectiveness})"
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
                       (weight units) to $N_{inf}$ (number of potential founder populations)"),
          type = "warning")
      }
    })

    get_parameters <- eventReactive(input$model_done,{
      par_names <- default_parameters_names[default_parameters %in% input$parameters]
      if(input$extra_parameters>0){
        extra_par <- c()
        for(i in 1:input$extra_parameters){
          extra_par[i] <- reactiveValuesToList(input)[paste0("p", i)]
        }
        par_names <- c(par_names, extra_par)
      }
      
      return(unlist(par_names))
    })

    return(
      list(
        ntrade_data = reactive(input$ntrade_data),
        nuts = reactive(input$nuts),
        values = reactive(input$values),
        model_done = reactive(input$model_done),
        parameters = get_parameters,
        model_def = model_def,
        ntrade_df = ntrade_df
      )
    )
  })
}

## To be copied in the UI
# mod_pathway_model_ui("pathway_model_1")

## To be copied in the server
# mod_pathway_model_server("pathway_model_1")

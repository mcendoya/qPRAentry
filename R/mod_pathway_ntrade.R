#' pathway_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pathway_ntrade_ui <- function(id){
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
                     h4("$N_{trade}$ data file (CSV)", style = "color:#327FB0"),
                     fileInput(ns("ntrade_data"),
                               label=NULL,
                               accept = c('.csv'),
                               width = "50%"),
                     h4("Column names:", style = "color:#327FB0"),
                     fluidRow(
                       column(9,
                              column(6,
                                     shinyWidgets::pickerInput(
                                       inputId = ns("nuts"),
                                       label = "NUTS codes:",
                                       choices = c("Data must be uploaded"),
                                       multiple = FALSE,
                                       width ="fit")
                              ),
                              column(6,
                                     shinyWidgets::pickerInput(
                                       inputId = ns("values"),
                                       label = "Values:",
                                       choices = c("Data must be uploaded"),
                                       multiple = FALSE,
                                       width ="fit")
                              )
                       )
                     ),
                     br(),
                     shinyjs::disabled(actionButton(ns("data_done"), "Done", 
                                                    style='width:100px; font-size:17px'))
        ),#sidebarPanel
        mainPanel(width=6, 
                  fluidRow(
                    div(class="table-container", style="height:300px;",
                        DT::dataTableOutput(ns("data_table"))
                    )
                  )
        )
      ))
    )
  )
}

#' pathway_model Server Functions
#'
#' @noRd
mod_pathway_ntrade_server <- function(id){
  NUTS_ID <- NULL
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    session$userData$ntrade_reactive <- eventReactive(input$ntrade_data,{
      output$message <- renderText({NULL})
      df <- tryCatch({load_csv(input$ntrade_data$datapath)
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
        shinyjs::enable("data_done")
        addClass("data_done", class="enable")
      }
    })
    
    #data_errors
    data_message <- function(df){
      m <- c()
      nuts <- unique(nchar(df$NUTS_ID))
      if((length(nuts) > 1 || (nuts != 2 && nuts != 4))){
        m <- c(m, data_ntrade_errors$nuts)
      }else{
        if(nuts==2){
          if(!all(df$NUTS_ID %in% NUTS_CODES$CNTR_CODE)){
            m <- c(m, data_ntrade_errors$nuts)
          }
        }else if(nuts == 4){
          if(!all(df$NUTS_ID %in% NUTS_CODES$NUTS2_CODE)){
            m <- c(m, data_ntrade_errors$nuts)
          }
        } 
      }
      if(!is.numeric(df$value)){
        m <- c(m, data_ntrade_errors$values_num)
      }
      if(any(df$value[!is.na(df$value)]<0)){
        m <- c(m, data_ntrade_errors$values_neg)
      }
      if(length(m)>0){
        mss <- paste(m, collapse = "\n")
      }else{
        mss <- NULL
      }
      return(mss)
    }
    
    ntrade_df <- eventReactive(input$data_done,{
      output$message <- renderText({NULL})
      if(data_complete()){
        tryCatch({
          df <- session$userData$ntrade_reactive()
          user_list <- c(NUTS_ID = input$nuts,
                         values = input$values)
          df <- df %>%
            select(c(input$nuts, input$values)) %>%
            rename(all_of(user_list))
          # data_errors
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
    
    observeEvent(input$data_done,{
      if(is.null(ntrade_df())){
        runjs("window.scrollTo({ top: 0, behavior: 'smooth' });")
      }
    })
    
    output$help_data <- renderUI({
      if(!data_complete()){
        text_ntrade_data
      }else{
        text_data_done
      }
    })
    
    output$data_table <- DT::renderDataTable({
      req(!is.null(input$ntrade_data))
      df <- load_csv(input$ntrade_data$datapath)
      numeric_columns <- names(df)[which(sapply(df, is.numeric))]
      DT::datatable(df, options = list(dom = 't', pageLength = -1)) %>%
        DT::formatRound(columns = numeric_columns, digits=2)
    })
    
  
    return(
      list(
        ntrade_data = reactive(input$ntrade_data),
        nuts = reactive(input$nuts),
        values = reactive(input$values),
        data_done = reactive(input$data_done),
        ntrade_df = ntrade_df
      )
    )
  })
}

## To be copied in the UI
# mod_pathway_ntrade_ui("pathway_ntrade_1")

## To be copied in the server
# mod_pathway_ntrade_server("pathway_ntrade_1")

#' ntrade_redistribution UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ntrade_redistribution_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width=3,
                   style='background: #ffff;',
                   radioButtons(ns("output_NUTS2"), 
                                "Data for proportional redistribution to NUTS2",
                                choices=c("Human population", "Custom Data"),
                                selected=character(0),
                                inline=T),
                   conditionalPanel(condition="input.output_NUTS2 == 'Human population'",
                                    ns = ns,
                                    shinyWidgets::pickerInput(
                                      ns("population_year"),
                                      "Human population data year(s)",
                                      choices = c("Downloading human population data..."),
                                      multiple = TRUE,
                                      selected = character(0),
                                      width ="fit"),
                                    uiOutput(ns("notification_ui"))
                   ),
                   conditionalPanel(condition="input.output_NUTS2 == 'Custom Data'",
                                    ns = ns,
                                    fileInput(
                                      inputId = ns("NUTS2_proportion"),
                                      label = NULL,
                                      width = "70%",
                                      accept = c('.csv')
                                    ),
                                    h4("Column names:", style = "color:#327FB0"),
                                    shinyWidgets::pickerInput(
                                      inputId = ns("colname_NUTS2"),
                                      label = "NUTS2:",
                                      choices = c("Data must be uploaded"),
                                      multiple = FALSE,
                                      width ="fit"
                                    ),
                                    shinyWidgets::pickerInput(
                                      inputId = ns("colname_values"),
                                      label = "Values:",
                                      choices = c("Data must be uploaded"),
                                      multiple = FALSE,
                                      width ="fit"
                                    )
                   ),#conditionalPanel
                   br(),
                   shinyjs::disabled(actionButton(ns("redistribution_done"), 
                                                  "See $N_{trade}$ redistribution")),
                   shinyjs::disabled(downloadButton(ns("downloadAll"), 
                                                    "Download results"))
      ), #sidebarPanel
      mainPanel(width=9,
                #help text
                fluidRow(
                  column(11,
                         div(class="warn",
                             verbatimTextOutput(ns("message"))
                         ),
                         br(),
                         uiOutput(ns("help_data")),
                         br()
                  )),
                uiOutput(ns("NUTS2_results")) 
      )
    )#sidebarLayout
  )
}

#' ntrade_redistribution Server Functions
#'
#' @noRd
#'
mod_ntrade_redistribution_server <- function(id, nuts_yr, Nt, time_period, units){
  CNTR_NAME <- NUTS2 <- NUTS2_CODE <- NUTS_ID <- Median <- CNTR_CODE <- NULL
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    button_pressed <- reactiveVal(FALSE)
    observeEvent(input$redistribution_done, {
      button_pressed(TRUE)
    })
    output$help_data <- renderUI({
      button_state <- button_pressed()
      if (button_state) {
        HTML('<p class="custom-text">Note: If you make any changes to the redistribution 
        data, please, press on <strong style="color: #1E68BA;">See <i>N<sub>trade</sub></i> 
        redistribution</strong> to apply the changes.<br><br>
        <i class="fa-solid fa-star" style="color: #63E6BE;"></i> Click on the 
        <strong style="color: #1E68BA;">Download results</strong> button to proceed to download the 
        <i>N<sub>trade</sub></i> data at NUTS0 and NUTS2 level and the final report.
        <br><br>
        You can also return to the <strong style="color: #1E68BA;">Data</strong> 
        tab to review or change the input data.<br></p>')
      } else {
        if(is.null(input$output_NUTS2)){
          text_DataRedistribution
        }else if(input$output_NUTS2 == "Human population"){
          text_PopulationYear
        }else if(input$output_NUTS2 == "Custom Data"){
          text_MyData
        }
      }
    })
    
    observeEvent(input$output_NUTS2,{
      button_pressed(FALSE)
    })

    all_done <- reactiveVal(FALSE)
    observe({
      if(!is.null(input$output_NUTS2)){
        if(input$output_NUTS2 == "Human population" &&
           !is.null(input$population_year)){
          all_done(TRUE)
        }else if(input$output_NUTS2 == "Custom Data" && 
                 !is.null(input$NUTS2_proportion) &&
                 !is.null(input$colname_NUTS2) &&
                 !is.null(input$colname_values)){
          all_done(TRUE)
        }
      }else{
        all_done(FALSE)
      }
    })
    
    observeEvent(input$output_NUTS2,{
      all_done(FALSE)
    })
 
    observeEvent(all_done(), {
      if (all_done()) {
        shinyjs::enable("redistribution_done")
        addClass("redistribution_done", class = "enable")
        shinyjs::enable("downloadAll")
        addClass("downloadAll", class = "enable")
      } else {
        shinyjs::disable("redistribution_done")
        removeClass("redistribution_done", class = "enable")
        shinyjs::disable("downloadAll")
        removeClass("downloadAll", class = "enable")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$output_NUTS2,{
      if(input$output_NUTS2=="Human population"){

        withProgress(message = 'Downloading human population data...', value = 0, {
          for (i in 1:5) {
            Sys.sleep(0.5) 
            incProgress(1/5)
          }

        df <- cached_get_eurostat_data(nuts_level = 2) 
          
        shinyWidgets::updatePickerInput(session = session,
                                        inputId = "population_year",
                                        choices = sort(unique(df$TIME_PERIOD)),
                                        selected = character(0))
        })#withProgress
      }
    })

    observeEvent(input$NUTS2_proportion,{
      df <- load_csv(input$NUTS2_proportion$datapath)
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "colname_NUTS2",
                                      choices = sort(colnames(df)))
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "colname_values",
                                      choices = sort(colnames(df)))
    }, ignoreInit = TRUE)

    
    Nt_redist <- eventReactive(input$redistribution_done,{
      output$message <- renderText({NULL})
      Nt <- Nt()
      if(input$output_NUTS2=="Human population"){
        redist_data  <-  "population"
        redist_nuts_col <- NULL
        redist_values_col <- NULL
        tp <- input$population_year
      }else if(input$output_NUTS2=="Custom Data"){
        redist_data <- load_csv(input$NUTS2_proportion$datapath)
        redist_nuts_col <- input$colname_NUTS2
        redist_values_col <- input$colname_values
        tp <- NULL
      }
      if(length(time_period())>1){
        nt_values <- c("Q0.05", "Median", "Q0.95")
      }else{
        nt_values <- paste0("Ntrade_",time_period())
      }
      tryCatch({
        Nt_r <- redist_nuts(
          data = Nt,
          nuts_col = "NUTS0",
          values_col = nt_values,
          to_nuts = 2,
          redist_data = redist_data,
          redist_nuts_col = redist_nuts_col,
          redist_values_col = redist_values_col,
          population_year = tp,
          nuts_year = nuts_yr()
        )
        
        Nt_r <- Nt_r %>% 
          left_join(select(Nt, NUTS0, CNTR_NAME), by="NUTS0") %>% 
          select(!NUTS0) %>% 
          relocate(NUTS2, CNTR_NAME)

        return(Nt_r)
      },error = function(e) {
        output$message <- renderText({e$message})
        return(NULL)
      })
    })

    observeEvent(input$redistribution_done,{
      output$NUTS2_results <- renderUI({
        tagList(
          HTML('<p class="custom-text"><br>View <i>N<sub>trade</sub></i> 
               redistribution results in table or map format.<br></p>'),
          shinyWidgets::radioGroupButtons(
            inputId = ns("NUTS2_btn"),
            label = NULL,
            choices = c("Table", "Map"),
            justified = TRUE,
            selected = "Table",
            width = "90%"
          ),
          uiOutput(ns("NUTS2_content"))
        )
      })
    })
    
    # EU NUTS2 map (from giscoR pkg)
    EU02 <- eventReactive(input$redistribution_done,{
      NUTS2map <- cached_get_EUmap(nuts_yr(), nuts=2) %>% 
        st_crop(xmin=-40,ymin=20,xmax=50,ymax=70)
      NUTS2map
    })
    
    # Results redistribution
     observeEvent(input$NUTS2_btn,{
      req(input$NUTS2_btn)
      if(input$NUTS2_btn=="Table"){
        output$NUTS2_content <- renderUI({
          fluidRow(
            div(class="table-container",
                  DT::dataTableOutput(ns("NUTS2_table")) %>% 
                  shinycssloaders::withSpinner(type=5, color = "#327FB0", size=0.8)
            )
          )
        })
      }else if(input$NUTS2_btn=="Map"){
        output$NUTS2_content <- renderUI({
          fluidRow(
            column(6,
                   HTML('<p class="custom-text">Place your cursor over the map to 
                        display the values. Click on a country to zoom in for a 
                        closer view.<br></p>'),
                   br(),
                   ggiraph::girafeOutput(ns("NUTS2map")) %>%
                     shinycssloaders::withSpinner(type=5, color = "#327FB0", size=0.8)
            ),
            column(6,
                   br(),br(),
                   ggiraph::girafeOutput(ns("NUTS2map_zoom"))
            )
          )
        })
      }
    })

    output$NUTS2_table <- DT::renderDataTable({
      numeric_columns <- names(Nt_redist())[which(sapply(Nt_redist(), is.numeric))]
      DT::datatable(Nt_redist(), options = list(dom = 'ft', pageLength = -1)) %>%
        DT::formatRound(columns = numeric_columns, digits=2) %>%
        DT::formatStyle(columns = "NUTS2", target = "cell", 
                        backgroundColor = "#F7080880") %>%
        DT::formatStyle(columns = numeric_columns, target = "cell", 
                        backgroundColor = "#F7080820")
    })

    EU02_dataplot <- eventReactive(input$redistribution_done,{
      Nt2 <- Nt_redist()
      EU02 <- EU02() %>%
        left_join(select(Nt2, !CNTR_NAME), by=join_by(NUTS_ID==NUTS2))
    })

    output$NUTS2map <- ggiraph::renderGirafe({
      EU02 <- EU02_dataplot()
      if(length(time_period())==1){
        EU02 <- EU02 %>% 
          rename(Ntrade_NUTS2 = !!paste0("Ntrade_",time_period()))
        tooltip <- paste0(EU02$NUTS_ID, 
                          "\nNtrade: ", round(EU02$Ntrade_NUTS2,2))
        title <- bquote(paste(N[trade], " ","NUTS2 ", .(time_period())))
      }else{
        EU02 <- EU02 %>% 
          rename(Ntrade_NUTS2 = Median)
        tooltip <- paste0(EU02$NUTS_ID, 
                          "\nQ0.05: ", round(EU02$Q0.05,2),
                          "\nMedian: ", round(EU02$Ntrade_NUTS2,2),
                          "\nQ0.95: ", round(EU02$Q0.95,2))
        title <- expression(paste(N[trade], " ", "NUTS2 - Median"))
      }
    limits <- c(min(EU02$Ntrade_NUTS2, na.rm=T), max(EU02$Ntrade_NUTS2, na.rm=T))
    ggiraph_plot(data = EU02, value = "Ntrade_NUTS2",
                 name = units(), 
                 title = title,
                 limits = limits,
                 tooltip = tooltip,
                 data_id=EU02$CNTR_CODE)
    })

    # reactive to change plot based on selected countries
    selected_NUTS0 <- reactiveVal()
    observeEvent(input$NUTS2map_selected,{
      selected_NUTS0(input$NUTS2map_selected)
    })

    #event_data
    observeEvent(input$NUTS2map_selected,{
      output$NUTS2map_zoom <- ggiraph::renderGirafe({
        idx <- selected_NUTS0()
        country <- EU02_dataplot() %>%
          filter(CNTR_CODE %in% idx)
        c_name <- unique(country$CNTR_NAME)
        EU02 <- EU02_dataplot()
        if(length(time_period())==1){
          country <- country %>% 
            rename(Ntrade_NUTS2 = !!paste0("Ntrade_",time_period()))
          tooltip <- paste0(country$NUTS_ID, 
                            "\nNtrade: ", round(country$Ntrade_NUTS2,2))
        }else{
          country <- country %>% 
            rename(Ntrade_NUTS2 = Median)
          tooltip <- paste0(country$NUTS_ID,
                            "\nQ0.05: ", round(country$Q0.05,2),
                            "\nMedian: ", round(country$Ntrade_NUTS2,2),
                            "\nQ0.95: ", round(country$Q0.95,2))
        }
        limits <- c(min(country$Ntrade_NUTS2, na.rm=T), 
                    max(country$Ntrade_NUTS2, na.rm=T))
        ggiraph_plot(data = country, value = "Ntrade_NUTS2",
                     name = units(), 
                     title = bquote(paste(N[trade]," ", .(idx)," "," - ", .(c_name))),
                     limits = limits,
                     tooltip = tooltip,
                     ii=5)
      })
    })

    # Download report and results files
 
    output$downloadAll <- downloadHandler(
      filename = function() {
        paste("Ntrade_results", Sys.Date(), ".zip", sep = "")
      },
      content = function(fname) {
        withProgress(message = 'Preparing download files...', value = 0, {
          for (i in 1:5) {
            Sys.sleep(0.5) 
            incProgress(1/5)
          }
        # temporary directory before processing
        userDir <- getwd()
        tempDir <- tempdir()
        setwd(tempDir)
        # PDF report
        tempReport <- file.path(tempDir, "Ntrade_report.html")
        rmdPath <- system.file("ShinyFiles", "Ntrade_report.Rmd", 
                               package = "qPRAentry")
        file.copy(rmdPath, tempReport, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        params <- list(time_period = time_period(),
                       units = units(),
                       nuts_yr = nuts_yr(),
                       Nt_result = Nt(),
                       Nt_redist = Nt_redist(),
                       data_redistribution = input$output_NUTS2,
                       population_year = input$population_year)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(input = rmdPath,
                          output_file = tempReport,
                          params = params,
                          envir = new.env(parent = globalenv()))
        
        # CSV files
        tempCsv <- file.path(tempDir, "Ntrade_NUTS0.csv")
        write.csv(Nt(), tempCsv, row.names = FALSE)
        
        tempCsv2 <- file.path(tempDir, "Ntrade_NUTS2.csv")
        write.csv(Nt_redist(), tempCsv2, row.names = FALSE)
        
        # Create ZIP file
        fs <- c("Ntrade_report.html", "Ntrade_NUTS0.csv", "Ntrade_NUTS2.csv")
        utils::zip(zipfile = fname, files = fs)
        setwd(userDir)
        
        }) #withProgress
      },
      contentType = "application/zip"
    )

  })
}

## To be copied in the UI
# mod_ntrade_redistribution_ui("ntrade_redistribution_1")

## To be copied in the server
# mod_ntrade_redistribution_server("ntrade_redistribution_1")

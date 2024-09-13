#' pathway_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pathway_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(1),
      column(10,
             HTML('<p class="custom-text"><br>View the pathway model results (<i>N<sub>inf</sub></i>) in table or map format.<br><br> 
 <i class="fa-solid fa-star" style="color: #63E6BE;"></i> Click on the <strong>"Download results"</strong> 
        button to download a zip folder including the <i>N<sub>inf</sub></i> data and the final report.<br><br>
                  You can also return to the "Pathway model" or "Parameters" tabs to review or 
                  change the input data.<br></p>'),
             br(),
             downloadButton(ns("downloadAll"), "Download results", class="enable"),
             br(),br(),
             shinyWidgets::radioGroupButtons(
               inputId = ns("Ninf_btn"),
               label = NULL,
               choices = c("Table", "Map"),
               justified = TRUE,
               selected = "Table"
             ),
             uiOutput(ns("Ninf_results"))
      ),
      column(1)
    )
  )
}

#' pathway_results Server Functions
#'
#' @noRd
mod_pathway_results_server <- function(id, dist_done, n_iter, model_def,
                                       param_names, par_settings, dist_result, 
                                       ntrade_df){
  NUTS_ID <- value <- CNTR_CODE <- NULL
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    model_solve <- eventReactive(dist_done(),{
      n_iter <- n_iter()
      equation <- model_def()
      sym_sub <- c("N_\\{inf\\} = N_\\{trade\\} \\* ", "\\{", "\\}", "_", "\\$")
      for(i in 1:length(sym_sub)){
        equation <- gsub(sym_sub[i], "", equation)
      }
      parameter_samples <- dist_result()
      for(i in 1:length(sym_sub)){
        names(parameter_samples) <- gsub(sym_sub[i], "", names(parameter_samples))
      }
      mat_samp <- as.matrix(do.call(cbind, parameter_samples))
      # Generic function to evaluate the equation in each matrix row
      eval_equation <- function(row, equation) {
        variables <- names(row)
        for (i in seq_along(variables)) {
          assign(variables[i], row[i])
        }
        eval(parse(text = equation))
      }
      res_samp <- apply(mat_samp, 1, function(row) eval_equation(row, equation))
      names(res_samp) <- paste0("res_", 1:n_iter)
      res_samp
    })

    Ninf_solve <- eventReactive(dist_done(),{
      n_iter <- n_iter()
      res_samp <- model_solve()
      Nt_df <- ntrade_df()
      fns <- c(mean,
               sd,
               partial(quantile, probs = 0.05, na.rm=TRUE),
               median,
               partial(quantile, probs = 0.95, na.rm=TRUE))
      fns_names <- c("Mean", "SD", "Q0.05", "Median", "Q0.95")
      apply_functions <- function(...) {
        values <- c(...)
        setNames(map(fns, ~.x(values)), fns_names)
      }
      # Ntrade * res_samp and summarise
      res_df <- map2(res_samp, 1:n_iter,
                     function(x, i){
                       res <- Nt_df$values * x
                       setNames(res, paste0("res", i))
                     }) %>%
        bind_cols() %>%
        pmap_dfr(apply_functions) %>%
        bind_cols(select(Nt_df, NUTS_ID)) %>%
        relocate(NUTS_ID)
      res_df
    })

    Ninf_samples <- eventReactive(dist_done(),{
      n_iter <- n_iter()
      res_samp <- model_solve()
      Nt_df <- ntrade_df()
      fns <- c(mean,
               sd,
               partial(quantile, probs = 0.05, na.rm=TRUE),
               median,
               partial(quantile, probs = 0.95, na.rm=TRUE))
      res_df <- map2(res_samp, 1:n_iter,
                     function(x, i){
                       res <- Nt_df$values * x
                       setNames(res, paste0("res", i))
                     }) %>%
        bind_cols() %>%
        bind_cols(select(Nt_df, NUTS_ID)) %>%
        relocate(NUTS_ID)
      res_df
    })

    Ninf_EU <- eventReactive(dist_done(),{
      df <- Ninf_samples() %>%
        summarise(across(starts_with("res"), sum)) %>%
        pivot_longer(everything()) %>%
        summarise(Mean = mean(value),
                  SD = sd(value),
                  Q0.05 = quantile(value, probs = 0.05, na.rm=TRUE),
                  Median = median(value),
                  Q0.95 = quantile(value, probs = 0.95, na.rm=TRUE))
      df
    })

    nuts_level <- eventReactive(dist_done(),{
      nuts <- nchar(ntrade_df()$NUTS_ID[1])
      nuts_level <- if(nuts==2){
        0
      }else if(nuts==4){
        2
      }
      return(nuts_level)
    })

    # EU NUTS0 map (from giscoR pkg)
    EU00 <- eventReactive(dist_done(),{
      map <- suppressWarnings(
        suppressMessages(
          giscoR::gisco_get_nuts(nuts_level = nuts_level()) %>%
            st_crop(xmin=-40,ymin=20,xmax=50,ymax=70)
        ))
      return(map)
    })

    observe({
      if(input$Ninf_btn=="Table"){
        output$Ninf_results <- renderUI({
          fluidRow(width = 11,
                   column(12, align="center", style='height:400px; overflow-y: scroll;',
                          DT::dataTableOutput(ns("Ninf_table")) %>%
                            shinycssloaders::withSpinner(type=5, color = "#327FB0", size=0.8)
                   ),
                   column(12, style='margin-top:10em;',
                          p("Total EU:", style="font-weight: bold;"),
                          DT::dataTableOutput(ns("NinfEU_table")),
                   )
          )
        })
      }else if(input$Ninf_btn=="Map"){
        output$Ninf_results <- renderUI({
          #If NUTS2
          if(nuts_level()==2){
            fluidRow(
              column(6,
                     HTML('<p class="custom-text">Place your cursor over the map to display the values. 
                 Click on a country to zoom in for a closer view.<br></p>'),
                     br(),
                     ggiraph::girafeOutput(ns("NUTSmap")) %>%
                       shinycssloaders::withSpinner(type=5, color = "#327FB0", size=0.8)
              ),
              column(6,
                     div(textOutput(ns("ClickOnMap")), style = "color:grey;"),
                     br(),br(),
                     ggiraph::girafeOutput(ns("NUTSmap_zoom"))
              )
            )
          }else{
            fluidRow(width = 11,
                     HTML('<p class="custom-text">Place your cursor over the map to display the values.<br></p>'),
                     br(),
                     ggiraph::girafeOutput(ns("NUTSmap")) %>%
                       shinycssloaders::withSpinner(type=5, color = "#327FB0", size=0.8)
            )
          }
        })
      }
    })

    output$Ninf_table <- DT::renderDataTable({
      DT::datatable(Ninf_solve(), options = list(dom = 'ft', pageLength = -1)) %>%
        DT::formatRound(columns = 2:length(Ninf_solve()), digits=2) %>%
        DT::formatStyle(columns = "NUTS_ID", target = "cell", backgroundColor = "#F7080880") %>%
        DT::formatStyle(columns = c("Mean","SD","Q0.05","Median","Q0.95"), target = "cell", backgroundColor = "#F7080820")
    })

    output$NinfEU_table <- DT::renderDataTable({
      DT::datatable(Ninf_EU(), rownames = c("EU"),
                    options = list(dom = 't', pageLength = -1)) %>%
        DT::formatRound(columns = 1:length(Ninf_EU()), digits=2)
    })

    output$NUTSmap <- ggiraph::renderGirafe({
      Ninf <- Ninf_solve()
      limits <- c(min(Ninf$Mean, na.rm=T), max(Ninf$Mean, na.rm=T))
      EU00 <- EU00() %>%
        left_join(Ninf, by=join_by(NUTS_ID==NUTS_ID))
      ggiraph_plot(data = EU00, value = "Mean",
                   name = expression(N[inf]),
                   title = expression(paste(N[inf], " ", "Mean")),
                   limits = limits,
                   tooltip = paste(EU00$NUTS_ID,
                                   "\nMean: ", round(EU00$Mean,2),
                                   "\nSD: ", round(EU00$SD, 2)),
                   data_id=EU00$CNTR_CODE)
    })

    observeEvent(Ninf_solve(),{
      if(length(Ninf_solve())>0){
        output$ClickOnMap <- renderText({
          "Click on a country to zoom in"
        })
      }
    })

    # reactive to change plot based on selected MS
    selected_NUTS0 <- reactiveVal()
    observeEvent(input$NUTSmap_selected,{
      selected_NUTS0(input$NUTSmap_selected)
    })

    # event_data
    observeEvent(input$NUTSmap_selected,{
      output$NUTSmap_zoom <- ggiraph::renderGirafe({
        idx <- selected_NUTS0()
        Ninf <- Ninf_solve()
        country <- EU00() %>%
          left_join(Ninf, by=join_by(NUTS_ID==NUTS_ID)) %>%
          filter(CNTR_CODE %in% idx)

        limits <- c(min(country$Mean, na.rm=T), max(country$Mean, na.rm=T))

        ggiraph_plot(data = country, value = "Mean",
                     name = expression(N[inf]),
                     title = bquote(paste(.(idx)," ", "- ", N[inf])),
                     limits = limits,
                     tooltip = paste0(country$NUTS_ID,
                                      "\nMean: ", round(country$Mean,2),
                                      "\nSD: ", round(country$SD, 2)))
      })
    })

    # Download report and results files
    
    output$downloadAll <- downloadHandler(
      filename = function() {
        paste("Pathway_results", Sys.Date(), ".zip", sep = "")
      },
      content = function(fname) {
        # temporary directory before processing
        userDir <- getwd()
        tempDir <- tempdir()
        setwd(tempDir)
        # PDF report
        tempReport <- file.path(tempDir, "pathway_report.pdf")
        rmdPath <- system.file("ShinyFiles", "pathway_report.Rmd", package = "qPRAentry")
        file.copy(rmdPath, tempReport, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        params <- list(ntrade = ntrade_df(),
                       model_def = model_def(),
                       param_names = param_names(),
                       n_iter = n_iter(),
                       par_settings = par_settings(),
                       dist_result = dist_result(),
                       Ninf = Ninf_solve())
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(input = rmdPath,
                          output_file = tempReport,
                          params = params,
                          envir = new.env(parent = globalenv()))
        
        # CSV files
        tempCsv <- file.path(tempDir, "Ninf.csv")
        write.csv(Ninf_solve(), tempCsv, row.names = FALSE)
        
        # Create ZIP file
        fs <- c("pathway_report.pdf", "Ninf.csv")
        utils::zip(zipfile = fname, files = fs)
        setwd(userDir)
      },
      contentType = "application/zip"
    )
    
  })
}

## To be copied in the UI
# mod_pathway_results_ui("pathway_results_1")

## To be copied in the server
# mod_pathway_results_server("pathway_results_1")

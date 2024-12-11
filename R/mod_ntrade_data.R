#' ntrade_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ntrade_data_ui <- function(id){
  ns <- NS(id) 
  tagList(
    sidebarLayout(
      sidebarPanel(width=3,
                   style='background: #ffff;',
                   div(style = "border-bottom:2px solid grey",
                       textInput(ns("units"), label = "$N_{trade}$ units:", 
                                 value = "tons") %>%
                         bsplus::shinyInput_label_embed(
                           bsplus::shiny_iconlink("question-circle", 
                                                  class= "help-btn") %>%
                             bsplus::bs_embed_popover(title = text_units$title, 
                                                      content = text_units$content,
                                                      placement = "right",
                                                      html="true",
                                                      container ="body")
                         ),
                       # years available in giscoR::gisco_get_nuts()
                       selectInput(ns("nuts_yr"), 
                                    label = "NUTS classification year:", 
                                    selected = "2016",
                                   choices = c("2003", 
                                               "2006", 
                                               "2010", 
                                               "2013", 
                                               "2016", 
                                               "2021",
                                               "2024")) %>%
                         bsplus::shinyInput_label_embed(
                           bsplus::shiny_iconlink("question-circle", 
                                                  class= "help-btn") %>%
                             bsplus::bs_embed_popover(title = text_nuts_yr$title, 
                                                      content = text_nuts_yr$content,
                                                      placement = "right",
                                                      html="true",
                                                      container ="body")
                         )
                   ),
                   div(style = "border-bottom:2px solid grey",
                       br(),
                       strong("Trade data:"),
                       # Extra
                       data_input(ns, "ExtraTotal", 
                                  HTML("<i style='font-size:18px;'>
                                       ExtraTotal Import</i>"), 
                                  extra=TRUE),
                       br(),
                       data_input(ns, "ExtraPest",
                                  HTML("<i style='font-size:18px;'>
                                       ExtraPest Import</i>"),
                                  extra=TRUE),
                       br(),
                       # Intra
                       data_input(ns, "Intra", 
                                  HTML("<i style='font-size:18px;'>
                                       Intra Trade</i>")),
                       br(),
                       #Internal production
                       data_input(ns, "IP", 
                                  HTML("<i style='font-size:18px;'>
                                       Internal Production</i>")
                                  , partner=FALSE),
                       br()
                   ),
                   br(),
                   # Time period
                   shinyWidgets::pickerInput(
                     inputId = ns("time_period"),
                     label = "Time period:",
                     choices = c("Data must be uploaded"),
                     selected = c("Data must be uploaded"),
                     multiple = TRUE,
                     options = list(`actions-box` = TRUE)
                   ) %>% 
                     bsplus::shinyInput_label_embed(
                       bsplus::shiny_iconlink("question-circle", 
                                              class= "help-btn") %>%
                         bsplus::bs_embed_popover(title = text_time$title, 
                                                  content = text_time$content,
                                                  placement = "right",
                                                  html="true",
                                                  container ="body")
                     ),
                   br(),
                   shinyjs::disabled(actionButton(ns("trade_done"), 
                                                  "See $N_{trade}$ results"))
      ), #sidebarPanel
      mainPanel(width=9,
                fluidRow(
                  column(11,
                         div(class="warn",
                             verbatimTextOutput(ns("message"))
                         ),
                         br(),
                         uiOutput(ns("help_data")),
                         br()
                  )),
                # Plot buttons
                uiOutput(ns("plot_buttons")),
                # Plots
                fluidRow(
                  div(class = "dual-plot-container",
                      div(class = "dual-plot-column-large", 
                          ggiraph::girafeOutput(ns("dataPlot"))),
                      div(class = "dual-plot-column-small", 
                          plotOutput(ns("MSplot")))
                  )
                )
      )#mainPanel
    )#sidebarLayout
  )#tagList
}

#' ntrade_data Server Functions
#'
#' @noRd
mod_ntrade_data_server <- function(id){
  partner <- value <- NULL
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # NUTS codes
    NUTS_CODES <- eventReactive(input$nuts_yr,{
      NUTS0_CODES <- cached_get_EUmap(year = input$nuts_yr, nuts=0) %>%
        st_drop_geometry()
      return(NUTS0_CODES)
    })
   
     # data
    session$userData$ExtraTotal_reactive <- eventReactive(input$ExtraTotal,{
      output$message <- renderText({NULL})
      df <- tryCatch({
        load_csv(input$ExtraTotal$datapath)
      }, error = function(e) {
        output$message <- renderText({e$message})
        return(NULL)
      })
      return(df)
    })
    session$userData$ExtraPest_reactive <- eventReactive(input$ExtraPest,{
      output$message <- renderText({NULL})
      df <- tryCatch({
        load_csv(input$ExtraPest$datapath)
      }, error = function(e) {
        output$message <- renderText({e$message})
        return(NULL)
      })
      return(df)
    })
    session$userData$Intra_reactive <- eventReactive(input$Intra,{
      output$message <- renderText({NULL})
      df <- tryCatch({
        load_csv(input$Intra$datapath)
      }, error = function(e) {
        output$message <- renderText({e$message})
        return(NULL)
      })
      return(df)
    })
    session$userData$IP_reactive <- eventReactive(input$IP,{
      output$message <- renderText({NULL})
      df <- tryCatch({
        load_csv(input$IP$datapath)
      }, error = function(e) {
        output$message <- renderText({e$message})
        return(NULL)
      })
      return(df)
    })
    
    # Column names
    observeEvent(session$userData$ExtraTotal_reactive(),{
      colnames_select(session$userData$ExtraTotal_reactive(), 
                      "ExtraTotal", partner=TRUE, session=session)
    })
    observeEvent(session$userData$ExtraPest_reactive(),{
      colnames_select(session$userData$ExtraPest_reactive(), 
                      "ExtraPest", partner=TRUE, session=session)
    })
    observeEvent(session$userData$Intra_reactive(),{
      colnames_select(session$userData$Intra_reactive(), 
                      "Intra", partner=TRUE, session=session)
    })
    observeEvent(session$userData$IP_reactive(),{
      colnames_select(session$userData$IP_reactive(), 
                      "IP", partner=FALSE, session=session)
    })
    
    # Extra Partners List
    observeEvent(c(session$userData$ExtraTotal_reactive(), input$partner_ExtraTotal),{
      df <- session$userData$ExtraTotal_reactive()
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "extra_partner_ExtraTotal",
                                      choices = unique(df[,input$partner_ExtraTotal]))
    }, ignoreInit = TRUE)
    
    observeEvent(c(session$userData$ExtraPest_reactive(), input$partner_ExtraPest),{
      df <- session$userData$ExtraPest_reactive()
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "extra_partner_ExtraPest",
                                      choices = unique(df[,input$partner_ExtraPest]))
    }, ignoreInit = TRUE)
    
    
    # Units
    output$unitsOutputExtraTotal <- renderText({
      paste0("= ", input$units)
    })
    output$unitsOutputExtraPest <- renderText({
      paste0("= ", input$units)
    })
    output$unitsOutputIntra <- renderText({
      paste0("= ", input$units)
    })
    output$unitsOutputIP <- renderText({
      paste0("= ", input$units)
    })
    
    #close dropMenu
    observeEvent(input$done_ExtraTotal,{
      shinyWidgets::hideDropMenu("ExtraTotal_menu_dropmenu")
      if(is.null(ExtraTotal_df())){
        runjs("window.scrollTo({ top: 0, behavior: 'smooth' });")
      }
    })
    observeEvent(input$done_ExtraPest,{
      shinyWidgets::hideDropMenu("ExtraPest_menu_dropmenu")
      if(is.null(ExtraPest_df())){
        runjs("window.scrollTo({ top: 0, behavior: 'smooth' });")
      }
    })
    observeEvent(input$done_Intra,{
      shinyWidgets::hideDropMenu("Intra_menu_dropmenu")
      if(is.null(Intra_df())){
        runjs("window.scrollTo({ top: 0, behavior: 'smooth' });")
      }
    })
    observeEvent(input$done_IP,{
      shinyWidgets::hideDropMenu("IP_menu_dropmenu")
      if(is.null(IP_df())){
        runjs("window.scrollTo({ top: 0, behavior: 'smooth' });")
      }
    })
    
    # fn to rename colnames
    colnames_rename <- function(df, data_name, partner = TRUE){
      user_colnames <- c(input[[paste0("reporter_", data_name)]],
                         input[[paste0("value_", data_name)]],
                         input[[paste0("time_period_", data_name)]])
      user_list <- c(reporter = input[[paste0("reporter_", data_name)]],
                     value = input[[paste0("value_", data_name)]],
                     time_period = input[[paste0("time_period_", data_name)]])
      if(partner){
        user_colnames <- c(user_colnames, input[[paste0("partner_", data_name)]])
        user_list <- c(user_list, partner =  input[[paste0("partner_", data_name)]])
      }
      default_list <- list(reporter = "reporter",
                           partner = "partner",
                           value = "value",
                           time_period = "time_period")
      df <- df %>% 
        select(all_of(user_colnames)) %>% 
        rename(!!!user_list[names(user_list)%in%default_list])
      return(df)
    }
    
    # Check for NULL column names
    columns_null <- function(data_name, partner = TRUE) {
      reporter_input <- input[[paste0("reporter_", data_name)]]
      value_input <- input[[paste0("value_", data_name)]]
      time_period_input <- input[[paste0("time_period_", data_name)]]
      partner_input <- if (partner) input[[paste0("partner_", data_name)]] else NULL
      missing_cols <- character(0)  # Initialize as empty character vector
      if (is.null(reporter_input)) {
        missing_cols <- c(missing_cols, "reporter")
      }
      if (partner && is.null(partner_input)) {
        missing_cols <- c(missing_cols, "partner")
      }
      if (is.null(value_input)) {
        missing_cols <- c(missing_cols, "value")
      }
      if (is.null(time_period_input)) {
        missing_cols <- c(missing_cols, "time_period")
      }
      if (length(missing_cols) > 0) {
        message <- paste("Error: column names must be selected for", 
                         paste(missing_cols, collapse = ", "))
        return(message)
      } else {
        return(NULL)
      }
    }
    
    #data_errors
    data_message <- function(df, NUTS_CODES,
                             partner=FALSE, extra_partner=FALSE, 
                             input_parner=NULL){
      m <- c()
      if(!all(df$reporter %in% NUTS_CODES$CNTR_CODE)){
        m <- c(m, data_errors$reporter)
      }
      if(partner && !all(df$partner %in% NUTS_CODES$CNTR_CODE)){
        m <- c(m, data_errors$partner)
      }
      if(!is.numeric(df$value)){
        m <- c(m, data_errors$values_num)
      }
      if(any(df$value[!is.na(df$value)]<0)){
        m <- c(m, data_errors$values_neg)
      }
      if(extra_partner && is.null(input_parner)){
        m <- c(m, data_errors$extra_partner)
      }
      if(length(m)>0){
        mss <- paste(m, collapse = "\n")
      }else{
        mss <- NULL
      }
      return(mss)
    }
    
    # read data and rename colnames
    ExtraTotal_df <- eventReactive(input$done_ExtraTotal,{
      NUTS_CODES <- NUTS_CODES()
      tryCatch({
        df <- session$userData$ExtraTotal_reactive()
        m <- columns_null("ExtraTotal")
        if (!is.null(m)) { stop(m) }
        df <- colnames_rename(df, "ExtraTotal") %>% 
          {if(any(.$reporter %in% NUTS_CODES$CNTR_CODE)) {
            mutate(., reporter = case_when(
              reporter == "GR" ~ "EL",
              reporter == "GB" ~ "UK",
              TRUE ~ reporter
            ))
          } else {
            .
          }}
        # data errors
        m <- data_message(df, NUTS_CODES, extra_partner = TRUE,
                          input_parner = input$extra_partner_ExtraTotal)
        if (!is.null(m)) { stop(m) }
        df <- df %>%
          filter(partner %in% input$extra_partner_ExtraTotal) %>%
          mutate(value = value * input$unitsExtraTotal)
        class(df$time_period) <- class(input$time_period)
        return(df)
      }, error = function(e) {
        output$message <- renderText({e$message})
        return(NULL)
      })
    })
    
    ExtraPest_df <- eventReactive(input$done_ExtraPest,{
      NUTS_CODES <- NUTS_CODES()
      tryCatch({
        df <- session$userData$ExtraPest_reactive()
        m <- columns_null("ExtraPest")
        if (!is.null(m)) { stop(m) }
        df <- colnames_rename(df, "ExtraPest") %>% 
          {if(any(.$reporter %in% NUTS_CODES$CNTR_CODE)) {
            mutate(., reporter = case_when(
              reporter == "GR" ~ "EL",
              reporter == "GB" ~ "UK",
              TRUE ~ reporter
            ))
          } else {
            .
          }}
        # data errors
        m <- data_message(df, NUTS_CODES,
                          extra_partner = TRUE,
                          input_parner = input$extra_partner_ExtraPest)
        if (!is.null(m)) { stop(m) }
        df <- df %>%
          filter(partner %in% input$extra_partner_ExtraPest) %>%
          mutate(value = value * input$unitsExtraPest)
        class(df$time_period) <- class(input$time_period)
        return(df)
      }, error = function(e) {
        output$message <- renderText({e$message})
        return(NULL)
      })
    })
    
    Intra_df <- eventReactive(input$done_Intra,{
      NUTS_CODES <- NUTS_CODES()
      tryCatch({
        df <- session$userData$Intra_reactive()
        m <- columns_null("Intra")
        if (!is.null(m)) { stop(m) }
        df <- colnames_rename(df, "Intra") %>% 
          {if(any(.$reporter %in% NUTS_CODES$CNTR_CODE)) {
            mutate(., reporter = case_when(
              reporter == "GR" ~ "EL",
              reporter == "GB" ~ "UK",
              TRUE ~ reporter
            ))
          } else {
            .
          }} %>% 
          {if(any(.$partner %in% NUTS_CODES$CNTR_CODE)) {
            mutate(., partner = case_when(
              partner == "GR" ~ "EL",
              partner == "GB" ~ "UK",
              TRUE ~ partner
            ))
          } else {
            .
          }}
        
        #data errors
        m <- data_message(df, NUTS_CODES, partner=TRUE)
        if (!is.null(m)) { stop(m) }
        df <- df %>%
          mutate(value = value * input$unitsIntra)
        class(df$time_period) <- class(input$time_period)
        return(df)
      }, error = function(e) {
        output$message <- renderText({e$message})
        return(NULL)
      })
    })
    
    IP_df <- eventReactive(input$done_IP,{
      NUTS_CODES <- NUTS_CODES()
      tryCatch({
        df <- session$userData$IP_reactive()
        m <- columns_null("IP", partner = FALSE)
        if (!is.null(m)) { stop(m) }
        df <- colnames_rename(df, "IP", partner=FALSE) %>% 
          {if(any(.$reporter %in% NUTS_CODES$CNTR_CODE)) {
            mutate(., reporter = case_when(
              reporter == "GR" ~ "EL",
              reporter == "GB" ~ "UK",
              TRUE ~ reporter
            ))
          } else {
            .
          }}
        #data_errors
        m <- data_message(df, NUTS_CODES)
        if (!is.null(m)) { stop(m) }
        df <- df %>%
          mutate(value = value * input$unitsIP)
        class(df$time_period) <- class(input$time_period)
        return(df)
      }, error = function(e) {
        output$message <- renderText({e$message})
        return(NULL)
      })
    })
    
    is_initial <- reactiveVal(TRUE)
    observe({
      # Update is_initial
      if (!is.null(ExtraTotal_df())||!is.null(ExtraPest_df())||!is.null(Intra_df())||!is.null(IP_df())){
        is_initial(FALSE)
      }
    })
    output$help_data <- renderUI({
      if(is_initial()){
        text_trade_data("ExtraTotal")
      }else if(is.null(input$ExtraTotal$datapath)||input$done_ExtraTotal==0||is.null(ExtraTotal_df())){
        text_trade_data("ExtraTotal")
      }else if(is.null(input$ExtraPest$datapath)||input$done_ExtraPest==0||is.null(ExtraPest_df())){
        text_trade_data("ExtraPest")
      }else if(is.null(input$Intra$datapath)||input$done_Intra==0||is.null(Intra_df())){
        text_trade_data("Intra", partner=FALSE)
      }else if(is.null(input$IP$datapath)||input$done_IP==0||is.null(IP_df())){
        text_trade_data("IP", partner=FALSE)
      }else{
        text_dataDone
      }
    })
    
    all_time_periods <- reactiveValues()
    
    # when Done
    observeEvent(input$done_ExtraTotal, {
      df <- ExtraTotal_df()
      if (is.null(df)) {
        updateActionButton(session, "ExtraTotal_menu",
                           label= '<p><strong style="color:#327FB0; font-size:18px;">
                           <i>ExtraTotal Import</i></strong></p>') #remove check icon
        return(NULL)  # Do nothing if there is an error
      } else {
        updateActionButton(session, "ExtraTotal_menu",
                           label= '<p><strong style="color:#327FB0; font-size:18px;">
                           <i>ExtraTotal Import</i></strong>&nbsp;&nbsp;
                           <i class="fa-solid fa-circle-check" style="color: #63E6BE;">
                           </i></p>') #check icon
        output$message <- renderText({NULL})  # Clear message
        all_time_periods$ExtraTotal <- unique(df$time_period)
      }
    })
    observeEvent(input$done_ExtraPest, {
      df <- ExtraPest_df()
      if (is.null(df)) {
        updateActionButton(session, "ExtraPest_menu",
                           label= '<p><strong style="color:#327FB0; font-size:18px;">
                           <i>ExtraPest Import</i></strong></p>')#remove check icon
        return(NULL)  # Do nothing if there is an error
      } else {
        updateActionButton(session, "ExtraPest_menu",
                           label= '<p><strong style="color:#327FB0; font-size:18px;">
                           <i>ExtraPest Import</i></strong>&nbsp;&nbsp;
                           <i class="fa-solid fa-circle-check" style="color: #63E6BE;">
                           </i></p>')#check icon
        output$message <- renderText({NULL})  # Clear message
        all_time_periods$ExtraPest <- unique(df$time_period)
      }
    })
    observeEvent(input$done_Intra, {
      df <- Intra_df()
      if (is.null(df)) {
        updateActionButton(session, "Intra_menu",
                           label= '<p><strong style="color:#327FB0; font-size:18px;">
                           <i>Intra Trade</i></strong></p>')#remove check icon
        return(NULL)  # Do nothing if there is an error
      } else {
        updateActionButton(session, "Intra_menu",
                           label= '<p><strong style="color:#327FB0; font-size:18px;">
                           <i>Intra Trade</i></strong>&nbsp;&nbsp;
                           <i class="fa-solid fa-circle-check" style="color: #63E6BE;">
                           </i></p>')#check icon
        output$message <- renderText({NULL})  # Clear message
        all_time_periods$Intra <- unique(df$time_period)
      }
    })
    observeEvent(input$done_IP, {
      df <- IP_df()
      if (is.null(df)) {
        updateActionButton(session, "IP_menu",
                           label= '<p><strong style="color:#327FB0; font-size:18px;">
                           <i>Internal Production</i></strong></p>')#remove check icon
        return(NULL)  # Do nothing if there is an error
      } else {
        updateActionButton(session, "IP_menu",
                           label= '<p><strong style="color:#327FB0; font-size:18px;">
                           <i>Internal Production</i></strong>&nbsp;&nbsp;
                           <i class="fa-solid fa-circle-check" style="color: #63E6BE;">
                           </i></p>')#check icon
        output$message <- renderText({NULL})  # Clear message
        all_time_periods$IP <- unique(df$time_period)
      }
    })
    
    all_btns <- reactiveVal("")
    observe({
      if(input$done_ExtraTotal &
         input$done_ExtraPest &
         input$done_Intra &
         input$done_IP){
        all_btns("all")
      }
    })
    
    # Time periods
    observeEvent(reactiveValuesToList(all_time_periods),{
      if(all_btns()=="all"){
        output$message <- renderText({NULL})
        list_periods <- list(
          all_time_periods$ExtraTotal,
          all_time_periods$ExtraPest,
          all_time_periods$Intra,
          all_time_periods$IP
        )
        common_periods <- Reduce(intersect, list_periods)
        tryCatch({
          if (length(common_periods) > 0) {
            updatePickerInput(session = session,
                              inputId = "time_period",
                              choices = sort(common_periods),
                              selected = sort(common_periods))
            all_btns("OK")
          } else {
            all_btns("all")
            runjs("window.scrollTo({ top: 0, behavior: 'smooth' });")
            stop(paste(strwrap("Error: No common time periods found across the data. 
                               Please verify the 'Time period' column in the uploaded 
                               data."), collapse = " "))
            
          }
        }, error = function(e) {
          output$message <- renderText({e$message})
        })
      }
    })
    
    # Enable plot buttons
    observe({
      if(all_btns()=="OK"){
        shinyjs::enable("trade_done")
        addClass("trade_done", class="enable")
        
        output$plot_buttons <- renderUI({
          sidebarPanel(width = 11,
                       HTML('<p class="custom-text">Use the buttons <strong style="color: #1E68BA;">
                            Plot Extra Import</strong>, <strong style="color: #1E68BA;">
                            Plot Intra Trade</strong>, or <strong style="color: #1E68BA;">
                            Plot Internal Production</strong> to change the trade data 
                            visualisation.<br> 
                            Place your cursor over the bars to to view mean and 
                            standard deviation for each country.<br> 
                            Click on the bars to view the values for each country 
                            over time.<br></p>'),
                       fluidRow(
                         column(4, align = "center",
                                actionButton(ns("ExtraTotal_plot"),
                                             HTML("Plot</br>Extra Import"),
                                             class="enable")
                         ),
                         column(4, align = "center",
                                actionButton(ns("Intra_plot"),
                                             HTML("Plot</br> Intra Trade"),
                                             class="enable")
                         ),
                         column(4, align = "center",
                                actionButton(ns("IP_plot"),
                                             HTML("Plot</br> Internal Production"),
                                             class="enable")
                         )
                       )
          )
        })
      }else{
        shinyjs::disable("trade_done")
        removeClass("trade_done", class="enable")
        output$plot_buttons <- renderUI({NULL})
      }
    })
    
    # trade data
    TradeData <- eventReactive(c(input$done_ExtraTotal, input$done_ExtraPest, 
                                 input$done_Intra, input$done_IP, input$time_period),{
                                   if(all_btns()=="OK"){
                                     tryCatch({
                                       withCallingHandlers({
                                         shinyjs::html("message", "")
                                         df <- trade_data(extra_total = ExtraTotal_df(),
                                                          extra_pest = ExtraPest_df(),
                                                          intra_trade = Intra_df(),
                                                          internal_production = IP_df(),
                                                          filter_period = input$time_period)
                                       },
                                       message = function(m) {
                                         shinyjs::html(id = "message", 
                                                       html = m$message, add = TRUE)
                                       })
                                       return(df)
                                     },error = function(e) {
                                       output$message <- renderText({e$message})
                                       return(NULL)
                                     })
                                   }
                                 })
    
    # Data plot when press buttons
    data_plot <- reactiveVal()
    
    observe_plot_event <- function(input_id) {
      observeEvent(input[[paste0(input_id, "_plot")]], {
        data_plot(input_id)
      })
    }
    
    # change data_plot reactive
    observe_plot_event("ExtraTotal")
    observe_plot_event("Intra")
    observe_plot_event("IP")
    
    output$dataPlot <- ggiraph::renderGirafe({
      req(length(data_plot())>0)
      df <- TradeData()$total_trade
      timePeriod <- input$time_period
      yLab <- paste0("Quantity (", input$units, ")")
      if(data_plot()=="ExtraTotal"){
        pl <- plot_dataUpload(df = df,
                              dfName = "ExtraTotal",
                              timePeriod, yLab,
                              plotTitle = "Extra Import",
                              legendTitle = "Third countries:")
      }else if(data_plot()=="Intra"){
        pl <- plot_dataUpload(df = df,
                              dfName = "Intra",
                              timePeriod, yLab,
                              plotTitle = "Intra Trade",
                              legendTitle = "Trade:")
      }else if(data_plot()=="IP"){
        pl <- plot_dataUpload(df = df,
                              dfName = "IP",
                              timePeriod, yLab,
                              plotTitle = "Internal production")
      }
      ggiraph::girafe(ggobj=pl, options = list(
        opts_hover(css = "stroke-width:1;"),
        opts_selection(only_shiny = FALSE, type = "single", css = "stroke:white;")
      ))
    })
    
    # reactive to change plot based on selected MS
    selected_MS <- reactiveVal()
    observeEvent(input$dataPlot_selected,{
      selected_MS(input$dataPlot_selected)
    })
    
    #event_data
    observeEvent(input$dataPlot_selected,{
      output$MSplot <- renderPlot({
        
        df <- TradeData()$total_trade
        idx <- selected_MS()
        yLab <- paste0("Quantity (", input$units, ")")
        if(data_plot()=="ExtraTotal"){
          pl <- plot_byCountry(df = df,
                               dfName = "ExtraTotal",
                               idx,
                               yLab,
                               plotTitle = "Extra Tmport",
                               legendTitle = "Third countries:")
        }else if(data_plot()=="Intra"){
          pl <- plot_byCountry(df = df,
                               dfName = "Intra",
                               idx,
                               yLab,
                               plotTitle = "Intra Trade",
                               legendTitle = "Trade:")
        }else if(data_plot()=="IP"){
          pl <- plot_byCountry(df = df,
                               dfName = "IP",
                               idx,
                               yLab,
                               plotTitle = "Internal production")
        }
        pl
      })
    })
    
    return(
      list(
        nuts_yr = reactive(input$nuts_yr),
        trade_done = reactive(input$trade_done),
        time_period = reactive(input$time_period),
        units = reactive(input$units),
        NUTS_CODES = NUTS_CODES,
        TradeData = TradeData
      )
    )
  })
}

## To be copied in the UI
# mod_ntrade_data_ui("ntrade_data_1")

## To be copied in the server
# mod_ntrade_data_server("ntrade_data_1")

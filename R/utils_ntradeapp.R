data_input <- function(ns, data_name, title_data, partner = TRUE, extra=FALSE){
  fluidRow(
    shinyWidgets::dropMenu(
      arrow = FALSE,
      maxWidth = "100%",
      placement = "bottom-start",
      hideOnClick = TRUE,
      actionButton(ns(paste0(data_name, "_menu")),
                   h5(strong(title_data, style = "color:#327FB0")),
                   icon = icon("angle-down"),
                   width = "100%"),
      width = "100%",
      fileInput(ns(data_name),
                "Data file (CSV):",
                accept = c('.csv'),
                width = "100%"),
      fluidRow(
        column(9, class="inline",
               numericInput(ns(paste0("units", data_name)),
                            HTML("Data units &nbsp;&nbsp;&nbsp;<b>X</b>"),
                            value = 1)
        ),
        column(3,  class="unitslab",
               textOutput(ns(paste0("unitsOutput", data_name))))
      ),
      br(),
      h4("Column names:", style = "color:#327FB0"),
      shinyWidgets::pickerInput(
        inputId = ns(paste0("reporter_", data_name)),
        label = "Reporter:",
        choices = c("Data must be uploaded"),
        multiple = FALSE,
        width ="fit"
      ),
      if(partner){
        shinyWidgets::pickerInput(
          inputId = ns(paste0("partner_", data_name)),
          label = "Partner:",
          choices = c("Data must be uploaded"),
          multiple = FALSE,
          width ="fit"
        )
      },
      shinyWidgets::pickerInput(
        inputId = ns(paste0("value_", data_name)),
        label = "Values:",
        choices = c("Data must be uploaded"),
        multiple = FALSE,
        width ="fit"
      ),
      shinyWidgets::pickerInput(
        inputId = ns(paste0("time_period_", data_name)),
        label = "Time period:",
        choices = c("Data must be uploaded"),
        multiple = FALSE,
        width ="fit"
      ),
      if(extra){
        shinyWidgets::pickerInput(
          inputId = ns(paste0("extra_partner_", data_name)),
          label = "Partner countries:",
          choices = c("Data must be uploaded"),
          multiple = TRUE,
          width = "100%",
          options = list(`actions-box` = TRUE)
        )
      },
      br(),
      actionButton(ns(paste0("done_", data_name)), "Done", class="enable")
    )#dropdown
  )
}

colnames_select <- function(df, data_name, partner=TRUE, session = session){
  updatePickerInput(session = session,
                    inputId = paste0("reporter_", data_name),
                    # selected = character(0),
                    selected = "reporter",
                    choices = sort(colnames(df)))
  if(partner){
    updatePickerInput(session = session,
                      inputId = paste0("partner_", data_name),
                      # selected = character(0),
                      selected = "partner",
                      choices = sort(colnames(df)))
  }
  updatePickerInput(session = session,
                    inputId = paste0("value_", data_name),
                    # selected = character(0),
                    selected = "OBS_VALUE",
                    choices = sort(colnames(df)))
  updatePickerInput(session = session,
                    inputId = paste0("time_period_", data_name),
                    # selected = character(0),
                    selected = "TIME_PERIOD",
                    choices = sort(colnames(df)))
}

data_summary <- function(df, group, value){
  df <- df %>%
    group_by(!! sym(group)) %>%
    summarise(Mean = mean(!! sym(value), na.rm=TRUE),
              SD = sd(!! sym(value), na.rm=TRUE)) %>%
    replace(is.na(.), 0)
  df
}

plot_dataUpload <- function(df, dfName, timePeriod, yLab,
                            plotTitle, legendTitle=NULL){
  IDs <- extra_total <- extra_pest <- intra_import <- intra_export <- 
    internal_production <- text1 <- text2 <- Mean <- SD <- Trade <- 
    trade_mean <- trade_sd <- value <- time_period <- NULL
  
  if(dfName == "ExtraTotal"){
    if(length(timePeriod)==1){
      Total <- df %>%
        select(IDs, extra_total) %>%
        mutate(value=extra_total,
               text1 = paste0(IDs, "\nTotal: ", round(extra_total,2)))
      Pest <- df %>%
        select(IDs, extra_pest) %>%
        mutate(value = extra_pest,
               text2 = paste0(IDs, "\nPest present: ", round(value,2)))
      
      pl <- ggplot(NULL, aes(IDs, value)) +
        geom_col_interactive(aes(fill = "Total", tooltip=text1, data_id=IDs), data = Total, alpha = 0.5) +
        geom_col_interactive(aes(fill= "Pest present", tooltip=text2, data_id=IDs), data = Pest, alpha = 0.7)
      
    }else{
      Total <- df %>%
        select(IDs, extra_total) %>%
        data_summary(group="IDs", value="extra_total") %>%
        mutate(text1 = paste0(IDs, " - Total\nMean: ", round(Mean,2),
                              "\nSD: ", round(SD,2)))
      Pest <- df %>%
        select(IDs, extra_pest) %>%
        data_summary(group="IDs", value="extra_pest") %>%
        mutate(text2 = paste0(IDs, " - Pest present\nMean: ", round(Mean,2),
                              "\nSD: ", round(SD,2)))
      pl <- ggplot(NULL, aes(IDs, Mean)) +
        geom_col_interactive(aes(fill = "Total", tooltip=text1, data_id=IDs), data = Total, alpha = 0.5) +
        geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD),
                      data = Total, width=.2, color="#6D6C6C") +
        geom_col_interactive(aes(fill="Pest present", tooltip=text2, data_id=IDs), data = Pest, alpha = 0.7) +
        geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD),
                      data = Pest, width=.2, color="#6D6C6C")
    }
  }else if(dfName == "IntraEU"){
    if(length(timePeriod)==1){
      intraEU <- df %>%
        select(IDs, intra_import, intra_export) %>%
        rename(Import = intra_import,
               Export = intra_export) %>%
        pivot_longer(cols=c('Import', 'Export'),
                     names_to='Trade',
                     values_to='value')
      pl <- ggplot(intraEU, aes(IDs, value, fill=Trade, data_id=IDs))+
        geom_col_interactive(position = "dodge", alpha=0.7,
                             aes(tooltip = paste0(IDs, "\n", Trade, ": ", round(value,2)))) +
        scale_fill_manual(values =c("#009E73", "orange"))
    }else{
      intraEU_mean <- df %>%
        select(IDs, time_period, intra_import, intra_export) %>%
        group_by(IDs) %>%
        summarise(Import = mean(intra_import),
                  Export = mean(intra_export)) %>%
        pivot_longer(cols=c('Import', 'Export'),
                     names_to="Trade",
                     values_to="trade_mean")
      
      intraEU_sd <- df %>%
        select(IDs, time_period, intra_import, intra_export) %>%
        group_by(IDs) %>%
        summarise(Import = sd(intra_import),
                  Export = sd(intra_export)) %>%
        pivot_longer(cols=c('Import', 'Export'),
                     names_to="Trade",
                     values_to="trade_sd")
      
      intraEU <- intraEU_mean %>%
        left_join(intraEU_sd, by = join_by(IDs, Trade)) %>%
        replace(is.na(.), 0)
      
      pl <- ggplot(intraEU, aes(IDs, trade_mean, fill=Trade, data_id=IDs))+
        geom_col_interactive(position = "dodge", alpha=0.7,
                             aes(tooltip = paste0(IDs,
                                                  "\n", Trade, " Mean: ", round(trade_mean,2),
                                                  "\n", Trade, " SD: ", round(trade_sd,2))))+
        geom_errorbar(aes(ymin=trade_mean-trade_sd, ymax=trade_mean+trade_sd),
                      position=position_dodge(.9), width=.3, color="#6D6C6C")+
        scale_fill_manual(values =c("#009E73", "orange"))
    }
    
  }else if(dfName == "IP"){
    if(length(timePeriod)==1){
      pl <- ggplot(df, aes(IDs, internal_production, data_id=IDs)) +
        geom_col_interactive(alpha = 0.7, fill="#CAD100",
                             aes(tooltip = paste0(IDs, ": ", round(internal_production,2)) ))
    }else{
      df <- df %>%
        select(IDs, internal_production, time_period) %>%
        data_summary(group="IDs", value="internal_production")
      pl <- ggplot(df, aes(IDs, Mean, data_id=IDs))+
        geom_col_interactive(position = "dodge", alpha=0.7, fill="#CAD100",
                             aes(tooltip=paste0(IDs, "\nMean: ", round(Mean,2),
                                                "\nSD: ", round(SD,2))))+
        geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD),
                      width=.2, color="#6D6C6C")
    }
  }
  
  if(!is.null(legendTitle)){
    pl <- pl + guides(fill=guide_legend(title=legendTitle))
  }
  pl <- pl +
    xlab("")+
    ylab(yLab)+
    ggtitle(plotTitle)+
    theme(legend.position = "top",
          legend.text = element_text(size=9),
          legend.title = element_text(size=10),
          title = element_text(size=10),
          axis.text = element_text(size=8))
  return(pl)
}

plot_byCountry <- function(df, dfName, idx, yLab, plotTitle, legendTitle=NULL){
  IDs <- extra_total <- extra_pest <- intra_import <- time_period <- value <- 
    intra_export <- Trade <- internal_production <- NULL
  if(dfName=="ExtraTotal"){
    Total <- df %>%
      select(IDs, time_period, extra_total) %>%
      rename(value = extra_total) %>%
      filter(IDs==idx)
    Pest <- df %>%
      select(IDs, time_period, extra_pest) %>%
      rename(value = extra_pest) %>%
      filter(IDs==idx)
    v_max <- max(Total$value, na.rm=T)
    pl <- ggplot(NULL, aes(as.factor(time_period), value)) +
      geom_col(aes(fill = "Total"), data = Total, alpha = 0.5) +
      geom_col(aes(fill="Pest present"), data = Pest, alpha = 0.7)
  }else if(dfName=="IntraEU"){
    intraEU <- df %>%
      select(IDs, time_period, intra_import, intra_export) %>%
      rename(Import = intra_import,
             Export = intra_export) %>%
      filter(IDs==idx) %>%
      pivot_longer(cols=c('Import', 'Export'),
                   names_to='Trade',
                   values_to='value')
    v_max <- max(intraEU$value, na.rm=T)
    pl <- ggplot(intraEU, aes(as.factor(time_period), value, fill=Trade))+
      geom_col(position = "dodge", alpha=0.7) +
      scale_fill_manual(values =c("#009E73", "orange"))
  }else if(dfName=="IP"){
    IP_time <- df %>%
      select(IDs, time_period, internal_production) %>%
      rename(value = internal_production) %>%
      filter(IDs==idx)
    v_max <- max(IP_time$value, na.rm=T)
    pl <- ggplot(IP_time, aes(as.factor(time_period), value))+
      geom_col(fill="#CAD100", alpha=0.7)
  }
  if(!is.null(legendTitle)){
    pl <- pl + guides(fill=guide_legend(title=legendTitle))
  }
  pl <- pl +
    ylim(0, v_max) +
    xlab("Time period") +
    ylab(yLab) +
    ggtitle(paste(plotTitle, idx, sep=" ")) +
    theme(legend.position = "top",
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          title = element_text(size=15),
          axis.text = element_text(size=12))
  
  return(pl)
}
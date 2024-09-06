#' The ntrade application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @noRd
ntradeapp_ui <- function(request){
  tags$div(class="container",
  navbarPage(
    HTML('<strong style="color:#1E68BA;"><i>N<sub>trade</sub></i> application</strong>'), 
    windowTitle = "Ntrade application",
    header = tags$div(
      includeCSS(system.file("www/style.css", package = "qPRAentry")),
      bsplus::use_bs_popover(),
      shinyjs::useShinyjs(),
      withMathJax(),
      tags$div(HTML("<script type='text/x-mathjax-config' >
                 MathJax.Hub.Config({
                    tex2jax: {
                      inlineMath: [['$','$'], ['\\\\(','\\\\)']],
                      processEscapes: true
                    },
                    TeX: {
                      extensions: ['tex2jax.js', 'AMSmath.js', 'AMSsymbols.js']
                    }
                  });
                 </script >")
      )
    ),
      tabPanel("Info ",
               class="intro",
               icon = icon("book-open", "fa-pull-right"),
               includeMarkdown(system.file("ShinyFiles/Info_ntrade.md", package = "qPRAentry"))
      ),
      tabPanel("$N_{trade}$ ",
               icon = icon("chart-line", "fa-pull-right"),
               tabsetPanel(id = "trade_tabs",
                           type = "pills",
                           tabPanel("Data", value = "tab1",
                                    mod_ntrade_data_ui("ntrade_data")),
                           tabPanel("Results", value = "tab2",
                                    mod_ntrade_results_ui("ntrade_results")),
                           tabPanel("Redistribution", value="tab3",
                                    mod_ntrade_redistribution_ui("ntrade_redistribution"))
               )
      )#tabPanel
    )#navbar
  )#div
}
#' The pathway application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @noRd
pathwayapp_ui <- function(request){
  tags$div(class="container",
           navbarPage(
             HTML('<strong style="color:#1E68BA;">Pathway model application</strong>'), 
             windowTitle = "Pathway model application",
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
                      includeMarkdown(system.file("ShinyFiles/Info_pathway.md", package = "qPRAentry"))
             ),
             tabPanel("Pathway model",
                      icon = icon("chart-line", "fa-pull-right"),
                      tabsetPanel(id = "pathway_tabs",
                                  type = "pills",
                                  tabPanel("Pathway model", value = "tab1",
                                           mod_pathway_model_ui("pathway_model")),
                                  tabPanel("Parameters", value="tab2",
                                           mod_pathway_parameters_ui("pathway_parameters")),
                                  tabPanel("Results", value="tab3",
                                           mod_pathway_results_ui("pathway_results"))
                      )
             )
           )
  )
}


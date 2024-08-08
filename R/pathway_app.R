#' Shiny app for pathway model
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pathway_app()
#' }
#' @importFrom shiny runApp shinyApp
pathway_app <- function(){
  app <- shinyApp(
    ui = pathwayapp_ui,
    server = pathwayapp_server
  )
  runApp(app,
         display.mode = "normal",
         launch.browser = TRUE)
}

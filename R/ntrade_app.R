#' Shiny app for Ntrade
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ntrade_app()
#' }
#' @importFrom shiny runApp shinyApp
ntrade_app <- function(){
  app <- shinyApp(
    ui = ntradeapp_ui,
    server = ntradeapp_server
  )
  runApp(app,
         display.mode = "normal",
         launch.browser = TRUE)
}

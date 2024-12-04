#' Shiny app for Ntrade
#' 
#' Interactive application for the calculation and redistribution of the potentially 
#' infected/infested quantity of commodities imported by a country from third countries 
#' where the pest is present (\eqn{N_{trade}}) using the NUTS coding system. See 
#' [Nomenclature of territorial units for statistics](https://ec.europa.eu/eurostat/web/nuts).
#' 
#' @seealso [ntrade()], [redist_nuts()], [redist_iso()]
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

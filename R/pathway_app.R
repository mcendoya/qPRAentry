#' Shiny app for pathway model
#' 
#' Interactive application to estimate the number of potential founder populations (\eqn{NPFP}) 
#' of a pest in different regions or countries using the NUTS 
#' coding system. See 
#' [Nomenclature of territorial units for statistics](https://ec.europa.eu/eurostat/web/nuts).
#' 
#' @seealso [pathway_model()]
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

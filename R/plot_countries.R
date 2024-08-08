#' Plot trade values on a map
#'
#' This function plots country values on a map using data provided and allows customization
#' of the aesthetics such as colors, legend title, and title.
#'
#' @param data A data frame containing the values to be plotted on the map.
#' @param IDs_column Column name in \code{data} with the country codes.
#' CNTR_ID from \code{\link[giscoR]{giscoR}} package .
#' [To be completed]
#' @param values_column Column name in \code{data} with the values to be plotted.
#' @param countrycode Country code in \code{IDs_column}. Possible values are "CNTR_CODE" 
#' (2-letter code of each country), or "ISO3_CODE" (3-letter code of each country). 
#' Default is "CNTR_CODE", for Europe the same as NUTS codes (country level), 
#' United Kingdom as "UK" and Greece as "EL". 
#' See \code{\link[giscoR]{gisco_countrycode}} for details on country codes.
#' @param colors Optional vector of colors used in the gradient scale.
#' @param na.value Color for missing values (default is "grey").
#' @param title A title for the plot (default is NULL).
#' @param legend_title A title for the legend. Default NULL, name in the \code{values_column}.
#'
#' @return A ggplot object with the plotted countries.
#'
#' @examples
#' ## Example with CNTR_CODE (2-letter country code)
#' # Country codes from the giscoR package
#' library(giscoR)
#' IDs  <- gisco_countrycode$CNTR_CODE[gisco_countrycode$continent=="Africa"]
#' # Values simulation
#' df <- data.frame(IDs = IDs,
#'                  value = runif(length(IDs)))
#' # Plot
#' plot_countries(data = df,
#'                IDs_column = "IDs",
#'                values_column = "value",
#'                countrycode = "CNTR_CODE")
#' # Changing colors and adding other ggplot2 options
#' pl <- plot_countries(data = df,
#'                      IDs_column = "IDs",
#'                      values_column = "value",
#'                      countrycode = "CNTR_CODE",
#'                      colors = c("white", "lightblue", "darkblue"))
#' library(ggplot2)
#' pl + 
#'   xlim(-40, 60) + ylim(-40, 40) +
#'   theme_bw()
#' 
#' ## Example with ISO3_CODE (3-letter country code)
#' # Country codes from the giscoR package
#' df_ISO3 <- data.frame(IDs = sample(gisco_countrycode$ISO3_CODE, 20),
#'                       value = runif(20))
#' plot_countries(data = df_ISO3,
#'                IDs_column = "IDs",
#'                values_column = "value",
#'                countrycode = "ISO3_CODE")
#'
#' @export
plot_countries <- function(data, IDs_column, values_column,
                       countrycode = "CNTR_CODE",
                       colors = NULL, na.value = "grey",
                       title=NULL, legend_title=NULL){
  
  if(countrycode == "CNTR_CODE"){
    countrycode <- "CNTR_ID"
  }
  legend_title <- ifelse(is.null(legend_title), values_column, legend_title)
  map <- gisco_get_countries()
  map <- map %>%
    left_join(data, by = join_by(!!countrycode==!!IDs_column)) %>%
    st_as_sf()
  if(is.null(colors)){
    colors <- c('#ffff96', '#e58938', '#a0042a')
  }else{
    colors <- colors
  }
  pl <- ggplot(map) +
    geom_sf(aes(fill=.data[[values_column]]))+
    scale_fill_gradientn(colors = colors,
                         na.value = na.value,
                         name = legend_title) +
    ggtitle(title)
  
  pl
}

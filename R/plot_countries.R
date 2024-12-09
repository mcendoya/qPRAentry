#' Plot values on a map at country level
#'
#' Plots country values on a map using data provided and allows 
#' customisation of various aesthetics, such as colors, legend title, and title. 
#' 
#' Extracts an [sf] object from the [giscoR] package.
#' It uses the [ggplot2] package for the representation. Also, it supports the addition  
#' of other [ggplot2] options (see examples).
#'
#' @param data A data frame containing the values to be plotted on the map.
#' @param iso_col A string specifying the column name in \code{data} 
#' with the ISO 3166-1 (alpha-2) country codes. See 
#' [ISO 3166 Maintenance Agency](https://www.iso.org/iso-3166-country-codes.html) 
#' for details on country codes.
#' @param values_col A string specifying the column name in \code{data} with the 
#' values to be plotted.
#' @param colors Optional vector of colors used in the gradient scale.
#' @param na_value Color for missing values (default is "grey").
#' @param title A title for the plot (default is \code{NULL}).
#' @param legend_title A title for the legend. Default \code{NULL}, name in the 
#' \code{values_col}.
#'
#' @return A \code{ggplot} object with the plotted countries.
#'
#' @examples
#' # Simulated data trade in Northern America
#' data("datatrade_NorthAm")
#' # Mean of internal production for each country
#' library(dplyr)
#' data_plot <- datatrade_NorthAm$internal_production %>% 
#'   group_by(reporter) %>% 
#'   summarise(mean_value = mean(value))
#' 
#' head(data_plot)
#' 
#' #Plot
#' pl <- plot_countries(data = data_plot,
#'                      iso_col = "reporter",
#'                      values_col = "mean_value")
#' pl
#' 
#' # Changing colors and adding other ggplot2 options
#' library(ggplot2)
#' pl <- plot_countries(data = data_plot,
#'                      iso_col = "reporter",
#'                      values_col = "mean_value",
#'                      colors = c("white", "lightblue", "darkblue"),
#'                      title = "Plot internal production",
#'                      legend_title = "units")
#' pl + 
#'   xlim(-170, -20) + ylim(10, 90) +
#'   theme_bw()
#'                
#' @export
#' 
plot_countries <- function(data, iso_col, values_col,
                           colors = NULL, na_value = "grey",
                           title=NULL, legend_title=NULL){
  CNTR_ID <- NULL
  if(any(class(data) == "sf")){
    data <- data %>% st_drop_geometry()
  }
  # check data.frame
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be data.frame.")
  }
  # Check if the specified columns exist in the data frame
  if (!all(c(iso_col, values_col) %in% names(data))) {
    stop(paste(strwrap("The data frame 'data' must contain the columns specified 
                       in 'iso_col' and 'values_col'"), collapse=" "))
  }

  map <- gisco_get_countries() 
  
  # check value numeric
  if (!is.numeric(data[[values_col]])) {
    stop("Error: 'values_col' in 'data' must be numeric.")
  }
  
  if ("GR" %in% unique(data[[iso_col]])) {
    data[[iso_col]][data[[iso_col]] == "GR"] <- "EL"
  }
  if ("GB" %in% unique(data[[iso_col]])) {
    data[[iso_col]][data[[iso_col]] == "GB"] <- "UK"
  }
  # check country codes
  if (!any(data[[iso_col]] %in% map$CNTR_ID)) {
    stop(paste(strwrap("Error: 'iso_col' in 'data' does not contain ISO 3166-1 
                       (alpha-2) country codes."), collapse=" "))
  }

  legend_title <- ifelse(is.null(legend_title), values_col, legend_title)
  map <- map %>%
    left_join(data, by = join_by(CNTR_ID==!!iso_col)) %>%
    st_as_sf()
  if(is.null(colors)){
    colors <- c('#ffff96', '#e58938', '#a0042a')
  }else{
    colors <- colors
  }
  pl <- ggplot(map) +
    geom_sf(aes(fill=.data[[values_col]]))+
    scale_fill_gradientn(colors = colors,
                         na.value = na_value,
                         name = legend_title) +
    ggtitle(title)
  
  pl
}

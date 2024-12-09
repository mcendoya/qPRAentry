#' Plot NUTS region values on a map
#'
#' Plots NUTS region values on a map using the provided data and allows 
#' customisation of various aesthetics, such as colors, legend title, and title.
#' 
#' Extracts an [sf] object from the [giscoR] package.
#' It uses the [ggplot2] package for the representation. Also, it supports the addition  
#' of other [ggplot2] options (see examples).
#'
#' @param data A data frame containing the values to be plotted on the map.
#' @param nuts_col A string specifying the column name in \code{data} containing 
#' NUTS codes.
#' @param values_col A string specifying the column name in \code{data} with the 
#' values to be plotted.
#' @param nuts_level A numeric value (0, 1, 2, or 3) specifying the NUTS level to plot. 
#' Default is 2 indicating NUTS2. See 
#' [Nomenclature of territorial units for statistics](https://ec.europa.eu/eurostat/web/nuts).
#' @param nuts_year Year of NUTS classification. Accepted values are '2003','2006',
#' '2010','2013','2016' (default),'2021', or '2024'. See 
#' [NUTS - History](https://ec.europa.eu/eurostat/web/nuts/history).
#' @param colors Optional vector of colors used in the gradient scale.
#' @param na_value Color for missing values (default is "grey").
#' @param title A title for the plot (default is \code{NULL}).
#' @param legend_title  A title for the legend. Default \code{NULL}, name in the 
#' \code{values_col}.
#'
#' @return A ggplot object with the plotted NUTS regions.
#'
#' @examples
#' ## Example plot at NUTS0 level (country level)
#' # Simulated data trade in European countries
#' data("datatrade_EU")
#' # Mean of internal production for each country
#' library(dplyr)
#' data_plot <- datatrade_EU$internal_production %>% 
#'   group_by(reporter) %>% 
#'   summarise(mean_value = mean(value))
#' 
#' head(data_plot)
#' 
#' #Plot
#' pl <- plot_nuts(data = data_plot,
#'                 nuts_col = "reporter",
#'                 values_col = "mean_value",
#'                 nuts_level = 0)
#' pl
#' 
#' ## Example plot at NUTS1 level (codes extracted from 'giscoR' package)
#' library(dplyr)
#' library(giscoR)
#' data_plot <- gisco_get_nuts(nuts_level=1) %>% 
#'   select(NUTS_ID) %>% 
#'   # simulate values for each NUTS1
#'   mutate(values = abs(rnorm(nrow(.), 0, 1000)))
#' 
#' #Plot
#' pl <- plot_nuts(data = data_plot,
#'                 nuts_col = "NUTS_ID",
#'                 values_col = "values",
#'                 nuts_level = 1,
#'                 colors = c("white", "lightblue", "darkblue"),
#'                 title = "NUTS1",
#'                 legend_title = "units")
#' 
#' # Changing colors and adding other ggplot2 options
#' library(ggplot2)
#' pl + 
#'   xlim(-40, 50) + ylim(20, 70) +
#'   theme_bw()
#' 
#' @export
plot_nuts <- function(data, nuts_col, values_col,
                      nuts_level = 2,
                      nuts_year = "2016",
                      colors=NULL, na_value = "grey",
                      title=NULL, legend_title=NULL){
  if(any(class(data) == "sf")){
    data <- data %>% st_drop_geometry()
  }
  # check data.frame
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be data.frame.")
  }
  # Check if the specified columns exist in the data frame
  if (!all(c(nuts_col, values_col) %in% names(data))) {
    stop(paste(strwrap("The data frame 'data' must contain the columns specified 
                       in 'nuts_col' and 'values_col'"), collapse=" "))
  }
  
  # check nuts year
  if (!nuts_year %in% c('2003','2006','2010','2013','2016','2021','2024')) {
    stop(paste(strwrap("Error: nuts_year not available. Try '2003', '2006', '2010', 
                       '2013', '2016', '2021', or '2024'"), collapse=" "))
  }
  # check nuts_level
  if (!nuts_level %in% c(0, 1, 2, 3) || !is.numeric(nuts_level)) {
    stop("Error: 'nuts_level' must be numeric, 0, 1, 2 or 3 NUTS level.")
  }
  NUTS_CODES <- cached_get_EUmap(year = nuts_year, nuts = nuts_level) 

  # check value numeric
  if (!is.numeric(data[[values_col]])) {
    stop("Error: 'values_col' in 'data' must be numeric.")
  }
 
  if ("GR" %in% unique(data[[nuts_col]])) {
    data[[nuts_col]][data[[nuts_col]] == "GR"] <- "EL"
  }
  if ("GB" %in% unique(data[[nuts_col]])) {
    data[[nuts_col]][data[[nuts_col]] == "GB"] <- "UK"
  }
  # check country codes
  if (!any(data[[nuts_col]] %in% NUTS_CODES$NUTS_ID)) {
    stop("Error: 'nuts_col' in 'data' does not contain NUTS codes.")
  }
  
  NUTS_ID <- NULL
  legend_title <- ifelse(is.null(legend_title), values_col, legend_title)
  map <- NUTS_CODES %>%
    left_join(data, by = join_by(NUTS_ID == !!nuts_col)) %>%
    st_as_sf()
  if(is.null(colors)){
    colors <- c('#ffff96', '#e58938', '#a0042a')
  }else{
    colors <- colors
  }
  pl <- ggplot() +
    geom_sf(data = map, aes(fill=.data[[values_col]]))+
    scale_fill_gradientn(colors=colors,
                         na.value = na_value,
                         name= legend_title) +
    ggtitle(title)

  pl
}

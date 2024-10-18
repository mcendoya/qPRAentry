#' Plot NUTS2 region values on a map
#'
#' This function plots NUTS2 region values on a map using data provided and allows customization
#' of the aesthetics such as colors, legend title, and title.
#'
#' @param data A data frame containing the values to be plotted on the map.
#' @param nuts_column Column name in \code{data} containing NUTS2 codes.
#' @param values_column Column name in \code{data} with the values to be plotted.
#' @param colors Optional vector of colors used in the gradient scale.
#' @param na.value Color for missing values (default is "grey").
#' @param title A title for the plot (default is NULL).
#' @param legend_title  A title for the legend. Default NULL, name in the \code{values_column}.
#'
#' @return A ggplot object with the plotted NUTS regions.
#'
#' @examples
#' \dontrun{
#' plot_nuts(data = nt_redistrib, nuts_column = "NUTS2", values_column="mean")
#' }
#' @export
plot_nuts <- function(data, nuts_column, values_column,
                      nuts_level = 2,
                      colors=NULL, na.value = "grey",
                      title=NULL, legend_title=NULL){
  NUTS_ID <- NULL
  legend_title <- ifelse(is.null(legend_title), values_column, legend_title)
  map <- gisco_get_nuts(nuts_level = nuts_level)
  map <- map %>%
    left_join(data, by = join_by(NUTS_ID == !!nuts_column)) %>%
    st_as_sf()
  if(is.null(colors)){
    colors <- c('#ffff96', '#e58938', '#a0042a')
  }else{
    colors <- colors
  }
  pl <- ggplot() +
    geom_sf(data = map, aes(fill=.data[[values_column]]))+
    scale_fill_gradientn(colors=colors,
                         na.value = na.value,
                         name= legend_title) +
    ggtitle(title)

  pl
}

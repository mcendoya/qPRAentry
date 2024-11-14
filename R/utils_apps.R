# EU map (from giscoR pkg)
get_EUmap <- function(nuts) {
  suppressWarnings(
    suppressMessages(
      giscoR::gisco_get_nuts(year = year, nuts_level = nuts) %>%
        st_crop(xmin=-40,ymin=20,xmax=50,ymax=70)
    ))
}

cached_get_EUmap <- memoise::memoise(get_EUmap)

ggiraph_plot <- function(data, value, name, title, limits, tooltip, data_id = NULL, ii=0){
  colors <- c('#ffff96', '#e58938', '#a0042a')
  pl <- ggplot() +
    ggiraph::geom_sf_interactive(data=data,
                                 aes(fill=.data[[value]], 
                                     tooltip=tooltip,
                                     data_id=data_id)) +
    scale_fill_gradientn(colors=colors,
                         na.value = "grey",
                         name = name,
                         limits=limits) +
    labs(title=title)+
    theme(legend.key.width = unit(0.8,"cm"),
          legend.key.height = unit(3, "cm"),
          legend.text = element_text(size=28+ii),
          legend.title = element_text(size=32+ii),
          title = element_text(size=34+ii),
          axis.text = element_text(size=26+ii))
  pl_out <- ggiraph::girafe(ggobj = pl,
                            width_svg = 20, height_svg = 15,
                            options = list(opts_hover(css = "stroke-width:1;"),
                                           opts_selection(only_shiny = FALSE, 
                                                          type = "single", 
                                                          css = "stroke:white;")))
  return(pl_out)
}

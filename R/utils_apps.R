read_file <- function(filepath) {
  # Common separators
  seps <- c(",", ";", "\t", "")
  # Try read
  for(sep in seps) {
    t <- try(read.csv(filepath, sep = sep), silent = TRUE)
    # If error try wit row.names = NULL
    if("try-error" %in% class(t)) {
      t <- try(read.csv(filepath, sep = sep, row.names = NULL), silent = TRUE)
      # If error try with the next sep
      if("try-error" %in% class(t)) {
        next
      }
      # If there is a column "row.names", adjust column names
      if("row.names" %in% colnames(t)) {
        colnames(t) <- colnames(t)[2:ncol(t)]
        t <- t[, 1:(ncol(t)-1)]
      }
    }
    # If no error and there are more than one column
    if(ncol(t) > 1) {
      return(t)
    }
  }
  # If all fails
  stop("Error: The file could not be read. Check file format (.csv recommended).")
}

# EU map (from giscoR pkg)
get_EUmap <- function(nuts) {
  suppressWarnings(
    suppressMessages(
      giscoR::gisco_get_nuts(nuts_level = nuts) %>%
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
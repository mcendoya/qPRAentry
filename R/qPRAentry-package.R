#' @import dplyr
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @importFrom utils globalVariables read.csv write.csv zip
#' @importFrom DT datatable formatRound formatStyle
#' @importFrom graphics hist lines
#' @importFrom ggiraph geom_col_interactive geom_sf_interactive girafe girafeOutput 
#' opts_hover opts_selection renderGirafe
#' @importFrom ggplot2 aes element_text ggplot ggtitle guides guide_legend labs 
#' theme unit xlab xlim ylab ylim geom_sf geom_col geom_errorbar 
#' position_dodge scale_fill_manual scale_fill_gradientn theme_bw
#' @importFrom bsplus shinyInput_label_embed shiny_iconlink bs_embed_popover use_bs_popover
#' @importFrom giscoR gisco_get_countries gisco_get_nuts
#' @importFrom eurostat get_eurostat
#' @importFrom memoise memoise
#' @importFrom purrr imap map map2 partial pmap_dfr reduce
#' @importFrom sf st_as_sf st_crop st_drop_geometry
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs addClass disable disabled enable html removeClass runjs useShinyjs
#' @importFrom shinyWidgets dropMenu hideDropMenu pickerInput radioGroupButtons updatePickerInput
#' @importFrom stats rbeta rbinom rcauchy rchisq rexp rf rgamma rgeom rlnorm rnbinom 
#' rnorm rpois rt runif rweibull qbeta qbinom qcauchy qchisq qexp qf qgamma qgeom 
#' qlnorm qnbinom qnorm qpois qt qunif qweibull median quantile sd density setNames
#' @importFrom tidyr expand pivot_longer
#' @keywords internal
"_PACKAGE"

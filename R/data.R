#' Example Trade Data for the European Union
#'
#' `datatrade_EU` simulated trade data for a commodity within the European Union (EU).
#' This dataset illustrates the trade and production flow of a commodity that is a potential 
#' pathway for the entry of a pest into EU countries in yearly time periods. It also 
#' includes simulated consumption data of the commodity at NUTS1 level. 
#' This dataset serves as a reference on how the data should be structured and as 
#' a basis for the examples included in the [qPRAentry] package to deal with the NUTS 
#' code system.
#'
#' @format A list of four data frames:
#' \itemize{
#'   \item **extra_import** \verb{ } Data on imports from countries outside of the EU. 
#'    A data frame with 216 rows and 4 columns:
#'      \tabular{lll}{
#'        \code{reporter} \tab \verb{ } \tab NUTS0 code of the importing country.\cr
#'   \tab \cr
#'        \code{partner} \tab \verb{ } \tab ID of the exporting countries, coded 
#'        as "Extra_Total" for all external countries, and "CNTR_1", "CNTR_2", 
#'        "CNTR_3" to indicate individual non-EU countries where the pest is present.\cr 
#'        \tab \cr
#'        \code{time_period} \tab \verb{ } \tab Time period of trade, with values 
#'        2020 and 2021.\cr \tab \cr
#'        \code{value} \tab \verb{ } \tab Quantity of the commodity imported.\cr
#'      }}
#' \itemize{
#'   \item **intra_trade** \verb{ } Data on internal trade within EU member countries. 
#'    A data frame with 1404 rows and 4 columns:
#'      \tabular{lll}{
#'        \code{reporter} \tab \verb{ } \tab NUTS0 code of the importing country.\cr
#'   \tab \cr
#'        \code{partner} \tab \verb{ } \tab NUTS0 code of the exporting country.\cr
#'   \tab \cr
#'        \code{time_period} \tab \verb{ } \tab Time period of trade, with values 
#'        2020 and 2021.\cr \tab \cr
#'        \code{value} \tab \verb{ } \tab Quantity of the commodity imported.\cr
#'      }}
#' \itemize{
#'   \item **internal_production** \verb{ } Data on the internal production of 
#'   the commodity within the EU. A data frame with 54 rows and 3 columns:
#'      \tabular{lll}{
#'        \code{reporter} \tab \verb{ } \tab NUTS0 code of the producing country.\cr
#'   \tab \cr
#'        \code{time_period} \tab \verb{ } \tab Time period of trade, in years 
#'        (2020 and 2021).\cr \tab \cr
#'        \code{value} \tab \verb{ } \tab Quantity of the commodity produced.\cr
#'      }}
#' \itemize{
#'      \item **consumption_nuts1** \verb{ } Data on the consumption of the commodity 
#'      in the EU at NUTS1 level (administrative divisions). A data frame with 92 rows 
#'      and 2 columns:
#'      \tabular{lll}{
#'        \code{NUTS_ID} \tab \verb{ } \tab NUTS1 code of the administrative division.\cr
#'   \tab \cr
#'        \code{value} \tab \verb{ } \tab Quantity of the commodity consumed.\cr
#'      }}
#' 
"datatrade_EU"

#' Example Trade Data for Northern America
#'
#' `datatrade_NorthAm` simulated trade data for a commodity within Northern America.
#' This dataset illustrates the trade and production flow of a commodity that is a potential 
#' pathway for the entry of a pest into Northern American countries in a year expressed 
#' in three-month periods. It also includes simulated consumption data of the commodity 
#' at at the level of principal sub-national divisions (ISO 3166-2 codes). 
#' This dataset serves as a reference on how the data should be structured and as 
#' a basis for the examples included in the [qPRAentry] package to deal with the ISO 
#' 3166 code system.
#'
#' @format A list of four data frames:
#' \itemize{
#'   \item **extra_import** \verb{ } Data on imports from countries outside of 
#'   Northern America. A data frame with 100 rows and 4 columns:
#'      \tabular{lll}{
#'        \code{reporter} \tab \verb{ } \tab ISO 3166-1 alpha-2 code of the importing country.\cr
#'   \tab \cr
#'        \code{partner} \tab \verb{ } \tab ID of the exporting country, coded as 
#'        "CNTR_1" to "CNTR_5" to represent specific  non-Northern American countries.\cr
#'   \tab \cr
#'        \code{time_period} \tab \verb{ } \tab Time period of trade, in three-month 
#'        periods ("January-March", "April-June", "July-September", "October-December").\cr
#'   \tab \cr
#'        \code{value} \tab \verb{ } \tab Quantity of the commodity imported.\cr
#'      }}
#'   \itemize{
#'   \item **intra_trade** \verb{ } Data on internal trade within Northern American countries. 
#'    A data frame with 80 rows and 4 columns:
#'      \tabular{lll}{
#'        \code{reporter} \tab \verb{ } \tab ISO 3166-1 alpha-2 code of the importing 
#'        country.\cr \tab \cr
#'        \code{partner} \tab \verb{ } \tab ISO 3166-1 alpha-2 code  of the exporting 
#'        country.\cr \tab \cr
#'        \code{time_period} \tab \verb{ } \tab Time period of trade, in three-month 
#'        periods ("January-March", "April-June", "July-September", "October-December").\cr 
#'        \tab \cr
#'        \code{value} \tab \verb{ } \tab Quantity of the commodity imported.\cr
#'      }}
#'   \itemize{
#'   \item **internal_production** \verb{ } Data on the internal production of the commodity 
#'   within Northern America. A data frame with 20 rows and 3 columns:
#'      \tabular{lll}{
#'        \code{reporter} \tab \verb{ } \tab ISO 3166-1 alpha-2 code of the producing 
#'        country.\cr \tab \cr
#'        \code{time_period} \tab \verb{ } \tab Time period of trade, in three-month 
#'        periods ("January-March", "April-June", "July-September", "October-December").\cr 
#'        \tab \cr
#'        \code{value} \tab \verb{ } \tab Quantity of the commodity produced.\cr
#'   }}
#'    \itemize{
#'    \item **consumption_iso2** \verb{ } Data on the consumption of the commodity in 
#'    Northern America at ISO 3166-2 level (principal subdivisions of countries). 
#'    A data frame with 83 rows and 2 columns:
#'      \tabular{lll}{
#'        \code{iso_3166_2} \tab \verb{ } \tab ISO 3166-2 code of the subdivision.\cr 
#'        \tab \cr
#'        \code{value} \tab \verb{ } \tab Quantity of the commodity consumed.\cr
#'      }}
"datatrade_NorthAm"

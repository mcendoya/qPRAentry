#' NUTS_CODES data
#'
#' NUTS codes (Nomenclature of territorial units for statistics) for the European 
#' countries, including European Union Member States, 
#' EFTA countries, and candidate countries and potential candidates. 
#'
#' @format
#' A data frame with 335 rows and 3 columns:
#' \describe{
#'   \item{CNTR_CODE}{2 letter country codes (NUTS0 level), identical to the 
#'   ISO 3166-1 alpha-2 code}
#'   \item{NUTS2_CODE}{4 alphanumeric region codes (NUTS2 level)}
#'   \item{CNTR_NAME}{Country name}
#' }
#' 
#' @source <https://ec.europa.eu/eurostat/web/nuts>
"NUTS_CODES"

#' Example Trade Data for the European Union
#'
#' `datatrade_EU` simulated trade data for a commodity within the European Union (EU).
#' This dataset illustrates the trade and production flow of a commodity that is a potential 
#' pathway for the entry of a pest into EU countries in yearly time periods.
#'
#' @format A list of three data frames:
#' \describe{
#'   \item{extra_import}{Data on imports from countries outside of the EU. 
#'    A data frame with 296 rows and 4 columns:
#'      \describe{
#'        \item{reporter}{ID of the receiving country in NUTS0 code (country level).}
#'        \item{partner}{ID of the exporting countries, coded as "Extra_Total" for all external countries, 
#'        and "CNTR_1", "CNTR_2", "CNTR_3" to indicate individual non-EU countries where
#'        the pest is present.}
#'        \item{time_period}{Time period of trade, with values 2020 and 2021.}
#'        \item{value}{Quantity of the commodity imported.}
#'      }}
#'   \item{intra_trade}{Data on internal trade within EU member countries. 
#'    A data frame with 2664 rows and 4 columns:
#'      \describe{
#'        \item{reporter}{ID of the receiving country in NUTS0 code.}
#'        \item{partner}{ID of the exporting country in NUTS0 code.}
#'        \item{time_period}{Time period of trade, with values 2020 and 2021.}
#'        \item{value}{Quantity of the commodity imported.}
#'      }}
#'   \item{internal_production}{Data on the internal production of the commodity within the EU. 
#'    A data frame with 74 rows and 3 columns:
#'      \describe{
#'        \item{reporter}{ID of the producing country in NUTS0 code.}
#'        \item{time_period}{Time period of trade, in years (2020 and 2021).}
#'        \item{value}{Quantity of the commodity produced.}
#'      }}
#' }
#' 
"datatrade_EU"

#' Example Trade Data for Northern America
#'
#' `datatrade_NorthAm` simulated trade data for a commodity within Northern America.
#' This dataset illustrates the trade and production flow of a commodity that is a potential 
#' pathway for the entry of a pest into Northern American countries in a year expressed 
#' in three-month periods.
#'
#' @format A list of three data frames:
#' \describe{
#'   \item{extra_import}{Data on imports from countries outside of Northern America. 
#'    A data frame with 100 rows and 4 columns:
#'      \describe{
#'        \item{reporter}{ID of the receiving country in a two-letter country code (e.g., "US", "CA").}
#'        \item{partner}{ID of the exporting country, coded as "CNTR_1" to "CNTR_5" to represent specific 
#'        non-Northern American countries.}
#'        \item{time_period}{Time period of trade, in three-month periods ("January-March",
#'        "April-June", "July-September", "October-December").}
#'        \item{value}{Quantity of the commodity imported.}
#'      }}
#'   \item{intra_trade}{Data on internal trade within Northern American countries. 
#'    A data frame with 80 rows and 4 columns:
#'      \describe{
#'        \item{reporter}{ID of the receiving country in a two-letter code (e.g., "US", "CA").}
#'        \item{partner}{ID of the exporting country in a two-letter code (e.g., "US", "CA").}
#'        \item{time_period}{Time period of trade, in three-month periods ("January-March",
#'        "April-June", "July-September", "October-December").}
#'        \item{value}{Quantity of the commodity imported.}
#'      }}
#'   \item{internal_production}{Data on the internal production of the commodity within Northern America. 
#'    A data frame with 20 rows and 3 columns:
#'      \describe{
#'        \item{reporter}{ID of the producing country in a two-letter code (e.g., "US", "CA").}
#'        \item{time_period}{Time period of trade, in three-month periods ("January-March",
#'        "April-June", "July-September", "October-December").}
#'        \item{value}{Quantity of the commodity produced.}
#'      }}
#' }
"datatrade_NorthAm"
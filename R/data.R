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

#' datatrade_EU data
#'
#' NUTS codes (Nomenclature of territorial units for statistics) for the European 
#' countries, including European Union Member States, 
#' EFTA countries, and candidate countries and potential candidates. 
#'
#' @format
#' List of 3 dataframes:
#' 
#' \describe{
#'\itemize{
#' \item \code{extra_import} A data frame external import with 296 rows and 4 columns:
#'
#' \tabular{ll}{
#'   \code{reporter} \tab Country IDs.\cr
#'   \tab \cr
#'   \code{partner} \tab Time period.\cr
#'   \tab \cr
#'   \code{time_period} \tab Time period.\cr
#'   \tab \cr
#'   \code{value} \tab External import from countries where the pest is present.\cr
#' }
#' }
#'\itemize{
#' \item \code{intra_trade} A data frame internal trade with 2664 rows and 4 columns:
#'
#' \tabular{ll}{
#'   \code{reporter} \tab Country IDs.\cr
#'   \tab \cr
#'   \code{partner} \tab Time period.\cr
#'   \tab \cr
#'   \code{time_period} \tab Time period.\cr
#'   \tab \cr
#'   \code{value} \tab External import from countries where the pest is present.\cr
#' }
#' }
#' \itemize{
#' \item \code{internal_production} A data frame internal production with 74 rows 
#' and 3 columns:
#'
#' \tabular{ll}{
#'   \code{reporter} \tab Country IDs.\cr
#'   \tab \cr
#'   \code{time_period} \tab Time period.\cr
#'   \tab \cr
#'   \code{value} \tab External import from countries where the pest is present.\cr
#' }
#' }
#' }
#' 
"datatrade_EU"

#' datatrade_NorthAm data
#'
#' NUTS codes (Nomenclature of territorial units for statistics) for the European 
#' countries, including European Union Member States, 
#' EFTA countries, and candidate countries and potential candidates. 
#'
#' @format
#' List of 3 dataframes:
#' 
#' \describe{
#'\itemize{
#' \item \code{extra_import} A data frame external import with 296 rows and 4 columns:
#'
#' \tabular{ll}{
#'   \code{reporter} \tab Country IDs.\cr
#'   \tab \cr
#'   \code{partner} \tab Time period.\cr
#'   \tab \cr
#'   \code{time_period} \tab Time period.\cr
#'   \tab \cr
#'   \code{value} \tab External import from countries where the pest is present.\cr
#' }
#' }
#'\itemize{
#' \item \code{intra_trade} A data frame internal trade with 2664 rows and 4 columns:
#'
#' \tabular{ll}{
#'   \code{reporter} \tab Country IDs.\cr
#'   \tab \cr
#'   \code{partner} \tab Time period.\cr
#'   \tab \cr
#'   \code{time_period} \tab Time period.\cr
#'   \tab \cr
#'   \code{value} \tab External import from countries where the pest is present.\cr
#' }
#' }
#' \itemize{
#' \item \code{internal_production} A data frame internal production with 74 rows 
#' and 3 columns:
#'
#' \tabular{ll}{
#'   \code{reporter} \tab Country IDs.\cr
#'   \tab \cr
#'   \code{time_period} \tab Time period.\cr
#'   \tab \cr
#'   \code{value} \tab External import from countries where the pest is present.\cr
#' }
#' }
#' }
#' 
"datatrade_NorthAm"
#' Load a CSV file with automatic separator detection
#' 
#' Reads CSV files containing more than one column of 
#' data. It automatically detects common separators (i.e., comma, semicolon, tab), 
#' while allowing users to specify encoding and decimal separators as required.
#'
#' @param filepath A string specifying the path to the CSV file (.csv extension).
#' @param dec A character specifying the decimal separator to use. 
#' Default is "." (period).
#' @param encoding A string specifying the encoding of the file. Default is the native 
#' system encoding set by \code{getOption("encoding")}. For more details on encoding 
#' see \link[base]{file}.
#' 
#' @return A data frame containing the data from the CSV file.
#' 
#' @examples
#' # file path
#' fpath <- system.file("extdata", "data_ex.csv", package="qPRAentry")
#' # Load a CSV file
#' df <- load_csv(fpath) 
#' head(df) # value as character
#' # Load a CSV file with comma separated decimals
#' df <- load_csv(fpath, dec = ",") # value as numeric
#' 
#' @export
load_csv <- function(filepath, dec = ".", encoding = getOption("encoding")) {
  # Verify that the file has a .csv extension
  if (tolower(tools::file_ext(filepath)) != "csv") {
    stop("Error: The file must have a .csv extension.")
  }
  # Common separators
  seps <- c(",", ";", "\t")
  # Try reading with each separator and specified encoding
  for(sep in seps) {
    t <- try(read.csv(filepath, sep = sep, fileEncoding = encoding, dec = dec), 
             silent = TRUE)
    # If error try wit row.names = NULL
    if("try-error" %in% class(t)) {
      t <- try(read.csv(filepath, sep = sep, fileEncoding = encoding, dec = dec, 
                        row.names = NULL), silent = TRUE)
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
  stop("Error: The file could not be read. Check file format and encoding.")
}

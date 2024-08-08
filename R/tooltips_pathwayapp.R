# Help text pathway app
# Data tab
text_ntrade_data <- HTML('<p class="custom-text">
          <ul class="custom-text"">
          <li>Upload the data (CSV format file) </li>
          <li>Select the corresponding column names:</br>
          <ul style="margin-right:10px;">
          <li> NUTS CODES: Column with the NUTS codes (country level or NUTS2) of the 
          EU countries/regions of interest</li>
          <li> Ntrade Values: Column with the Ntade quantitiy.</li>
          </ul>
          </ul>
          </p>')
text_pathwaymodel <- HTML('<p class="custom-text">Select parameters and press <strong>Done</strong></p>')

# data errors
data_ntrade_errors <- list(reporter = paste0("Error: The selected column for 'Reporter' does not contain NUTS Country codes. ",
                                      "Please choose a different column with valid NUTS codes (2-letter code country level)."),
                    partner = paste0("Error: The selected column for 'Partner' does not contain NUTS Country codes. ",
                                     "Please choose a different column with valid NUTS codes (2-letter code country level)."),
                    values_num = paste0("Error: The selected column for 'Values' does not contain numerical data. ",
                                        "Please choose a different column containing numbers."),
                    values_neg = paste0("Error: Invalid values detected. The 'Values' variable contains negative values, ",
                                        "which are not interpretable as quantities. Please review and correct these values."))

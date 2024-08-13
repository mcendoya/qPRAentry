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
text_model_done <- HTML('<p class="custom-text">Note: If you make any changes in trade data
        (new data, columns) or in the pathway model, you must press <strong>Done</strong> again
        to apply the changes.<br></p>')

# data errors
data_ntrade_errors <- list(
  nuts = "Error: The column selected for 'NUTS CODES' must contain NUTS0 codes (country level) or NUTS2 codes.",
  values_num = paste0("Error: The selected column for 'Ntrade Values' does not contain numerical data. ",
                      "Please choose a different column containing numbers."),
  values_neg = paste0("Error: Invalid values detected. The 'Ntrade Values' variable contains negative values, ",
                      "which are not interpretable as quantities. Please review and correct these values."))

# Help text pathway app
# Data tab
text_ntrade_data <- HTML('<p class="custom-text">
          <ul class="custom-text">
          <li><strong>Upload data file:</strong> Please upload your data file in CSV format.</li>
          <li><strong>Column selection:</strong>
            <ul style="margin-right:10px;">
              <li><strong>NUTS codes:</strong> Select the column that contains the NUTS codes, which may be at the country level (NUTS0) or regional level (NUTS2) for the EU countries or regions of interest.</li>
              <li><strong>N<sub>trade</sub> Values:</strong> Choose the column that includes the N<sub>trade</sub> values.</li>
            </ul>
          </li>
          </ul>
          </p>')

text_pathwaymodel <- HTML('<p class="custom-text">
    Select the appropriate parameters for the pathway model. You can choose to include or exclude default parameters and add new parameters as needed. When adding new parameters, specify their role in the model equation, such as defining them as a multiplying factor or another type of contribution.<br><br>
    After configuring the settings, press <strong>Done</strong> to apply and finalise your selections.
</p>')
text_model_done <- HTML('<p class="custom-text">Note: If you make any changes to the trade data (such as uploading new data, selecting different columns, or making adjustments to existing columns) or modify the pathway model parameters, you must press <strong>Done</strong> again to apply and update these changes. Once you have completed the equation, click on <strong>Parameters >></strong><br></p>')

# data errors
data_ntrade_errors <- list(
  nuts = "Error: The column selected for 'NUTS codes' must contain valid NUTS codes. This can include either NUTS0 codes (representing countries) or NUTS2 codes (representing regions). Please verify that the selected column contains the correct format of NUTS codes.",
  values_num = paste0("Error: The column selected for 'N<sub>trade</sub> Values' does not contain numerical data.",
                      "Please choose a different column containing numbers."),
  values_neg = paste0("Error: Invalid values detected. The 'N<sub>trade</sub> Values' variable contains negative values, ",
                      "which are not interpretable as quantities. Please review the data in this column and correct any negative values to ensure accurate data interpretation and processing."
))


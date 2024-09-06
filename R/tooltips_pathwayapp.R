# Help text pathway app
# pathway model tab
text_ntrade_data <- HTML('<p class="custom-text">
          <ul class="custom-text">
          <li><strong>Upload data file:</strong> Please upload your data file in CSV format.</li>
          <li><strong>Column selection:</strong>
            <ul style="margin-right:10px;">
              <li><strong>NUTS codes:</strong> Select the column that contains the NUTS codes, 
              which may be at the country level (NUTS0) or regional level (NUTS2) for the EU countries or regions of interest.</li>
              <li><strong>Values:</strong> Choose the column that includes the <i>N<sub>trade</sub></i> values.</li>
            </ul>
          </li>
          </ul>
          </p>')

text_pathwaymodel <- HTML('<p class="custom-text">
    Select the appropriate parameters for the pathway model. You can choose to include 
    or exclude default parameters and add new parameters as needed. When adding new parameters, 
    specify their role in the model equation, such as defining them as a multiplying factor or another type of contribution.<br><br>
    After configuring the settings, press <strong>Done</strong> to apply and finalise your selections.
</p>')
text_model_done <- HTML('<p class="custom-text">Note: If you make any changes to the trade data 
                        (such as uploading new data, selecting different columns, or making adjustments 
                        to existing columns) or modify the pathway model parameters, you must press 
                        <strong>Done</strong> again to apply and update these changes. Once 
                        you have completed the equation, click on <strong>Parameters >></strong><br></p>')

# data errors
data_ntrade_errors <- list(
  nuts = paste0("Error: The column selected for 'NUTS codes' must contain valid NUTS codes. ",
                "This can include either NUTS0 codes (representing countries) or NUTS2 codes (representing regions). ",
                "Please verify that the selected column contains the correct format of NUTS codes."),
  values_num = HTML(paste0("Error: The column selected for 'Values' does not contain numerical data. ",
                      "Please choose a different column containing numbers.")),
  values_neg = paste0("Error: Invalid values detected. The 'Values' variable contains negative values, ",
                      "which are not interpretable as quantities. Please review the data in this column and correct ",
                      "any negative values to ensure accurate data interpretation and processing."
  ))

# Parameters tab

text_parameters <- HTML('<p class="custom-text">
          <ul class="custom-text">
          <li><strong>Number of iterations:</strong> Set the number of iterations for ...</li>
          <li><strong>Parameters distribution:</strong> Distribution ...</li>
          <li>Click on  <strong>Done</strong></li>
            </ul>
          </li>
          </ul>
          </p>')

text_parametersDone <- HTML(
  '<p class="custom-text">Note: If you make any changes to the If you make any changes 
  to the number of iterations or the parameter distribution, 
  please press <strong>Done</strong> again to apply the changes.<br><br> 
  <i class="fa-solid fa-star" style="color: #63E6BE;"></i> Click on <strong>"Results"</strong> to go to the Results tab.<br></p>'
)

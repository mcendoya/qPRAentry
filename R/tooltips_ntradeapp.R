# Help text ntrade model
# Data tab
text_ExtraTotal <- HTML('<p class="custom-text">To start, click on 
          <strong>Extra-EU Import Total</strong> and follow these steps:
          <ul class="custom-text" style="margin-right:10px;">
          <li>Upload your data file in CSV format.</li>
          <li>Verify unit conversion: The values in the "Values" column will be 
          multiplied by the conversion factor you provide to match the required units.</br>
          (For example, if your data is in kilograms and you need the final values in tons, 
          enter 0.001 as the conversion factor).</li>
          <li>Assign the appropriate columns from your dataset:</br>
          <ul style="margin-right:20px;">
          <li><strong>Reporter:</strong> Select the column containing the NUTS codes (2-letter code) of the EU countries that are importing the commodity.</li>
          <li><strong>Partner:</strong> Select the column containing the IDs of the third countries (non-EU) exporting the commodity.</br>
          Note: It is not necessary to list each exporting country individually; they can be grouped under a single ID (e.g., "total").</li>
          <li><strong>Values:</strong> Select the column that contains the quantity of the imported commodity.</li>
          <li><strong>Time Period:</strong> Select the column that identifies the time period (e.g., year, month, season) for each entry.</li>
          </ul>
          </li>
          <li>Choose the partner countries from the values available in the "Partner" column.</li>
          <li>Finally, click on <strong>Done</strong> to complete the process.</li>
          </ul>
          </p>')
text_ExtraPest <- HTML('<p class="custom-text">Click on <strong>Extra-EU Import from Countries Where the Pest Is Present</strong>, and follow these steps:
          <ul class="custom-text" style="margin-right:10px;">
          <li>Upload your data file in CSV format.</li>
          <li>Check unit conversion: The values in the "Values" column will be multiplied by the conversion factor you provide to match the required units.<br>
          (For example, if the values are in kilograms and the final unit is tons, enter 0.001 as the conversion factor).</li>
          <li>Select the corresponding column names from your dataset:<br>
          <ul style="margin-right:20px;">
          <li><strong>Reporter:</strong> Column with the NUTS codes (2-letter code) for the EU countries importing the commodity.</li>
          <li><strong>Partner:</strong> Column with the IDs of the third countries (non-EU) where the pest is present. You do not need to identify each country individually; you can aggregate them into a single ID (e.g., "extra_pest").</li>
          <li><strong>Values:</strong> Column with the quantity of the imported commodity.</li>
          <li><strong>Time Period:</strong> Column with the time identifier (e.g., year, month, season) for each row of the data.</li>
          </ul>
          </li>
          <li>Select the partner countries from the values found in the "Partner" column.</li>
          <li>Click on <strong>Done</strong> to apply the changes!</li>
          </ul>
          </p>')
text_IntraEU <- HTML('<p class="custom-text">Click on <strong>Intra-EU Import</strong>, and follow these steps:
          <ul class="custom-text" style="margin-right:10px;">
          <li>Upload your data file in CSV format.</li>
          <li>Check unit conversion: The values in the "Values" column will be multiplied by the conversion factor you provide to match the required units.<br>
          (For example, if the values are in kilograms and the final unit is tons, enter 0.001 as the conversion factor).</li>
          <li>Select the corresponding column names from your dataset:<br>
          <ul style="margin-right:20px;">
          <li><strong>Reporter:</strong> Column with the NUTS codes (2-letter code) for the EU countries importing the commodity.</li>
          <li><strong>Partner:</strong> Column with the NUTS codes (2-letter code) for the EU countries exporting the commodity.</li>
          <li><strong>Values:</strong> Column with the quantity of the imported commodity.</li>
          <li><strong>Time Period:</strong> Column with the time identifier (e.g., year, month, season) for each row of the data.</li>
          </ul>
          </li>
          <li>Click on <strong>Done</strong> to apply the changes!</li>
          </ul>
          </p>')
text_IP <- HTML('<p class="custom-text">To get started with <strong>Internal Production</strong>, follow these steps:
          <ul class="custom-text" style="margin-right:10px;">
          <li>Upload your data file in CSV format.</li>
          <li>Verify unit conversion: The values in the "Values" column will be multiplied by the conversion factor you provide to match the required units.</br>
          (For instance, if the data is in kilograms and you need the final values in tons, enter 0.001 as the conversion factor).</li>
          <li>Select the appropriate column names from your dataset:</br>
          <ul style="margin-right:20px;">
          <li><strong>Reporter:</strong> Column with the NUTS codes (2-letter code) for the EU countries that are producing the commodity.</li>
          <li><strong>Values:</strong> Column with the quantity of the commodity produced.</li>
          <li><strong>Time Period:</strong> Column with the time identifier (e.g., year, month, season) for each row of data.</li>
          </ul>
          </li>
          <li>Click on <strong>Done</strong> to apply the changes.</li>
          </ul>
          </p>')
text_dataDone <- HTML('<p class="custom-text">Note: If you make any changes to the trade data 
        (such as updating data, selecting different columns, adjusting units, or modifying partners), 
        please press <strong>Done</strong> again to apply the changes.<br>
        Use the buttons <strong>"Plot Extra-EU Import"</strong>, <strong>"Plot Intra-EU Trade"</strong>, or <strong>"Plot Internal Production"</strong> 
        to change the trade data visualization. You can also click on the bars to view the data for each country across different time periods.<br></p>')

# units info
text_units <- list(title = HTML('<p>Units (weight) for N<sub>trade</sub></p>'),
                   content = HTML("<p>This input will be used for plots and as a 
                                  reference if trade data need to be transformed.<br> 
                                  This input does not exclude the transformation of 
                                  trade data if they are not in the same units 
                                  (this can be done in the corresponding <q>units</q> 
                                  field when uploading each data frame).</p>")
                                    )
# time info
text_time <- list(title = "Trade data time periods",
                  content = HTML("<p>Once the trade data are uploaded, the available 
                                 time periods will be loaded. Select the ones to be 
                                 included in the N<sub>trade</sub> calculation.</p>"))
# data errors
data_errors <- list(
  reporter = paste0("Error: The selected column for 'Reporter' does not contain NUTS Country codes. ",
                    "Please choose a different column with valid NUTS codes (2-letter code country level)."),
  partner = paste0("Error: The selected column for 'Partner' does not contain NUTS Country codes. ",
                   "Please choose a different column with valid NUTS codes (2-letter code country level)."),
  values_num = paste0("Error: The selected column for 'Values' does not contain numerical data. ",
                      "Please choose a different column containing numbers."),
  values_neg = paste0("Error: Invalid values detected. The 'Values' variable contains negative values, ",
                      "which are not interpretable as quantities. Please review and correct these values."),
  extra_partner = paste0("Error: You must select at least one partner for extra trade."))

# Redistribution tab
text_NtradeValue <- HTML('<p class="custom-text">Select the N<sub>trade</sub> value for redistribution.</p>')
text_DataRedistribution <- HTML('<p class="custom-text">Select the data to proportionally 
                        redistribute N<sub>trade</sub> to NUTS2.
                        <ul class="custom-text" style="margin-right:10px;">
                          <li>Population: Eurostat population data</li>
                          <li>My data: requires uploading your own database for redistribution 
                          (e.g. consumption data at NUTS2 level).</li>
                        </ul></p>')

text_PopulationYear <- HTML('<p class="custom-text">Select the year(s) of population data. 
                    If more than one year is selected, the redistribution will be made according 
                    to the average population of the selected years.
                    <ul class="custom-text" style="margin-right:10px;">
                      <li>Click on <strong>"See N<sub>trade</sub> redistribution"</strong></li>
                    </ul></p>')

text_MyData <- HTML('<p class="custom-text">Upload the data (CSV format file)
          <ul class="custom-text" style="margin-right:10px;">
          <li>Select the corresponding column names:</br>
          <ul style="margin-right:20px;">
          <li> NUTS2: Column with the NUTS2 codes</li>
          <li> Values: Column with values to proportionally redistribute N<sub>trade</sub></li>
          </ul>
          <li>Click on <strong>"See N<sub>trade</sub> redistribution"</strong></li>
          </ul>
          </p>')
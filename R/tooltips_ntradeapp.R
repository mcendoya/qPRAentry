# Help text ntrade model
# Data tab
text_ExtraTotal <- HTML('<p class="custom-text">Click on 
          <strong>Extra-EU import total</strong>
          <ul class="custom-text" style="margin-right:10px;">
          <li>Upload the data (CSV format file) </li>
          <li>Check units conversion: the value column is multiplied by the value entered 
          to transform the units</br>
          (e.g., if the values are in kg and tons is defined as the final unit, 
          0.001 must be entered).</li>
          <li>Select the corresponding column names:</br>
          <ul style="margin-right:20px;">
          <li> Reporter: Column with the NUTS codes (2-letter code) of the EU countries of interest 
          importing the commodity</li>
          <li> Partner: Column with the IDs of the third countries (non-EU) exporting 
          the commodity.</br>
          It is not required to identify each of these countries, they can be aggregated 
          into a single ID (e.g., "total").</li>
          <li> Values: Column with the quantity of imported commodity</li>
          <li> Time period: Column with the identifier for the time (year, month, season, ...) 
          corresponding to each row of the data</li>
          </ul>
          <li>Select the partner countries: values found in the column Partner</li>
          <li>Click on <strong>Done</strong>!</li>
          </ul>
          </p>')
text_ExtraPest <- HTML('<p class="custom-text"">Click on 
          <strong>Extra-EU import from countries where the pest is present</strong>
          <ul class="custom-text" style="margin-right:10px;">
          <li>Upload the data (CSV format file) </li>
          <li>Check units conversion: the value column is multiplied by the value entered 
          to transform the units</br>
          (e.g., if the values are in kg and tons is defined as the final unit, 
          0.001 must be entered).</li>
          <li>Select the corresponding column names:</br>
          <ul style="margin-right:20px;">
          <li> Reporter: Column with the NUTS codes (2-letter code) of the EU countries of interest 
          importing the commodity</li>
          <li> Partner: Column with the IDs of the third countries (non-EU) where the pest 
          is present exporting the commodity.</br>
          It is not required to identify each of these countries, they can be aggregated 
          into a single ID (e.g., "extra_pest").</li>
          <li> Values: Column with the quantity of imported commodity.</li>
          <li> Time period: Column with the identifier for the time (year, month, season, ...) 
          corresponding to each row of the data</li>
          </ul>
          <li>Select the partner countries: values found in the column Partner</li>
          <li>Click on <strong>Done</strong>!</li>
          </ul></p>')
text_IntraEU <- HTML('<p class="custom-text">Click on 
          <strong>Intra-EU import</strong>
          <ul class="custom-text" style="margin-right:10px;">
          <li>Upload the data (CSV format file) </li>
          <li>Check units conversion: the value column is multiplied by the value entered 
          to transform the units</br>
          (e.g., if the values are in kg and tons is defined as the final unit, 
          0.001 must be entered).</li>
          <li>Select the corresponding column names:</br>
          <ul style="margin-right:20px;">
          <li> Reporter: Column with the NUTS codes (2-letter code) of the EU countries of interest 
          importing the commodity</li>
          <li> Partner: Column with the NUTS codes (2-letter code) of the EU countries of interest 
          exporting the commodity</li>
          <li> Values: Column with the quantity of imported commodity</li>
          <li> Time period: Column with the identifier for the time (year, month, season, ...) 
          corresponding to each row of the data</li>
          </ul>
          <li>Click on <strong>Done</strong>!</li>
          </ul>
          </p>')
text_IP <- HTML('<p class="custom-text">Click on 
          <strong>Internal production</strong>
          <ul class="custom-text" style="margin-right:10px;">
          <li>Upload the data (CSV format file) </li>
          <li>Check units conversion: the value column is multiplied by the value entered 
          to transform the units</br>
          (e.g., if the values are in kg and tons is defined as the final unit, 
          0.001 must be entered).</li>
          <li>Select the corresponding column names:</br>
          <ul style="margin-right:20px;">
          <li> Reporter: Column with the NUTS codes (2-letter code) of the EU countries of interest 
          producing the commodity</li>
          <li> Values: Column with the quantity of commodity produced</li>
          <li> Time period: Column with the identifier for the time (year, month, season, ...) 
          corresponding to each row of the data</li>
          </ul>
          <li>Click on <strong>Done</strong>!</li>
          </ul>
          </p>')
text_dataDone <- HTML('<p class="custom-text">Note: If you make any changes in trade data
        (new data, columns, units, partner), you must press <strong>Done</strong> again
        to apply the changes.<br>Click on buttons to change the trade data visualization.
        Click on bars to plot each country at the different time periods.<br></p>')

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
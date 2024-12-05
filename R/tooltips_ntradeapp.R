# Help text ntrade model
# Data tab
text_trade_data <- function(data, partner=TRUE){
  if(data == "ExtraTotal"){
    data_name <- "ExtraTotal Import"
    data_def <- "Total quantity of commodity from third countries imported by the 
    countries of interest."
    columns <- c('<li><strong>Reporter:</strong> Select the column containing the NUTS codes
                 (2-letter code) of the countries of interest that are importing the commodity.</li>
                   <li><strong>Partner:</strong> Select the column containing the IDs of the
                 third countries exporting the commodity.</br>
                   Note: It is not necessary to list each exporting country individually;
                 they can be grouped under a single ID (e.g., "total").</li>
                   <li><strong>Values:</strong> Select the column that contains the quantity 
                 of the imported commodity.</li>')
  }else if(data == "ExtraPest"){
    data_name <- "ExtraPest Import"
    data_def <- "Quantity of commodity from third countries where the pest under 
    assessment is present imported by the countries of interest."
    columns <- c('<li><strong>Reporter:</strong> Column with the NUTS codes (2-letter code)
          for the countries of interest importing the commodity.</li>
          <li><strong>Partner:</strong> Column with the IDs of the third countries 
          where the pest is present. You do not need to identify each country individually;
          you can aggregate them into a single ID (e.g., "extra_pest").</li>
          <li><strong>Values:</strong> Column with the quantity of the imported commodity.</li>')
  }else if(data == "Intra"){
    data_name <- "Intra Trade"
    data_def <- "Quantity of commodity traded between the countries of interest."
    columns <- c('<li><strong>Reporter:</strong> Column with the NUTS codes (2-letter code) 
          for the countries of interest importing the commodity.</li>
          <li><strong>Partner:</strong> Column with the NUTS codes (2-letter code) 
          for the countries of interest exporting the commodity.</li>
          <li><strong>Values:</strong> Column with the quantity of the imported commodity.</li>')
  }else if(data == "IP"){
    data_name <- "Internal Production"
    data_def <- "Quantity of commodity produced in the countries of interest."
    columns <- c('<li><strong>Reporter:</strong> Column with the NUTS codes (2-letter code) 
                 for the countries of interest that are producing the commodity.</li>
                 <li><strong>Values:</strong> Column with the quantity of the commodity 
                 produced.</li>')
  }
  
  if(partner){
    partner_countries <- c('<li><strong><strong>Partner countries</strong></strong>: 
                           Choose the partner countries from the values available 
                           in the "Partner" column.</li>')
  }else{
    partner_countries <- ''
  }
  
  HTML(
    paste0(
      '<p style="color:#1E68BA; font-size:18px;"><strong><i>', data_name, 
      ': </i></strong>', data_def, '</p>',
      '<p class="custom-text">Click on
          <strong><i>', data_name, '</i></strong> and follow these steps:
          <ul class="custom-text" style="margin-right:10px;">
          <li><strong>Data file</strong>: Upload your data file in CSV format 
          (ensure that numeric values use a decimal point "." as the separator).</li>
          <li><strong>Data units</strong>: Verify unit conversion. The values in the 
          "Values" column will be multiplied by the conversion factor you provide 
          to match the required units. For example, if your data is in kilograms 
          and you need the final values in tons, enter 0.001 as the conversion factor.</li>
          <li><strong>Column names</strong>: Assign the appropriate columns from 
          your dataset:</br>
          <ul style="margin-right:20px;">',
      columns,
      '<li><strong>Time period:</strong> Select the column that identifies the 
          time period (e.g., year, month, season) for each entry.</li>
          </ul></li>',
      partner_countries,
      '<li>Finally, click on <strong>Done</strong> to complete the process.</li>
          </ul></p>'
    )
  )
}

text_dataDone <- HTML(
  '<p class="custom-text">Note: If you make any changes to the trade data
  (such as updating data, selecting different columns, adjusting units, or modifying 
  partners), please press <strong>Done</strong> again to apply the changes.<br><br> 
  <i class="fa-solid fa-star" style="color: #63E6BE;"></i> Click on <strong>"See 
  <i>N<sub>trade</sub></i> results"</strong> to go to the Results tab.<br></p>'
)

# units info
text_units <- list(title = HTML('<p>Units (weight) for <i>N<sub>trade</sub></i></p>'),
                   content = HTML("<p>This input will be used for plots and as a 
                                  reference if trade data need to be transformed.<br> 
                                  This input does not exclude the transformation of 
                                  trade data if they are not in the same units 
                                  (this can be done in the corresponding <q>units</q> 
                                  field when uploading each data frame).</p>")
                                    )
# NUTS year info
text_nuts_yr <- list(title = HTML('<p>NUTS classification year</p>'),
                   content = HTML("<p>Please check that the NUTS codes in your data 
                   correspond to the NUTS classification selected. Otherwise an error will 
                   be displayed. See <a href='https://ec.europa.eu/eurostat/web/nuts/history'>
                   Eurostat-NUTS</a> for further information.</p>")
)
# time info
text_time <- list(title = "Trade data time periods",
                  content = HTML("<p>Once the trade data are uploaded, the available 
                                 time periods will be loaded. Select the ones to be 
                                 included in the <i>N<sub>trade</sub></i> calculation.</p>"))
# data errors
data_errors <- list(
  reporter = paste(strwrap("Error: The selected column for 'Reporter' does not contain 
                           NUTS Country codes. Please choose a different column with 
                           valid NUTS codes (2-letter code country level)."), 
                   collapse=" "),
  partner = paste(strwrap("Error: The selected column for 'Partner' does not contain 
                          NUTS Country codes. Please choose a different column with 
                          valid NUTS codes (2-letter code country level)."), 
                  collapse=" "),
  values_num = paste(strwrap("Error: The selected column for 'Values' does not contain 
                             numerical data. Please choose a different column containing 
                             numbers."), collapse=" "),
  values_neg = paste(strwrap("Error: Invalid values detected. The 'Values' variable 
                             contains negative values, which are not interpretable 
                             as quantities. Please review and correct these values."), 
                     collapse=" "),
  extra_partner = paste(strwrap("Error: You must select at least one partner for 
                                Extra Import."), collapse=" ")
  )

# Redistribution tab
text_NtradeValue <- HTML('<p class="custom-text">Select the <i>N<sub>trade</sub></i> 
                         value for redistribution.</p>')
text_DataRedistribution <- HTML(
  '<p class="custom-text">To proportionally redistribute <i>N<sub>trade</sub></i> 
  to NUTS2 regions, select either:
  <ul class="custom-text" style="margin-right:10px;">
  <li><strong>Human population (Eurostat):</strong> This option uses the Eurostat 
  human population data to proportionally redistribute <i>N<sub>trade</sub></i> 
  based on the human population size in each NUTS2 region.</li>
  <li><strong>Custom Data:</strong> This option allows the user to upload a custom 
  dataset that can serve as the basis for the redistribution 
  (e.g., consumption data at the NUTS2 level).</li>
  </ul></p>'
)

text_PopulationYear <- HTML('<p class="custom-text">Select one or more years of 
                            human population data for redistribution. If multiple 
                            years are selected, the redistribution will be based on the 
                            average human population across those years. 
                            <ul class="custom-text" style="margin-right:10px;">
                            <li>Once the years are selected, click on <strong>"See 
                            <i>N<sub>trade</sub></i> redistribution"</strong> to 
                            proceed.</li></ul></p>')

text_MyData <- HTML('<p class="custom-text">Upload the data in CSV format by following 
                    these steps:<ul class="custom-text" style="margin-right:10px;">
                    <li>Select the appropriate column names:</li>
                    <ul style="margin-right:20px;">
                    <li><strong>NUTS2:</strong> Column containing the NUTS2 codes 
                    for regions</li><li><strong>Values:</strong> Column containing 
                    the values for proportional redistribution of <i>N<sub>trade</sub></i>
                    </li></ul>
                    <li>Click on <strong>"See <i>N<sub>trade</sub></i> 
                    redistribution"</strong> to proceed.</li></ul></p>')

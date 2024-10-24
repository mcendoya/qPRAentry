# Help text pathway app
# pathway model tab
text_pathwaymodel <-
  HTML(
    '<p class="custom-text">
  Select the appropriate parameters for the pathway model. You can choose to 
  include or exclude <strong>default parameters</strong> and add new parameters as needed.
  <br><br>
  <strong>Add other parameters:</strong>
  <ul>
    <li> Select the number of parameters to add.</li>
    <li><strong>Parameter Name</strong>:
    <ul>
      <li>Enter only the name of the parameter (e.g., <code>p1</code>, <code>p_1</code>, <code>p_{inf}</code>).</li>
      <li>If using subscripted parameters, use <code>_</code> between braces for more than one character 
      (e.g., <code>p_{inf}</code>).</li>
    </ul>
    </li>
    <li><strong>In the equation as</strong>:
      <ul>
        <li>Select the operation symbol that applies to the parameter</li>
        <li>Define how the parameter is introduced in the equation. 
        If transformations are required, input them here.
          <ul>
            <li>Example transformations: <code>(1 - p_1)</code>, <code>(p_{inf} / 100)</code></li>
          </ul>
        </li>
        <li>You may add parentheses to adjust the order of operations as necessary.
          <ul>
            <li>Example: <code>((p1*100)</code>, <code>(1-p2))</code></li>
          </ul>
        <li>Ensure that the parameter name in the equation matches exactly what was 
        entered in <strong>Parameter Name</strong>.</li>
      </ul>
    </li>
  </ul>
  <br>
  Once the parameters have been set, click on <strong>Done</strong> to update the equation.
  </p>'
  )


text_model_done <- HTML('<p class="custom-text">
                        Note: If you modify the pathway model parameters, you must 
                        press <strong>Done</strong> again to apply and update these changes.<br><br>
                        <i class="fa-solid fa-star" style="color: #63E6BE;"></i>
                        Once you have completed the equation, you can go to the 
                        <strong style="color: #1E68BA;"><i>N<sub>trade</sub></i> data</strong> 
                        tab to to proceed with the data upload.<br></p>')
# Ntrade data tab


text_ntrade_data <- HTML('<p class="custom-text">
          <ul class="custom-text">
          <li><strong>Upload data file:</strong> Please upload your data file in CSV format.</li>
          <li><strong>Column selection:</strong>
            <ul style="margin-right:10px;">
              <li><strong>NUTS codes:</strong> Select the column that contains the NUTS codes, 
              which may be at the country level (NUTS0) or regional level (NUTS2) for the EU 
              countries or regions of interest.</li>
              <li><strong>Values:</strong> Choose the column that includes the <i>N<sub>trade</sub></i> values.</li>
            </ul>
          </li>
          </ul>
          </p>')

text_data_done <- HTML('<p class="custom-text">Note: If you make any changes to the trade data 
                        (such as uploading new data, selecting different columns, or making adjustments 
                        to existing columns), you must press <strong>Done</strong> again to apply 
                        and update these changes.<br><br>
                        <i class="fa-solid fa-star" style="color: #63E6BE;"></i>
                        Once you have verified the data, you can go to the <strong style="color: #1E68BA;">
                        Parameters</strong> tab.<br></p>')



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
          <li><strong>Number of iterations:</strong> The number of iterations determines how many times the process will be repeated. 
        The total number of iterations that will be performed during the execution needs to be specified in the box "number of iterations". 
        In each iteration, a single value is drawn from the specified distribution for each parameter, representing a sample for that iteration.</li>
          <li><strong>Parameters distribution:</strong> The distribution type for each parameter needs to be defined. 
          Common distribution types include Normal (Gaussian), Uniform, Exponential, or others that may fit the nature of the data. 
        </li>
          <li>Once all parameters have been set, you can click on the <strong>Done</strong> button to save the current configuration. 
          After clicking, for each parameter, the distribution histograms will be displayed next to the parameter.</li>
            </ul>
          </li>
          </ul>
          </p>')

text_parametersDone <- HTML('<p class="custom-text">
                            Note: If you make any changes to the number of iterations 
                            or the parameter distribution, please press <strong>Done</strong> 
                            again to apply the changes.<br><br>
                            <i class="fa-solid fa-star" style="color: #63E6BE;"></i>
                            After checking the distributions, you can go to the 
                            <strong style="color: #1E68BA;">Results</strong> tab.<br></p>'
)

---
output:
  html_document: default
  pdf_document: default
---

# Overview of the Pathway model Shiny application

<hr style="border:1px solid #1E68BA">
<br>

The Pathway model application is designed to estimate the number of potential founder populations of a pest in different regions, using $N_{trade}$ data combined with additional user-defined parameters.


The pathway model is tailored for use with European countries, either at the national level (NUTS0) or the regional level (NUTS2), as classified by the [NUTS - Nomenclature of Territorial Units for Statistics](https://ec.europa.eu/eurostat/web/nuts), including the European Union Member States, EFTA countries, and candidate countries and potential candidates. Note that it is not necessary to consider all countries included in this list.

For countries outside of Europe, the pathway model can be applied using the `pathway_model` function from the `qPRAentry` package in R. In order to use the `pathway_model` function, install the `qPRAentry` package.

<br>

### Data and parameter requirements
<br>

In order to built and execute the pathway model, the following inputs are required:

- **$N_{trade}$ data**: This represents the total quantity of commodities imported by a country or region from third countries where the pest is present.The use of the $N_{trade}$ Shiny application is recommended for the calculation of $N_{trade}$ data at the national level (NUTS0) or the regional level (NUTS2).
- **User-defined parameters**: The model uses additional parameters to estimate the number of founder populations in each country/region. These parameters can either be:
  - **Default parameters**: The application comes pre-loaded with commonly used parameters based on the [EFSA 2018 guidelines](https://doi.org/10.2903/j.efsa.2018.5350).
  - **Custom Parameters**: Users have the flexibility to deselect the pre-loaded parameters and add new ones. When adding new parameters,  their role in the model equation has to be specified, such as defining them as a multiplying factor or another type of contribution.
  - **Parameter distribution**: The distribution type for each parameter needs to be defined. The distributions and shape of the parameters can come from the Expert Knowledge Elicitation (EKE) ([EFSA 2018 guidelines](https://doi.org/10.2903/j.efsa.2018.5350)). 

<br>


## $N_{inf}$: downloading results

<br>

Upon completion of the analysis, the user will have the option to download a ZIP file containing:

- A comprehensive PDF report summarising the analysis results
- A CSV file with the $N_{inf}$ table results

To proceed with the analysis, navigate to the "Pathway model" tab at the top of the page.




#_____________
I would remove the part below as the random sampling is explained in the dedicated page

### Model execution and outputs

<br>

- **Random sampling**: For each parameter, the model generates random values (as many as the specified number of iterations) based on the given distributions. 

- **Statistical summary**: Once the model has completed its iterations, the application generates a detailed statistical summary of the results. This summary includes the estimated number of potential founder populations for each individual country or region, as well as an aggregate total for all the included areas.

<br>

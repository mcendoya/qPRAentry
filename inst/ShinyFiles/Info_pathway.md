---
output:
  html_document: default
  pdf_document: default
---

# Overview of the Pathway model Shiny application

<hr style="border:1px solid #1E68BA">
<br>

The Pathway model application is designed to estimate the number of potential founder populations of a pest in different regions, using $N_{trade}$ data combined with additional user-defined parameters.


## Regional applications of the pathway model

### For European countries

The pathway model is tailored for use with European countries, either at the national level (NUTS0) or the regional level (NUTS2), as classified by the [NUTS - Nomenclature of Territorial Units for Statistics](https://ec.europa.eu/eurostat/web/nuts). This includes:

- **European Union Member States**
- **EFTA countries**
- **Candidate countries and potential candidates**

### For non-European countries
For countries outside of Europe, the pathway model can be applied using the `pathway_model` function from the `qPRAentry` package in R. In order to use the `pathway_model` function, install the `qPRAentry` package.


### Data and parameter requirements

The model requires the following inputs:

- **$N_{trade}$ data**: This represents the total quantity of commodities imported by a country or region from third countries where the pest is present.The use of the $N_{trade}$ Shiny application is recommended for the calculation of $N_{trade}$ data at the national level (NUTS0) or the regional level (NUTS2).

- **User-defined parameters**: The model uses additional parameters to estimate the number of founder populations in each country/region. These parameters can either be:
  - **Default parameters**: The application comes pre-loaded with commonly used parameters based on the [EFSA 2018 guidelines](https://doi.org/10.2903/j.efsa.2018.5350).
  - **Custom Parameters**: Users have the flexibility to input their own parameter distributions.


### Model execution and outputs

- **Random sampling**: For each parameter, the model generates random values (as many as the specified number of iterations) based on the given distributions. 

- **Statistical summary**: Once the model has completed its iterations, the application generates a detailed statistical summary of the results. This summary includes the estimated number of potential founder populations for each individual country or region, as well as an aggregate total for all the included areas.

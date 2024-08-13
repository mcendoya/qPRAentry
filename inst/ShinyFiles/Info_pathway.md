---
output:
  html_document: default
  pdf_document: default
---

# Overview of the Pathway model Shiny application
=======================

The Pathway model application is designed to estimate the number of potential founder populations of a pest in different regions, using $N_{trade}$ data combined with additional user-defined parameters.


## Regional Applications of the Pathway Model

### For European Countries

The Pathway model is tailored for use with European countries, either at the national level (NUTS0) or the regional level (NUTS2), as classified by the [NUTS - Nomenclature of Territorial Units for Statistics](https://ec.europa.eu/eurostat/web/nuts). This includes:

- **European Union Member States**
- **EFTA countries**
- **Candidate countries and potential candidates**

### For non-European countries
For countries outside of Europe, the Pathway model can be applied using the `pathway_model` function from the `qPRAentry` package in R. In order to use the `pathway_model` function, install the `qPRAentry` package.


### Data and parameter requirements

The model requires the following inputs:

- **$N_{trade}$ data**: This represents the total quantity of commodities imported by a country or region from third countries where the pest is present.

- **User-Defined Parameters**: The model uses additional parameters to simulate the establishment of founder populations. These parameters can either be:
  - **Default Parameters**: The application comes pre-loaded with commonly used parameters based on the [EFSA 2018 guidelines](https://doi.org/10.2903/j.efsa.2018.5350).
  - **Custom Parameters**: Users have the flexibility to input their own parameter distributions.





Info Pathway model
=======================

* For European countries at country (NUTS0) or region (NUTS2) level 
([NUTS - Nomenclature of territorial units for statistics](https://ec.europa.eu/eurostat/web/nuts))
(European Union Member States, EFTA countries, candidate countries and potential candidates)
* Uses $N_{trade}$ data and additional user-defined parameters to estimate the number 
of founder populations in each country/region
* The use of `ntrade_app` is recommended for the calculation of $N_{trade}$ at NUTS0 or NUTS2 level
* The parameters commonly used in the pathway model ([EFSA 2018](https://doi.org/10.2903/j.efsa.2018.5350)) 
are provided by default, however the model can be completely user-defined
* Requires $N_{trade}$ data and parameter distributions. Random values (as many as the 
specified number of iterations) for the model parameters are generated from the given 
distribution for each one.
* As a result, the statistical summary of the iterations of the number of potential 
founder populations for each country/region and the total for all the included countries/regions.

* For other countries of the world see the `pathway_model` function included in the 
`qPRAentry` package

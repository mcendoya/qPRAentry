---
output:
  html_document: default
  pdf_document: default
---

# Overview of the Pathway model Shiny app

<hr style="border:1px solid #1E68BA">
<br>

The Pathway model Shiny app is designed to estimate the number of potential 
founder populations $(\mathit{NPFP})$ of a pest in different regions, using 
$N_{trade}$ data combined with additional user-defined parameters.


The pathway model is tailored for application within European countries, 
either at the national level (NUTS0) or the regional level (NUTS2), as classified by the 
[NUTS - Nomenclature of Territorial Units for Statistics](https://ec.europa.eu/eurostat/web/nuts) classification. This includes all European Union Member States, EFTA countries,
candidate countries and potential candidates. 
It is important to note that it is not necessary to include all countries 
included in this list.

For countries outside of Europe, the pathway model can be applied using the `pathway_model` 
function from the `qPRAentry` package in R. In order to use the `pathway_model` 
function, install the `qPRAentry` package.

<br>

## Data and parameter requirements
<br>

The pathway model equation to estimate the $\mathit{NPFP}$ in each country/region 
must first be established. 

The application provides by default the model with the parameters frequently 
used in EFSA's quantitative Pest Risk Assessments ([EFSA 2018 guidelines](https://doi.org/10.2903/j.efsa.2018.5350)). 
However, users can deselect these default parameters and/or add new ones.
When adding new parameters, their roles in the model equation must be specified, 
including the symbol for their incorporation into the model 
and whether they require transformation.


Once the equation has been established, the following inputs are required:
- **$N_{trade}$ data**: Total quantity of commodities imported by 
a country or region from third countries where the pest of interest is present.
The use of the $N_{trade}$ Shiny app is recommended for the calculation of 
$N_{trade}$ data at NUTS0 or NUTS2 level.
- **Parameter distribution**: The distribution type for each of the chosen 
parameter needs to be defined. The distributions and shape of the parameters 
can come from the Expert Knowledge Elicitation (EKE)
([EFSA 2018 guidelines](https://doi.org/10.2903/j.efsa.2018.5350)). 

<br>


## Downloading results

<br>

Upon completion of the analysis, the user have the option to download a 
ZIP file containing:

- A comprehensive PDF report summarising the analysis results (pathway_report.pdf)
- A CSV file with the $\mathit{NPFP}$ table results (NPFP.csv)

To proceed with the analysis, navigate to the "Pathway model" tab at the top of the page.
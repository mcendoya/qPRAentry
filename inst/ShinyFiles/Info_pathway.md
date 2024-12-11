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


The pathway model is tailored for application within countries included in the NUTS 
([Nomenclature of territorial units for statistics](https://ec.europa.eu/eurostat/web/nuts)) 
coding system, which includes European Union Member States, EFTA countries, candidate 
countries and potential candidates. The pathway model can be applied at the national 
level (NUTS0) or the regional level (NUTS2). It is important to note that it is 
not necessary to include all countries included in this list.

For other countries worldwide, the pathway model can be applied using the `pathway_model` 
function from the `qPRAentry` package in R. In order to use the `pathway_model` 
function, install the `qPRAentry` package.

<br>

## Data and parameter requirements
<br>

The pathway model equation to estimate the $\mathit{NPFP}$ in each NUTS0 or NUTS2 
must first be established. 

The application provides by default the model with the parameters frequently 
used in EFSA's quantitative Pest Risk Assessments 
([EFSA 2018 guidelines](https://doi.org/10.2903/j.efsa.2018.5350)). 
However, users can deselect these default parameters and/or add new ones.
When adding a new parameter, the user must specify how it should be incorporated 
into the equation, including the symbol for their incorporation into the model 
(e.g., as a multiplying factor) and whether they require transformation.


Once the equation has been established, the following inputs are required:
- **$N_{trade}$ data**: Total quantity of commodities imported by 
a NUTS0 or NUTS2 from third countries where the pest under assessment is present.
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

- A comprehensive report summarising the analysis results (pathway_report.html)
- A CSV file with the $\mathit{NPFP}$ table results (NPFP.csv)

To proceed with the analysis, navigate to the "Pathway model" tab at the top of the page.
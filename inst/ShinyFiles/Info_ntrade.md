---
output:
  html_document: default
  pdf_document: default
---

# Overview of $N_{trade}$ Shiny application

<hr style="border:1px solid #1E68BA">
<br>

$N_{trade}$ is defined as the total quantity of commodities (infested by a pest or not) imported by a country (e.g., an EU country) from other third-countries where the pest is present. 

This application is meant to calculate $N_{trade}$ for European countries ([NUTS - Nomenclature of territorial units for statistics](https://ec.europa.eu/eurostat/web/nuts)), including European Union Member States, EFTA countries, and candidate countries and potential candidates. Note that it is not necessary to consider all countries included in this list.

For countries outside Europe, the calculation of $N_{trade}$ can be performed using the `ntrade` function from the `qPRAentry` package.

For the use of this application, trade data of the commodities of interest, i.e., potential carriers of the target pests, are required. The trade data required are: 

- Total quantity of non-EU commodity imported by EU countries.
- Quantity of commodities from non-EU countries where the pest is present imported by EU countries.
- Quantity of commodity imported between EU countries.
- Quantity of commodity produced in the EU countries.


**Suggestion: It is suggested to download these trade data from Eurostat.**

<br>

## Calculation of $N_{trade}$

<br>

For a given country $i$, $N_{trade}$ is calculated as follows:

$$
N_{trade_i} = ExtraPest_i - ExtraPest_i \sum_{j \neq i} R_{ij} + \sum_{j \neq i} ExtraPest_j R_{ji},
$$

Where:

- **$ExtraPest_i$**: Quantity of non-EU commodity imported by country $i$ from countries where the pest is present.
- **$ExtraPest_j$**: Quantity of non-EU commodity imported by country $j$ from countries where the pest is present.
- **$R_{ij}$**: The proportion of $ExtraPest_i$ reexported from country $i$ to country $j$. This is calculated as:
  $$
  R_{ij} = \frac{IntraExp_{ij}}{Total_i}
  $$
  where $IntraExp_{ij}$ is the quantity of commodity exported from country $i$ to country $j$, and $Total_i$ is the total quantity of commodity available in country $i$, given by:
  $$
  Total_i = IP_i + ExtraTotal_i
  $$
  Here, $IP_i$ represents the internal production in country $i$, and $ExtraTotal_i$ is the total quantity of non-EU commodity imported by country $i$.
- **$R_{ji}$**: The proportion of $ExtraPest_j$ reexported from country $j$ to country $i$. This is calculated as:
  $$
  R_{ji} = \frac{IntraExp_{ji}}{Total_j}
  $$
  where $IntraExp_{ji}$ is the quantity of commodity exported from country $j$ to country $i$, and $Total_j$ is the total quantity of commodity available in country $j$.

<br>

## $N_{trade}$ redistribution to NUTS2

<br>

After calculating $N_{trade}$ at the national level (NUTS0), the next step involves redistributing these quantities to NUTS2 regions. 

Users have two options for redistributing $N_{trade}$ data to NUTS2 regions:

1. **Population-based redistribution using Eurostat data**

   - **Overview:** This option allows users to redistribute $N_{trade}$ data to NUTS2 regions proportionally based on population data. The population data is sourced from Eurostat and is already integrated into the application.
   
   - **How It works:** The application calculates the proportion of the national population that resides in each NUTS2 region. It then redistributes the total $N_{trade}$ value for a country to its respective NUTS2 regions based on these population proportions. This method approximates that trade activity and, consequently, pest risk are related to population density.


2. **Custom redistribution using user-uploaded data**

   - **Overview:** This option provides users with the flexibility to perform a personalised redistribution based on custom data. Users can upload their own dataset containing specific values for each NUTS2 region.
   
   - **How It works:** Users upload a file containing the NUTS2 codes along with corresponding values that represent the desired proportion of $N_{trade}$ to be allocated to each region. The application will then use this custom dataset to redistribute the $N_{trade}$ quantities accordingly.

<br>

## $N_{trade}$ results

<br>

Upon completion of the analysis, the user will have the option to download a ZIP file containing:

- A comprehensive PDF report summarising the analysis results
- A CSV file with the $N_{trade}$ table results
- A CSV file with the $N_{trade}$ table redistribution results

To proceed with the analysis, navigate to the $N_{trade}$ tab at the top of the page.

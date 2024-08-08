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
Info $N_{trade}$
=======================

* For European countries
([NUTS - Nomenclature of territorial units for statistics](https://ec.europa.eu/eurostat/web/nuts))
(European Union Member States, EFTA countries, candidate countries and potential candidates)
* For other countries of the world see the `ntrade` function included in the 
`qPRAentry` package

### $N_{trade}$ calculation

The calculation of $N_{trade}$  for each country of interest $i$ is based on the equation:

$$N_{trade_i} = ExtraPest_i - ExtraPest_i\sum_{j\neq i} \frac{IntraExp_{ij}}{Total_i} + \sum_{j\neq i} ExtraPest_j \frac{IntraExp_{ji}}{Total_j},$$

* $ExtraPest_i$: quantity of non-EU commodity imported by $i$ from countries where the pest is present.
* $ExtraPest_j$: quantity of non-EU commodity imported by $j$ from countries where the pest is present.
* $IntraExp_{ij}$: quantity of commodity exported from $i$ to $j$.
* $IntraExp_{ij}$: quantity of commodity exported from $j$ to $i$.
* $Total_i$: total quantity of commodity available in $i$ as $Total_i = IP_i + ExtraTotal_i$, 
where $IP_i$ is the internal production in $i$ and $ExtraTotal_i$ the total quantity of non-EU commodity imported by $i$.

### $N_{trade}$ redistribution

Redistribution to NUTS2: proportional redistribution

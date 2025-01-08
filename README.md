# qPRAentry <img src="man/figures/logo.png" align="right" width="150" />

<!-- badges: start -->
[![CRAN](https://www.r-pkg.org/badges/version/qPRAentry)](https://cran.r-project.org/package=qPRAentry)
[![R-CMD-check](https://github.com/mcendoya/qPRAentry/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mcendoya/qPRAentry/actions/workflows/R-CMD-check.yaml)
![downloads](https://cranlogs.r-pkg.org/badges/grand-total/qPRAentry)
<!-- badges: end -->
### Quantitative Pest Risk Assessment at the Entry Step

`qPRAentry` is an R package to supports risk assessors in performing the entry step of the quantitative Pest Risk Assessment. It allows the estimation of the amount of a plant pest entering a risk assessment area (in terms of founder populations) through the calculation of the imported commodities that could be potential pathways of pest entry, and the development of a pathway model. Two 'Shiny' apps based on the functionalities of the package are included, that simplify the process of assessing the risk of entry of plant pests. The approach is based on the work of the European Food Safety Authority [(EFSA PLH Panel et al., 2018)](https://doi.org/10.2903/j.efsa.2018.5350).

### Installation

The latest version of `qPRAentry` can be installed from CRAN via:

```r
install.packages("qPRAentry")
```

Load then the `qPRAentry` library and check the help function for the documentation:

```r
library(qPRAentry)
?qPRAentry
```

### Tutorial

For a detailed tutorial on how to use the `qPRAentry` functions and workflow, see the R package vignette:

Within R:
```R
vignette("qPRAentry_workflow", package = "qPRAentry")
```

Within a web-browser:
```R
browseVignettes(package = "qPRAentry")
```






As the `reportingGrofWild` package is not available on CRAN, the easiest way of installing the package is using the `devtools` package and referring to the git repository holding the package code:

```
devtools::install_github("inbo/reporting-rshiny-grofwildjacht", subdir = "reporting-grofwild")
```

Remark that the package itself is stored in a subfolder of the repository, which is tackled by the `subdir` command.

One installed, the functionalities can be derived by loading the package:

``` r
library("reportingGrofwild")
```

## Development information

When adding elements to the code or ui and you want to test if your local adaptations are effectively working, make sure to build and install your local adaptations:

(assuming your working in a Rstudio project)

```
devtools::build()
devtools::install()
library(reportingGrofwild)
reportingGrofwild::runWildApp()
```

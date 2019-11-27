
install.packages("ellipsis") 
## => indien error => restart sessie

devtools::install_github("inbo/reporting-rshiny-grofwildjacht@wildschade", subdir = "reporting-grofwild")
devtools::build(pkg = "reporting-grofwild")
devtools::install(pkg = "reporting-grofwild")
library(reportingGrofwild)
reportingGrofwild::runWildApp()

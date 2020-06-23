
rm(list=ls())
#install.packages("pkgdown")
pkgdown::build_site()

roxygen2::roxygenise()

devtools::build_vignettes()


#devtools::use_vignette("collecting-data")
#devtools::use_vignette("assembling-long-data")
#devtools::use_vignette("working-with-long-data")

devtools::build_vignettes()
browseVignettes("smallDB")

?get_core_data
collect_table()

roxygen2::roxygenise()

devtools::install(build_vignettes = TRUE)
3


data(epi_abs_codes)
epi_abs_codes

endo_dx_codes

rm(list=ls())
data("endo_dx_codes")
endo_dx_codes

?get_rx

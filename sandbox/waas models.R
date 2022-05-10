##################################################
## Project: Jay Yield
## Script purpose: Testing out the multi-trait indexes from metan
## Date: 2022-05-09
## Author: Jay Gillenwater
##################################################

source("./packages.R")
tar_make()

tar_load(c(all_yield_data, genotype_contrasts_mixed))

fit_waas <- function(yield_data, si = 20){
  
  waas_mod <- waasb(yield_data, 
                    env = ENV, 
                    gen = genotype, 
                    rep = rep, 
                    resp = c(oil, protein, yield, lod), 
                    mresp = c("h", "h", "h", "l"), 
                    wresp = c(25, 25, 100, 5))

  mtsi(waas_mod, SI = si) %>% 
    plot()
  
  
}


x11()
fit_waas(all_yield_data$two_year$`Jay Test 1`, si = 50)
x11()
fit_waas(all_yield_data$two_year$`Jay Test 2`, si = 50)

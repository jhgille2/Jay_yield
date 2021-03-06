##################################################
## Project: Jay Yield
## Script purpose: custom ggplot themes
## Date: 2022-05-10
## Author: Jay Gillenwater
##################################################

theme_jay_yield_base <- function(base_size = 12, base_family=""){
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14))
}
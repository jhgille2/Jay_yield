##################################################
## Project: Jay Yield
## Script purpose: Utility functions for the yield analysis
## Date: 2022-03-16
## Author: Jay Gillenwater
##################################################

# A function to return the ggplot color palette for a given number of categories
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# A function that takes a conversion table with an "old name" inn the first column
# and the name that it should be recoded to in the second column, and takes
# this table to recode a given variable in some dataframe
convert_from_table <- function(data, var, conversiontable){
  
  all_matches   <- match(data[, var], conversiontable[, 1])
  match_indices <- which(!is.na(all_matches))
  
  data[, var][match_indices] <- conversiontable[, 2][all_matches[match_indices]]
  
  return(data)
}
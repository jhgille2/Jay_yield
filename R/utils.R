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
  
  all_matches   <- match(unlist(data[, var]), unlist(conversiontable[, 1]))
  match_indices <- which(!is.na(all_matches))
  
  new_var <- as.character(unlist(data[, var]))
  
  new_var[match_indices] <- unlist(conversiontable[, 2])[all_matches[match_indices]]
  
  data[, var] <- new_var
  
  return(data)
}

# A simple version of the above, just returns the value of the first match
match_from_table <- function(old_name, conversiontable){
  
  all_matches   <- match(old_name, unlist(conversiontable[, 1]))
  
  converted_name <- as.character(unlist(conversiontable[, 2])[all_matches])
  
  return(converted_name)
}


# A function to replace the names of a dataframe by using a lookup table
rename_with_lookup <- function(df, lookup){
  
  index_match <- function(vec, convert_table){
    matches     <- match(vec, unlist(convert_table[, 1]))
    matches_ind <- which(!is.na(matches))
    
    replacement <- unlist(convert_table[, 2])[matches[matches_ind]]
    
    return(list("index" = matches_ind, 
                "replacement" = replacement))
  }
  
  replacement_names <- index_match(names(df), lookup)
  
  names(df)[replacement_names$index] <- replacement_names$replacement
  
  return(df)
}

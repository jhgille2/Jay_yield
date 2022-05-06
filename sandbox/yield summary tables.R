##################################################
## Project: Jay Yield
## Script purpose: Testing out functions to make summary tables of the 
## phenotype BLUEs/MSD tables
## Date: 2022-04-08
## Author: Jay Gillenwater
##################################################


# Load in the BLUEs, utility tables, and MSDs
tar_load(genotype_BLUEs)
tar_load(util_tables)
tar_load(least_sig_differences)
tar_load(linear_means)

# A function to get the minimum significant difference from the lsd results list
get_msd <- function(fit_model){
  msd <- fit_model %>% 
    pluck("hsd") %>% 
    pluck("statistics") %>% 
    pluck("MSD")
  
  if(is.null(msd)){
    msd <- NA
  }
  
  msd <- round(msd, 2)
  
  return(msd)
}

# A helper function for identifying the column indices of some phenotype
# when multiple variables are recorded for multiple phenotypes. The specific
# issue this function resolves is overlaps in matches for oil/protein with
# oil_plus_protein
get_pheno_indices <- function(column_names, pheno_name, name_conversion = util_tables$trait_shortnames){
  
  # Get all the string matches and then exclude protein_plus_oil if the pheno_name
  # is either protein or oil
  all_matches <- str_detect(column_names, pheno_name)
  
  if(pheno_name %in% c("protein", "oil")){
    p_o_matches <- str_detect(column_names, "protein_plus_oil")
    final_matches <- (all_matches & !p_o_matches) %>% 
      which()
  }else{
    final_matches <- which(all_matches)
  }
  
  # Get cleaned up names for the column indices
  return(final_matches)
}


# A function that takes the phenotype BLUEs table and calculates some summary
# information for each phenotype: 
#
# 1. The overall test average, 
# 2. The average for the checks
# 3. The rank of each genotype
make_pheno_comparison <- function(test_data, keep_phenos = c("yield", "protein", "oil", "protein_plus_oil")){
  
  genotype_ranks <- test_data %>% 
    pivot_longer(cols = util_tables$trait_shortnames$name, 
                 names_to = "pheno") %>% 
    group_by(pheno) %>% 
    arrange(desc(value)) %>% 
    mutate(rank = 1:n()) %>% 
    ungroup() %>%
    group_by(pheno) %>% 
    mutate(test_avg = mean(value, na.rm = TRUE)) %>% 
    dplyr::filter(pheno %in% keep_phenos) %>% 
    mutate(pheno = factor(pheno, levels = keep_phenos)) %>%
    mutate(value = round(value, 2), 
           test_avg = round(test_avg, 2))
  
  check_pheno_avg <- test_data %>% 
    dplyr::filter(genotype %in% util_tables$check_table$genotype) %>% 
    pivot_longer(cols = util_tables$trait_shortnames$name, 
                 names_to = "pheno") %>% 
    group_by(pheno) %>% 
    summarise(check_avg_value = mean(value, na.rm = TRUE)) %>% 
    mutate(check_avg_value = round(check_avg_value, 2))
  
  full_data <- left_join(genotype_ranks, check_pheno_avg, by = "pheno") %>% 
    mutate(value = paste0(value, " (", round((value/check_avg_value)*100, 1), "%)"))
  
  return(full_data)
  
}

# This function joins the augmented test summary data to the MSD table
# to add the MSD for each phenotype onto the table and then pivot back to 
# a wider format
full_comparison_table <- function(blue_data, lsd_data, keep_phenos = c("yield", "protein", "oil", "protein_plus_oil")){
  
  # Get just the MSD from the msd dataframe
  just_msd <- lsd_data %>% 
    map(., function(x) mutate(x, msd = map_dbl(fit_model, get_msd)) %>%
          select(trait, msd))
  
  # Apply the phenotype comparison function to the BLUE data to make 
  # a summary table for each test
  test_comparisons <- map(blue_data, make_pheno_comparison, keep_phenos = keep_phenos)
  
  # Now join each summary table to the corresponding MSD table for each test
  # and then pivot to a wide format using the phenotypes to name columns
  # and all the columns that have been added to fill the values
  full_comparisons <- map2(test_comparisons, just_msd, function(x, y) left_join(x, y, by = c("pheno" = "trait")) %>% 
                             pivot_wider(id_cols = c("genotype"), names_from = "pheno", values_from = c("value", "rank", "test_avg", "check_avg_value", "msd"), names_vary = "slowest"))
  
  
  pheno_indices <- map(full_comparisons, 
                       function(x) map(keep_phenos, 
                                       function(y) get_pheno_indices(colnames(x), y)) %>% 
                         set_names(map_chr(keep_phenos, function(y) match_from_table(y, util_tables$trait_shortnames))))
  
  res <- map2(full_comparisons, pheno_indices, function(x, y) list(x, y) %>% 
                set_names(c("comparison_table", "phenotype_indices")))
  
  return(res)
}

# The same function from above, but keep the dat ain a long format
# that is more helpful for plotting and other data management
full_comparison_long <- function(blue_data, lsd_data, keep_phenos = c("yield", "protein", "oil", "protein_plus_oil")){
  
  # Get just the MSD from the msd dataframe
  just_msd <- lsd_data %>% 
    map(., function(x) mutate(x, msd = map_dbl(fit_model, get_msd)) %>%
          select(trait, msd))
  
  # Apply the phenotype comparison function to the BLUE data to make 
  # a summary table for each test
  test_comparisons <- map(blue_data, make_pheno_comparison, keep_phenos = keep_phenos)
  
  # Now join each summary table to the corresponding MSD table for each test
  # and then pivot to a wide format using the phenotypes to name columns
  # and all the columns that have been added to fill the values
  full_comparisons <- map2(test_comparisons, just_msd, function(x, y) left_join(x, y, by = c("pheno" = "trait")))
  
  return(full_comparisons)
}

# Testing the functions together
comparison_tables <- full_comparison_table(blue_data = linear_means, least_sig_differences)

# TODO:
# Need to make a function that takes this data and converts it to a more 
# reader-friendly format. I think that this will specifically involve writing
# a function that can make hierarchical headings based on the current column 
# names that can be used by kable to make nice looking latex table headings
#
# As an example, the yield trait has five columns of data: 
# genotype BLUE, genotype rank, overall test average, 
# check average, and MSD.
#
# Im imagining a table layout where each trait included in the table can have
# a layout something like this: 
#
#     |                 Yield                    |
#     |__________________________________________| .......
#     | BLUE | Rank | Test Avg | Check Avg | MSD |
#     
# I can think this can be done in a fairly straightforward way by finding the 
# iundexes of the columns that match each phenotype name eg
# str_detect(colnames(x), "phenotype name") %>% which() and then use other
# string processing functions to split the variable category (BLUE, rank, etc..)
# from the phenotype, and then use lookup tables to clean up the names. 
#
# kableExtra has the function add_header_above that needs a named vector of 
# header widths where the names say how the group headers will be labelled
#  e.g. c("Yield" = 5, "Oil" = 5,...). This makes things easier since each
# phenotype has the same number of summary variables (for now). I think
# it could be a good idea to include secondary output into the to function
# that makes the tables to provide the headers that will eventually be required
# by kable so that I won't have to change multiple functions later on so that
# the labels for the tables can be directly tied to how they're generated. 

# A function to remove phenotype names from the column names so that simpler
# variable names can be used under the phenotype group name headers
# This is necessary because I have to remove protein_plus_oil from the 
# names before protein or oil
remove_phenotype_names <- function(column_names, pheno_names = c("yield", "protein", "oil", "protein_plus_oil")){
  column_names %>% 
    str_remove_all("protein_plus_oil") %>% 
    str_remove_all(., paste(pheno_names, collapse = "|")) %>% 
    str_remove(., "_$")
}


# A function to make a kable table with group headings to match phenotypes
make_phenotype_comparison_table <- function(comparison_table_output, pheno_names = c("yield", "protein", "oil", "protein_plus_oil"), keep_msd = TRUE){
  
  # First, sort the indices of the phenotypes by the order in which they appear 
  # in the comparison table
  phenotypes_ordered <- pluck(comparison_table_output, "phenotype_indices") %>% 
    map_dbl(., min) %>% 
    order() 
  
  phenotype_indices <- pluck(comparison_table_output, "phenotype_indices")[phenotypes_ordered]
  
  # What column do the phenotype measurements start on?
  pheno_start_col <- min(unlist(phenotype_indices))
  
  # How many columns of summary data do each phenotype have?
  phenotype_widths <- phenotype_indices %>%
    map_dbl(., length) %>% 
    unlist()
  
  # Create a vector that can be passed to kable to add grouped headings
  # to the table
  column_groups <- c(rep("", pheno_start_col-1), phenotype_widths)
  
  current_table <- pluck(comparison_table_output, "comparison_table")
  
  # variables that can have their rows collapsed
  collapse_cols <- c("test_avg", 
                     "check_avg_value", 
                     "msd")
  
  # The indices of these columns
  collapse_indices <- map(collapse_cols, function(x) which(str_detect(colnames(current_table), x))) %>% 
    unlist() %>% 
    unique() %>% 
    sort()
  
  # Convert these columns to character variables
  current_table %<>% 
    mutate(across(collapse_indices, as.character))
  
  colnames(current_table) <- remove_phenotype_names(colnames(current_table), pheno_names = pheno_names)
  
  # Rename the columns in the dataframe using the summary table rename
  # helper table
  current_table <- rename_with_lookup(current_table, util_tables$column_shortnames)
  
  current_table <- as.data.frame(current_table)
  
  # If you want to remove the MSD columns (Like if I'm data from mixed models instead of the linear models)
  if(!keep_msd){
    
    # Remove the HSD columns from the collapse rows indices
    exclude_cols     <- which(colnames(current_table) == "HSD")
    
    newcolnames <- colnames(current_table)[-exclude_cols]
    
    # And reduce the header widths by 1 for each phenotype
    column_groups <- c(rep("", pheno_start_col-1), phenotype_widths-1)
    
    # Finally, remove the HSD columns from the overall data
    current_table <- current_table[, -exclude_cols]
    colnames(current_table) <- newcolnames
    
    # variables that can have their rows collapsed
    collapse_cols <- c("Test Average", 
                       "Check Average")
    
    # The indices of these columns
    collapse_indices <- map(collapse_cols, function(x) which(str_detect(colnames(current_table), x))) %>% 
      unlist() %>% 
      unique() %>% 
      sort()
  }
  
  
  knitr::kable(current_table, "html") %>% 
    kable_classic() %>% 
    collapse_rows(columns = collapse_indices) %>%
    add_header_above(column_groups)
  
}

make_phenotype_comparison_table(comparison_tables$`Jay Test 1`, keep_msd = FALSE)

tar_load(least_sig_differences)


# A function to get the minimum significant difference from the lsd results list
get_msd <- function(fit_model){
  msd <- fit_model %>% 
    pluck("lsd") %>% 
    pluck("statistics") %>% 
    pluck("MSD")
  
  if(is.null(msd)){
    msd <- NA
  }
  
  msd <- round(msd, 2)
  
  return(msd)
}



##################################################
## Project: Jay Yield
## Script purpose: Testing out functions to make sumamry tables of the 
## phenotype BLUEs/MSD tables
## Date: 2022-04-08
## Author: Jay Gillenwater
##################################################

# Load in the BLUEs, utility tables, and MSDs
tar_load(genotype_BLUEs)
tar_load(util_tables)
tar_load(least_sig_differences)

# A function that takes the phenotype BLUEs table and calculates some summary
# information for each phenotype: 
#
# 1. The overall test average, 
# 2. The average for the checks
# 3. The rank of each genoype
make_pheno_comparison <- function(test_data){
  
  keep_phenos <- c("yield", "protein", "oil", "protein_plus_oil")
  
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
    mutate(pheno = ordered(pheno, keep_phenos)) %>%
    mutate(value = round(value, 2), 
           test_avg = round(test_avg, 2))
  
  check_pheno_avg <- test_data %>% 
    dplyr::filter(genotype %in% util_tables$check_table$genotype) %>% 
    pivot_longer(cols = util_tables$trait_shortnames$name, 
                 names_to = "pheno") %>% 
    group_by(pheno) %>% 
    summarise(check_avg_value = mean(value, na.rm = TRUE)) %>% 
    mutate(check_avg_value = round(check_avg_value, 2))
  
  left_join(genotype_ranks, check_pheno_avg, by = "pheno")
  
}

# This function joins the augmented test summary data to the MSD table
# to add the MSD for each phenotype onto the table and then pivot back to 
# a wider format
full_comparison_table <- function(blue_data, lsd_data){
  
  # Get just the MSD from the msd dataframe
  just_msd <- lsd_data %>% 
    map(., function(x) mutate(x, msd = map_dbl(fit_model, get_msd)) %>%
          select(trait, msd))
  
  # Apply the phenotype comparison function to the BLUE data to make 
  # a summary table for each test
  test_comparisons <- map(blue_data, make_pheno_comparison)
  
  # Now join each summary table to the correspnding MSD table for each test
  # and then pivot to a wide format using the phenotypes to name columns
  # and all the columns that have been added to fill the values
  full_comparisons <- map2(test_comparisons, just_msd, function(x, y) left_join(x, y, by = c("pheno" = "trait")) %>% 
                             pivot_wider(id_cols = c("genotype"), names_from = "pheno", values_from = c("value", "rank", "test_avg", "check_avg_value", "msd"), names_vary = "slowest"))
  
  return(full_comparisons)
}

# Testing the functions together
comparison_tables <- full_comparison_table(blue_data = genotype_BLUEs$BLUEs, least_sig_differences) %>% 
  map2(., names(.), function(x, y) mutate(x, test_name = y) %>% relocate(test_name, 1))

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



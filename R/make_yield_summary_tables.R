#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param blue_data
#' @param utility_tables
#' @param lsd_data
#' @param linear_means
make_yield_summary_tables <- function(blue_data = genotype_BLUEs,
                                      utility_tables = util_tables, lsd_data =
                                      least_sig_differences, linear_means =
                                      linear_means, 
                                      summary_phenos = c("yield", "protein", "oil")) {

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
  get_pheno_indices <- function(column_names, pheno_name, name_conversion = utility_tables$trait_shortnames){
    
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
  make_pheno_comparison <- function(test_data, keep_phenos = summary_phenos){
    
    genotype_ranks <- test_data %>% 
      pivot_longer(cols = utility_tables$trait_shortnames$name, 
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
      dplyr::filter(genotype %in% utility_tables$yield_checks$genotype) %>% 
      pivot_longer(cols = utility_tables$trait_shortnames$name, 
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
  full_comparison_table <- function(blue_data, lsd_data, keep_phenos = summary_phenos){
    
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
                           set_names(map_chr(keep_phenos, function(y) match_from_table(y, utility_tables$trait_shortnames))))
    
    res <- map2(full_comparisons, pheno_indices, function(x, y) list(x, y) %>% 
                  set_names(c("comparison_table", "phenotype_indices")))
    
    return(res)
  }
  
  # The same function from above, but keep the dat ain a long format
  # that is more helpful for plotting and other data management
  full_comparison_long <- function(blue_data, lsd_data, keep_phenos = summary_phenos){
    
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
  
  # A function to remove phenotype names from the column names so that simpler
  # variable names can be used under the phenotype group name headers
  # This is necessary because I have to remove protein_plus_oil from the 
  # names before protein or oil
  remove_phenotype_names <- function(column_names, pheno_names = summary_phenos){
    column_names %>% 
      str_remove_all("protein_plus_oil") %>% 
      str_remove_all(., paste(pheno_names, collapse = "|")) %>% 
      str_remove(., "_$")
  }
  
  
  # A function to make a kable table with group headings to match phenotypes
  make_phenotype_comparison_table <- function(comparison_table_output, test_name, pheno_names = summary_phenos, keep_msd = TRUE){
    
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
    current_table <- rename_with_lookup(current_table, utility_tables$column_shortnames)
    
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
    
    table_caption <- paste("Marginal means and rankings of genotypes in", test_name, "for seed yield, seed oil, and seed protein.")
    
    names(current_table)[2] <- paste0(names(current_table)[2], 
                                      footnote_marker_symbol(1, format = "latex"))
    
    names(current_table)[3] <- paste0(names(current_table)[3], 
                                      footnote_marker_symbol(2, format = "latex", double_escape = TRUE))
    
    names(current_table)[4] <- paste0(names(current_table)[4], 
                                      footnote_marker_symbol(3, format = "latex", double_escape = TRUE))
    
    names(current_table)[5] <- paste0(names(current_table)[5], 
                                      footnote_marker_symbol(4, format = "latex", double_escape = TRUE))
    
    
    knitr::kable(current_table, "latex", booktabs = TRUE, caption = table_caption, align = rep("c", ncol(current_table))) %>% 
      collapse_rows(columns = collapse_indices) %>%
      add_header_above(column_groups) %>% 
      kable_styling(latex_options = "scale_down") %>% 
      footnote(symbol = c("Observed marginal mean of the phenotype (value of the phenotype/check average * 100%).", 
                          "Rank of the genotype within the particular phenotype.", 
                          "The average value of the phenotype for all lines in the test.", 
                          "The average value of the phenotype for the checks in the test."))
    
  }
  
  test_summary_tables <- full_comparison_table(blue_data = blue_data$BLUEs, lsd_data) %>% 
    map2(., names(.), make_phenotype_comparison_table, keep_msd = FALSE)
  
  return(test_summary_tables)
}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param blue_data
#' @param utility_tables
#' @param summary_phenos
make_elite_summary_tables <- function(blue_data = genotype_BLUEs,
                                      utility_tables = util_tables, 
                                      lsd_data = least_sig_differences) {

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
  make_pheno_comparison <- function(test_data, keep_phenos = c("yield", "protein_plus_oil")){
    
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
  full_comparison_table <- function(blue_data, lsd_data, keep_phenos = c("yield", "protein_plus_oil")){
    
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
                               pivot_wider(id_cols = c("genotype", "test_name"), names_from = "pheno", values_from = c("value", "rank", "test_avg", "check_avg_value", "msd"), names_vary = "slowest"))
    
    
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
  full_comparison_long <- function(blue_data, lsd_data, keep_phenos = c("yield", "protein_plus_oil")){
    
    # Get just the MSD from the msd dataframe
    just_msd <- lsd_data %>% 
      map(., function(x) mutate(x, msd = map_dbl(fit_model, get_msd)) %>%
            select(trait, msd))
    
    # Apply the phenotype comparison function to the BLUE data to make 
    # a summary table for each test
    test_comparisons <- map(blue_data, make_pheno_comparison, keep_phenos = keep_phenos)
    
    # Get the test that each genotype belongs to 
    geno_tests <- blue_data %>% 
      select(test_name, genotype)
    
    # Now join each summary table to the corresponding MSD table for each test
    # and then pivot to a wide format using the phenotypes to name columns
    # and all the columns that have been added to fill the values
    full_comparisons <- map2(test_comparisons, just_msd, function(x, y) left_join(x, y, by = c("pheno" = "trait"))) %>% 
      left_join(geno_tests, by = genotype)
    
    return(full_comparisons)
  }
  
  # Testing the functions together
  comparison_tables <- full_comparison_table(blue_data = blue_data$BLUEs, lsd_data)
  
  # Protein and yield comparison tables
  py_comparison_tables <- full_comparison_table(blue_data = blue_data$BLUEs, lsd_data,keep_phenos = c("yield", "protein"))
  
  # Protein oil yield comparison tables
  poy_comparison_tables <- full_comparison_table(blue_data = blue_data$BLUEs, lsd_data, keep_phenos = c("yield", "protein", "oil"))
  
  # Protein meal comparison tables
  meal_comparison_tables <- full_comparison_table(blue_data = blue_data$BLUEs, lsd_data, keep_phenos = c("protein_meal"))
  
  # Agronomic comparison tables
  ag_comparison_tables <- full_comparison_table(blue_data = blue_data$BLUEs, lsd_data, keep_phenos = c("ht", "lod", "sdwt", "sq"))
  
  # Lodging comparison tales
  lodging_comparison_tables <- full_comparison_table(blue_data = blue_data$BLUEs, lsd_data, keep_phenos = c("lod"))
  
  # Need to make a function that takes this data and converts it to a more 
  # reader-friendly format. I think that this will specifically involve writing
  # a function that can make hierarchical headings based on the current column 
  # names that can be used by kable to make nice looking latex table headings
  #
  # As an example, the yield trait has five columns of data: 
  # genotype BLUE, genotype rank, overall test average, 
  # check average, and MSD.
  #
  # I'm imagining a table layout where each trait included in the table can have
  # a layout something like this: 
  #
  #     |                 Yield                    |
  #     |__________________________________________| .......
  #     | BLUE | Rank | Test Avg | Check Avg | MSD |
  #     
  # I can think this can be done in a fairly straightforward way by finding the 
  # indexes of the columns that match each phenotype name eg
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
  make_phenotype_comparison_table <- function(comparison_table_output, pheno_names = c("yield", "protein_plus_oil"), keep_msd = TRUE, keep_genos = NULL, table_caption = NULL, sort_var = "protein"){
    
    if(all(names(comparison_table_output) == c("Jay Test 1", "Jay Test 2"))){
      merged_comparison_tables <- map(comparison_table_output, function(x) pluck(x, "comparison_table")) %>% 
        reduce(bind_rows)
      
      comparison_table_output <- list("comparison_table"  = merged_comparison_tables, 
                                      "phenotype_indices" = comparison_table_output[[1]]$phenotype_indices)
    }
    
    # If keep_genos is specified, first filter the comparison table fo just those genotypes
    if(!is.null(keep_genos)){
      comparison_table_output$comparison_table <- comparison_table_output$comparison_table %>% 
        dplyr::filter(genotype %in% keep_genos)
    }
    
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
                         "Check Average", 
                         "Test Name")
      
      # The indices of these columns
      collapse_indices <- map(collapse_cols, function(x) which(str_detect(colnames(current_table), x))) %>% 
        unlist() %>% 
        unique() %>% 
        sort()
    }
    
    
    # TODO: This part needs to be fixed so that the footnote symbols can be 
    # added to the column headings
    # names(current_table)[1] <- paste0(names(current_table)[1],
    #                                   footnote_marker_symbol(1, format = "latex"))
    # 
    # names(current_table)[2] <- paste0(names(current_table)[2],
    #                                   footnote_marker_symbol(2, format = "latex"))
    # 
    # names(current_table)[3] <- paste0(names(current_table)[3],
    #                                   footnote_marker_symbol(3, format = "latex"))
    # 
    # names(current_table)[4] <- paste0(names(current_table)[4],
    #                                   footnote_marker_symbol(4, format = "latex"))
    # 
    # names(current_table)[5] <- paste0(names(current_table)[5],
    #                                   footnote_marker_symbol(5, format = "latex"))
    
    table_footnotes <-  c("The genotype name.", 
                          "The test name",
                          "The genotype marginal mean for the phenotype (value divided by check average).", 
                          "The ranking of this genotype for the phenotype within its test.", 
                          "The average phenotype value for all genotypes in the test.",
                          "The average value of the checks in the test.")

    
    # Export two copies of the table, one that uses the kable package to make a latex table, and another that
    # uses the flextable package to make tables that can be exported to word. 
    kable_table <- knitr::kable(current_table, "latex", booktabs = TRUE, caption = table_caption) %>% 
      kable_styling(latex_options=c("scale_down", "HOLD_position")) %>%
      collapse_rows(columns = collapse_indices) %>%
      add_header_above(column_groups, escape = FALSE) %>% 
      kableExtra::footnote(symbol = table_footnotes)
    
    
    # Flextable doesn't like it if column names are repeated. I'll first
    # remove them and then put them back later as a header row. 
    current_table_names <- colnames(current_table)
    colnames(current_table) <- letters[1:length(current_table_names)]
    
    # Replace the empty elements of the column_groups vector with widths of 1
    column_groups[which(is.na(as.numeric(column_groups)))] <- 1
    
    FitFlextableToPage <- function(ft, pgwidth = 8){
      
      ft_out <- ft %>% autofit()
      
      ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
      return(ft_out)
    }
    
    # Make the flextable
    flextable_table <- flextable(current_table) %>% 
      delete_part() %>% 
      add_header_row(values = current_table_names) %>% 
      theme_box() %>% 
      add_header_row(colwidths = as.numeric(column_groups), values = names(column_groups)) %>% 
      merge_v(j = collapse_indices) %>%
      flextable::footnote(i = 2, j = 1:6, 
                          value = as_paragraph(table_footnotes), 
                          ref_symbols = letters[1:6], 
                          part = "header") %>% 
      set_caption(table_caption) %>%
      align(align = "center", part = "header") %>%
      align(align = "center", part = "body") %>% 
      FitFlextableToPage() %>% 
      fontsize(size = 8, part = "all") %>% 
      flextable::font(fontname = "Cambria (body)", part = "all")

    # And return a list of both tables. 
    res <- list("tex_table"  = kable_table, 
                "word_table" = flextable_table)
    
    return(res)
  }
  
  # Genotypes with similar oil and yield, high protein
  elite_genos <- c("N18-1635", "N18-1627", "N18-1643", "N18-1783")
  
  # The five highest performing protein lines
  high_protein <- c("N18-1761", "N18-1575", "N18-1769", "N18-1763", "N18-1855")
  
  # 10 highest p + o
  high_po <- c("N18-1761", "N18-1575", "N18-1769", "N18-1632-2", "N18-1855", 
               "N18-1763", "N18-1595", "N18-1731", "N18-1674", "N18-1674")
  
  # High pro, similar yield
  hp_sy <- c("N18-1572", "N18-1595", "N18-1620", "N18-1632-1", "N18-1635", 
             "N18-1674", "N18-1682", "N18-1731", "N18-1751", "N18-1763", 
             "N18-1855", "N18-1575", "N18-1627", "N18-1643", "N18-1761", 
             "N18-1769", "N18-1783")
  
  # High lodging genotypes
  high_lodging <- c("N18-1604", "N18-1641", "N18-1956", "N18-1577", "N18-1579", 
                    "N18-1586", "N18-1628", "N18-1659", "N18-1661", "N18-1796", 
                    "N18-1820")
  
  # All genotypes
  all_genos <- blue_data$BLUEs %>% 
    bind_rows() %>% 
    pluck("genotype") %>% 
    unique() %>% 
    as.character()

  
  elite_geno_table <- make_phenotype_comparison_table(poy_comparison_tables, 
                                                      keep_msd = FALSE, 
                                                      keep_genos = elite_genos, 
                                                      pheno_names = c("yield", "oil","protein"),
                                                      table_caption = "Soybean genotypes with yield and seed oil comparable to check cultivars, and seed protein superior to check cultivars.")
  
  high_po_table <- make_phenotype_comparison_table(comparison_tables, 
                                                   keep_msd = FALSE, 
                                                   keep_genos = high_po, 
                                                   table_caption = "Top 10 soybean genotypes on the basis of protein plus oil content.")
  
  hp_sy_table <- make_phenotype_comparison_table(py_comparison_tables, 
                                                 keep_msd = FALSE, 
                                                 keep_genos = hp_sy, 
                                                 pheno_names = c("yield", "protein"),
                                                 table_caption = "Soybean RILs with superior protein content and comparable yield performance to high-yielding check cultivars.")
  
  protein_meal_table <- make_phenotype_comparison_table(meal_comparison_tables, 
                                                        keep_msd = FALSE, 
                                                        keep_genos = elite_genos, 
                                                        pheno_names = "protein_meal", 
                                                        table_caption = "Protein meal values of soybean genotypes with yield and seed oil comparable to check cultivars, and seed protein superior to check cultivars.")
  
  agronomics_table <- make_phenotype_comparison_table(ag_comparison_tables, 
                                                        keep_msd = FALSE, 
                                                        keep_genos = all_genos, 
                                                        pheno_names = c("ht", "lod", "sdwt", "sq"), 
                                                        table_caption = "Agronomic traits of soybean genotypes in yield tests 1 and 2.")
  
  lodging_table <- make_phenotype_comparison_table(lodging_comparison_tables, 
                                                   keep_msd = FALSE, 
                                                   keep_genos = high_lodging, 
                                                   pheno_names = "lod", 
                                                   table_caption = "Lodging values of genotypes with significantly higher lodging scores than the yield checks of their respective test.")
  
  res <- list("elite_genos" = elite_geno_table, 
              "po_top_ten"  = high_po_table, 
              "hp_sy_table" = hp_sy_table, 
              "meal_table"  = protein_meal_table, 
              "ag_table"    = agronomics_table, 
              "lod_table"   = lodging_table)
  
  return(res)
}

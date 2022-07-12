#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param genotype_contrasts_mixed
#' @param supplementary_table_data
#' @param util_tables
#' @param exportdir
export_supplementary_tables <- function(supplementary_table_data,
                                        elite_genotype_tables,
                                        exportdir = here("exports", "data",
                                        "supplementary")) {

  
  # File path for the contrasts table (supplementary table 1)
  contrasts_path      <- paste0(exportdir, "/", "supplementary_data_1.csv")
  contrasts_path_docx <- paste0(exportdir, "/", "supplementary_table_1.docx")
  
  # File path for the table of lsmeans for the genotypes that had high protein
  # and yield, oil comparable to the checks for their tests
  # (supplementary table 2)
  meal_table_path      <- paste0(exportdir, "/", "supplementary_data_2.csv")
  meal_table_path_docx <- paste0(exportdir, "/", "supplementary_table_2.docx")
  
  # File path for the lsmeans table (supplementary table 3)
  lsmeans_path      <- paste0(exportdir, "/", "supplementary_data_3.csv")
  lsmeans_path_docx <- paste0(exportdir, "/", "supplementary_table_3.docx")
  
  # File path for the table of contrasts for the genotypes with lodging that
  # as higher than the checks for their tests
  high_lodging_path      <- paste0(exportdir, "/", "supplementary_data_4.csv")
  high_lodging_path_docx <- paste0(exportdir, "/", "supplementary_table_4.docx")
  
  # File paths
  
  # Combine all file paths into a list that can be returned by the function
  all_paths <- c(contrasts_path, 
                 contrasts_path_docx,
                 meal_table_path,
                 meal_table_path_docx,
                 lsmeans_path, 
                 lsmeans_path_docx,
                 high_lodging_path, 
                 high_lodging_path_docx)
  
  # Section properties as provided by the officer package. 
  # Basically, I want the tables to be landscape
  sect_properties <- prop_section(
    page_size = page_size(orient = "landscape",
                          width = 8.3, height = 11.7),
    type = "continuous",
    page_margins = page_mar()
  )
  
  sect_properties_long <- prop_section(
    page_size = page_size(orient = "portrait",
                          width = 8.3, height = 11.7),
    type = "continuous",
    page_margins = page_mar()
  )
  
  FitFlextableToPage <- function(ft, pgwidth = 8){
    
    ft_out <- ft %>% autofit()
    
    ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
    return(ft_out)
  }
  
  # Have to make a flextable of the contrast data
  contrast_flextable <- supplementary_table_data$contrasts_table %>% 
    mutate_if(is.numeric, round, 2) %>%
    flextable() %>% 
    theme_box() %>% 
    merge_v(j = c(1, 2)) %>% 
    flextable::footnote(i = 1, j = 1:6, 
                        ref_symbols = letters[1:6], 
                        part = "header", 
                        value = as_paragraph(c("The test which a RIL and it's associated checks belong to.", 
                                               "The phenotype .", 
                                               "The contrast label in the form RIL name - Checks.", 
                                               "The estimated difference between the RIL and the check average.", 
                                               "The standard error on the estimation of the contrast.", 
                                               "Degrees of freedom."))) %>% 
    align(align = "center", part = "header") %>%
    align(align = "center", part = "body") %>% 
    set_caption("RIL - Yield check contrasts for all genotypes and phenotype combinations with relevant summary statistics.") %>% 
    FitFlextableToPage(7.5) %>% 
    fontsize(size = 9, part = "all")
  
  # Export each dataframe to a csv file
  write_csv(supplementary_table_data$contrasts_table, contrasts_path)
  save_as_docx(contrast_flextable, path = contrasts_path_docx, pr_section = sect_properties_long)
  
  write_csv(supplementary_table_data$elite_genos_lsmeans, meal_table_path)
  save_as_docx(elite_genotype_tables$meal_table$word_table, path = meal_table_path_docx, pr_section = sect_properties)
  
  write_csv(supplementary_table_data$all_lsmeans, lsmeans_path)
  save_as_docx(elite_genotype_tables$ag_table$word_table, path = lsmeans_path_docx, pr_section = sect_properties)
  
  write_csv(supplementary_table_data$high_lodging, high_lodging_path)
  save_as_docx(elite_genotype_tables$lod_table$word_table, path = high_lodging_path_docx, pr_section = sect_properties)
  
  # And return th paths to these files
  return(all_paths)
}

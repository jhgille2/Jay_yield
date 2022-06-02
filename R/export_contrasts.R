#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param genotype_contrasts_mixed
#' @param util_tables
export_contrasts <- function(genotype_contrasts_mixed, util_tables, exportdir = here("exports", "data", "supplementary")) {

  # Unnest the contrasts
  contrasts_wide <- genotype_contrasts_mixed %>% 
    mutate(contrasts = map(contrasts, as_tibble)) %>% 
    select(test_name, pheno, contrasts) %>% 
    unnest(contrasts) 
  
  # Rename the variables in the pheno column
  contrasts_wide <- convert_from_table(contrasts_wide, "pheno", util_tables$trait_lookup) %>% 
    rename(`Test Name` = test_name)
  
  # Export the table
  save_path <- paste0(exportdir, "/", "supplementary_table_1.csv")
  
  write_csv(contrasts_wide, save_path)
  
  return(save_path)

}

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
                                        exportdir = here("exports", "data",
                                        "supplementary")) {

  
  # File path for the contrasts table (supplementary table 1)
  contrasts_path <- paste0(exportdir, "/", "supplementary_table_1.csv")
  
  # File path for the table of lsmeans for the genotypes that had high protein
  # and yield, oil comparable to the checks for their tests
  # (supplementary table 2)
  meal_table_path <- paste0(exportdir, "/", "supplementary_table_2.csv")
  
  # File path for the lsmeans table (supplementary table 3)
  lsmeans_path <- paste0(exportdir, "/", "supplementary_table_3.csv")
  
  # File path for the table of contrasts for the genotypes with lodging that
  # as higher than the checks for their tests
  high_lodging_path <- paste0(exportdir, "/", "supplementary_table_4.csv")
  
  # File path 
  
  # Combine all file paths into a list that can be returned by the function
  all_paths <- c(contrasts_path, 
                 meal_table_path,
                 lsmeans_path, 
                 high_lodging_path)
  
  # Export each dataframe to a csv file
  write_csv(supplementary_table_data$contrasts_table, contrasts_path)
  write_csv(supplementary_table_data$elite_genos_lsmeans, meal_table_path)
  write_csv(supplementary_table_data$all_lsmeans, lsmeans_path)
  write_csv(supplementary_table_data$high_lodging, high_lodging_path)
  
  # And return th paths to these files
  return(all_paths)
}

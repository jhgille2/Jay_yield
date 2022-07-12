#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param elite_genotype_tables
#' @param dir
export_elite_genotype_tables <- function(elite_genotype_tables, dir =
                                         here("exports", "tables",
                                         "elite_genos")) {

  # Make a vector of filepath names
  all_filepaths <- paste0(dir, "/", names(elite_genotype_tables), ".docx")
  
  export_fn <- function(table_list, file_path){
    
    # Section properties as provided by the officer package. 
    # Basically, I want the tables to be landscape
    sect_properties <- prop_section(
      page_size = page_size(orient = "landscape",
                            width = 8.3, height = 11.7),
      type = "continuous",
      page_margins = page_mar()
    )
    
    FitFlextableToPage <- function(ft, pgwidth = 11){
      
      ft_out <- ft %>% autofit()
      
      ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
      return(ft_out)
    }
    
    table_list %>% 
      pluck("word_table") %>% 
      FitFlextableToPage() %>%
      fontsize(part = "all", size = 8) %>%
      save_as_docx(path = file_path, pr_section = sect_properties)
  }
  
  walk2(elite_genotype_tables, all_filepaths, export_fn)
  
  return(all_filepaths)
}

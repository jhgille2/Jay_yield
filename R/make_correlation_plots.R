#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param genotype_BLUEs
#' @return
#' @author 'Jay
#' @export
make_correlation_plots <- function(genotype_BLUEs, pheno = c("protein", "oil", "yield", "sdwt", "md", "ht"), util_tables) {

  test_correlation <- function(test_data){
    
    test_data <- rename_with_lookup(test_data, util_tables$trait_lookup)
    
    new_var_names <- map_chr(pheno, match_from_table, conversiontable = util_tables$trait_lookup)
    
    corr_data <- corr_coef(test_data, new_var_names)
    corr_plot <- plot(corr_data)
    
    res <- list("correlation_data" = corr_data, 
                "correlation_plot" = corr_plot)
    
    return(res)
  }
  
  all_corr_data <- map(genotype_BLUEs$BLUEs, test_correlation)

  return(all_corr_data)
}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df
fit_gge_models <- function(df = all_yield_data$two_year) {

  gge_model <- function(test_data){
    
    phenos <- c("md", "ht", "lod", "oil", "protein", "yield", "sdwt", "sq", "protein_plus_oil")
    
    # A function that fits a gge model for some named phenotype
    fit_trait_model <- function(trait_name){
      model_fit <- gge(test_data, 
                       env  = ENV, 
                       gen  = genotype, 
                       resp = trait_name)
      return(model_fit)
    }
    
    # Fit a gge model for each phenotype
    pheno_gge <- map(phenos, fit_trait_model) %>% 
      set_names(phenos)
    
    return(pheno_gge)
  }
  
  # Apply the function to each list element of the df argument (each test)
  model_list <- map(df, gge_model)
  
  # And return the list of fit models
  return(model_list)
}

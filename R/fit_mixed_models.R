#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df
fit_mixed_models <- function(df = all_yield_data$two_year) {

  # Use the gamem_met function from metan to fit a mixed effect model on a tests data
  blup_model <- function(test_data){
    
    model_fit <- gamem_met(test_data,
                           env     = ENV, 
                           gen     = genotype, 
                           rep     = rep, 
                           resp    = everything(), 
                           random  = "gen",
                           verbose = FALSE)
    
    return(model_fit)
  }
  
  # Apply the function to each list element of the df argument (each test)
  model_list <- map(df, blup_model)

  # And return the list of fit models
  return(model_list)
}

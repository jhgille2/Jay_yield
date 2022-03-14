#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df
fit_gyt_model <- function(df = all_yield_data$two_year) {

  # A function to fit a gyt model to each tests data
  gyt_model <- function(test_data){
    
    model_fit <- gytb(test_data, 
                      gen       = genotype, 
                      yield     = yield, 
                      traits    = c("protein", "oil", "lod"),
                      ideotype  = c("h", "h", "l"),
                      svp       = "genotype",
                      scaling   = 1, 
                      centering = 2,
                      weight    = c(1, 1, 0.5))
    
  }
  
  # Apply the function to each list element of the df argument (each test)
  model_list <- map(df, gyt_model)
  
  # And return the list of fit models
  return(model_list)
}

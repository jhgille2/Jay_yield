#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit_models
make_blue_table <- function(fit_models = mixed_models_two_year) {
  
  # A function to extract the BLUEs from the fit models and then add the test name
  # to the summary table
  make_blue_summary <- function(fit_model, test_name){
    get_model_data(fit_model, "blupg") %>% 
      mutate(test_name = test_name) %>% 
      relocate(test_name, 1) %>% 
      rename(genotype = GEN) %>% 
      arrange(desc(yield))
  }
  
  # A function to get the GxE genotype blues
  make_ge_blue_summary <- function(fit_model, test_name){
    get_model_data(fit_model, "blupge") %>% 
      pivot_wider(names_from = ENV, values_from = c("md", 
                                                    "ht", 
                                                    "lod", 
                                                    "oil", 
                                                    "protein", 
                                                    "yield", 
                                                    "sdwt", 
                                                    "sq", 
                                                    "protein_plus_oil")) %>%
      mutate(test_name = test_name) %>% 
      relocate(test_name, 1) %>% 
      rename(genotype = GEN)
  }

  # Apply this function to the models fit in each test and 
  # return the list of summary tables
  all_blues    <- map2(fit_models, names(fit_models), make_blue_summary)
  all_blues_ge <- map2(fit_models, names(fit_models), make_ge_blue_summary)
  
  result_list <- list("BLUEs"    = all_blues, 
                      "GE_BLUEs" = all_blues_ge)
  
  return(result_list)
}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit_models
make_genotype_blue_plots <- function(fit_models = mixed_models_two_years, all_data = all_yield_data$two_year) {

  
  # A function to relevel the environment factor variable in the yield data
  # so that it is ordered by year and so that 
  reorder_environment <- function(yield_df){
    yield_df %>% 
      mutate(ENV = fct_relevel(ENV, 'CLA - 2020', 'CAS - 2020', 'CAS - 2021', 'PLY - 2021'), 
             genotype = fct_reorder(genotype, yield, .fun = mean, na.rm = TRUE)) %>% 
      rename(Genotype = genotype, 
             Environment = ENV, 
             Yield = yield)
  }
  
  all_data <- map(all_data, reorder_environment)
  
  # A genotype by environment plot for yield
  test_1_yield <- ge_plot(pluck(all_data, "Jay Test 1"), env = Environment, gen = Genotype, resp = Yield)
  test_2_yield <- ge_plot(pluck(all_data, "Jay Test 2"), env = Environment, gen = Genotype, resp = Yield)
  
  both_test_plots <- arrange_ggplot(test_1_yield, test_2_yield, 
                                    tag_levels = list(c("Test 1", "Test 2")))
  
  res <- list("ge_plot" = both_test_plots)
  
  return(res)
}

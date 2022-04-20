#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param test_anova_list
#' @param test_name
make_emmean_table <- function(test_anova_list, test_name) {
  
  # Pull just the joint anova results
  test_anova_list <- pluck(test_anova_list, "joint")
  
  # Each element if both_anova is a list of phenotype ANOVAs
  # I want a function to extract the marginal means from the 
  # phenotype
  extract_emmeans <- function(pheno_anova, pheno_name){
    pheno_anova %>% 
      pluck("model") %>% 
      emmeans("GEN") %>%
      as_tibble() %>%
      select(GEN, emmean) %>% 
      set_names(c("genotype", pheno_name))
  }
  
  all_emmeans <- map2(test_anova_list, names(test_anova_list), extract_emmeans) %>% 
    reduce(left_join) %>% 
    mutate(test_name = test_name) %>% 
    relocate(test_name, 1) %>% 
    arrange(desc(yield))
  
  return(all_emmeans)
}

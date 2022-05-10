##################################################
## Project: Jay Yield
## Script purpose: Exploring the mixed model contrasts to identify genotypes
## with similar/superior yield, protein, and oil when compared to the checks
## Date: 2022-05-10
## Author: Jay Gillenwater
##################################################

# Setting up environment
source("./packages.R")
tar_make()

tar_load(genotype_contrasts_mixed)

all_contrasts <- genotype_contrasts_mixed %>% 
  dplyr::filter(pheno %in% c("oil", "protein", "yield")) %>% 
  mutate(contrasts = map(contrasts, as_tibble)) %>% 
  select(test_name, pheno, contrasts) %>% 
  unnest(contrasts) %>% 
  group_by(test_name, pheno) %>% 
  mutate(estimate_scaled = as.numeric(scale(estimate))) %>% 
  arrange(test_name, contrast, pheno)

scaled_estimates <- all_contrasts %>% 
  group_by(test_name, contrast) %>% 
  summarise(avg_scaled_estimate = mean(estimate_scaled))
  

all_contrasts_summary <- left_join(all_contrasts, scaled_estimates, by = c("test_name", "contrast"))


all_contrasts_summary %>% 
  arrange(test_name, desc(avg_scaled_estimate)) %>% 
  View()

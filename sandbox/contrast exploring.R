##################################################
## Project: Jay Yield
## Script purpose: Explore the output of the contrast step to identify
## genotypes with yield and seed composition traits similar to, or superior
## to check cultivars
## Date: 2022-05-02
## Author: Jay Gillenwater
##################################################

# Run pipeline and load in the contrasts data
source("./packages.R")
tar_make()
tar_load(c(contrasts_linear, contrasts_mixed))

# The "main" phenotypes I'm interested in
main_pheno <- c("yield", "oil", "protein")

mixed_tbl <- contrasts_mixed %>% 
  dplyr::filter(pheno %in% main_pheno) %>% 
  select(test_name, pheno, contrasts) %>% 
  unnest(contrasts) %>% 
  group_by(test_name, pheno) %>% 
  mutate(contrast = str_remove(contrast, " - Checks")) %>% 
  dplyr::filter((p.value <= 0.05 & estimate > 0 )| p.value > 0.05) %>%
  arrange(test_name, pheno, desc(estimate))

linear_tbl <- contrasts_linear %>% 
  dplyr::filter(pheno %in% main_pheno) %>% 
  select(test_name, pheno, contrasts) %>% 
  unnest(contrasts) %>% 
  group_by(test_name, pheno) %>% 
  mutate(contrast = str_remove(contrast, " - Checks")) %>% 
  dplyr::filter((p.value <= 0.05 & estimate > 0 )| p.value > 0.05) %>%
  arrange(test_name, pheno, desc(estimate))

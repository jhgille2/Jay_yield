##################################################
## Project: Jay Yield
## Script purpose: Get marginal means from mixed models with standard errors
## Date: 2022-05-06
## Author: Jay Gillenwater
##################################################

source("./packages.R")
tar_load(c(mixed_models_two_years, util_tables))

# A function to get the marginal means from the model component of the 
# mixed models output
get_mar_means <- function(test_data, pheno_names = names(test_data)){
  
  one_pheno_means <- function(pheno_model, pheno_name){
    pheno_model %>% 
      pluck("modellme") %>% 
      emmeans("GEN") %>% 
      as_tibble() %>% 
      mutate(pheno_name = pheno_name) %>% 
      relocate(pheno_name, .after = "GEN")
  }
  
  map2(test_data, pheno_names, one_pheno_means) %>% 
    reduce(bind_rows)
  
}

# use the function to get the marginal means for both tests and then get the 
# output from just the first test
all_means <- map(mixed_models_two_years, get_mar_means)
one_test  <- all_means$`Jay Test 1`

# Get the marginal means for just the checks and get the average value for
# each phenotype
check_means <- one_test %>% 
  dplyr::filter(GEN %in% util_tables$yield_checks$genotype) %>% 
  group_by(pheno_name) %>% 
  summarise(check_avg = mean(emmean))

# Add a column to the test marginal means to indicate if the value for a phenotype is
# above the average of the checks
se_table <- left_join(one_test, check_means, by = "pheno_name") %>% 
  mutate(above_avg = ifelse(emmean > check_avg, "Above check average", "Below check average")) %>% 
  select(GEN, pheno_name, emmean, SE, above_avg) %>% 
  dplyr::filter(pheno_name == "yield")


ggplot(se_table, aes(x = emmean, y = reorder(GEN, emmean), fill = above_avg)) + 
  geom_point(shape = 21, size = 3.5) + 
  geom_errorbarh(aes(xmax = emmean + SE, xmin = emmean - SE)) + 
  theme_hc() + 
  ylab("Genotype") + 
  xlab("Yield (bu/acre)") + 
  theme(legend.title = element_blank())


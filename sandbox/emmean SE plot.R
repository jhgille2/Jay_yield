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
one_test  <- all_means$`Jay Test 2`

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
  select(GEN, pheno_name, emmean, SE, above_avg, check_avg)


ggplot(se_table, aes(x = emmean, y = reorder(GEN, emmean), fill = above_avg)) + 
  geom_point(shape = 21, size = 3.5) + 
  geom_errorbarh(aes(xmax = emmean + SE, xmin = emmean - SE)) + 
  theme_hc() + 
  ylab("Genotype") + 
  xlab("Yield (bu/acre)") + 
  theme(legend.title = element_blank())

# A function to make a dotplot with error bars for some given trait
make_pheno_dotplot <- function(emmean_data = se_table, pheno = "yield", pheno_lookup = util_tables$trait_lookup){
  
  # Filter the full marginal mean data to just the data for the chosen phenotype
  pheno_data <- emmean_data %>% 
    dplyr::filter(pheno_name == pheno)
  
  # Get the longer name for the phenotype from the phenotype name utility table
  pheno_longname <- match_from_table(pheno, pheno_lookup)
  
  # The average value of the checks for the given phenotype
  pheno_check_avg <- unique(pheno_data$check_avg)
  
  # Make a dotplot with errorbars to intidate the standard error 
  p <- ggplot(pheno_data, aes(x = emmean, y = reorder(GEN, emmean), fill = above_avg)) + 
    geom_point(shape = 21, size = 3.5) + 
    geom_errorbarh(aes(xmax = emmean + SE, xmin = emmean - SE)) + 
    theme_hc() + 
    ylab("Genotype") + 
    xlab(pheno_longname) + 
    geom_vline(xintercept = pheno_check_avg, lty = 2) + 
    scale_fill_manual(values = c("blue", "red")) + 
    theme(legend.title = element_blank())
  
  return(p)
}

tar_make()
tar_load(phenotype_scatterplots)
test_2_scatterplots <- phenotype_scatterplots %>% 
  dplyr::filter(test_name == "Jay Test 2")
ggarrange(plotlist = test_2_scatterplots$scatterplot, nrow = 1, ncol = nrow(test), common.legend = TRUE, legend = "bottom")


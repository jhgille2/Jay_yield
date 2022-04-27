##################################################
## Project: Jay Yield
## Script purpose: Testing out using custom contrasts with emmeans
## Date: 2022-04-27
## Author: Jay Gillenwater
##################################################

# Load packages and the data I need for testing
source("packages.R")
tar_load(c(anova_two_years, util_tables))

# The marginal means for yield in test 1 
example_emm <- anova_two_years$`Jay Test 1`$joint$yield$model %>% emmeans("GEN")

# A function to identify the checks vs the RILs and return
# a list of named contrast lists
check_ril_contrasts <- function(mar_means, check_genotypes = util_tables$yield_checks){
  
  # Convert the emmgrid to a tibble
  means_tibble <- as_tibble(mar_means)
  
  # Identify checks and RILs with numeric vectors
  check_contrasts <- means_tibble$GEN %in% check_genotypes$genotype
  ril_contrasts   <- !check_contrasts %>% as.numeric()
  check_contrasts <- as.numeric(check_contrasts)
  
  # I'm not sure if this is accomplishing what I really want to do right now...
  # Maybe a set of contrasts for each RIL and one for the average of checks(?)
  # Need to open the stats textbook back up
  ril_contrasts   <- ril_contrasts/sum(ril_contrasts)
  check_contrasts <- check_contrasts/sum(check_contrasts)
  
  # Maybe something like this?
  ril_genos <- means_tibble$GEN[ril_contrasts != 0]
  
  # A function to return a vector with a 1 in the position of a genotype
  # name match, and a zero everywhere else
  genotype_contrast <- function(genotype, genotype_vector = means_tibble$GEN){
    
    (genotype_vector == genotype) %>% 
      as.numeric()
    
  }
  
  # A function to return a named list of genotype vs check contrasts
  genotype_minus_check_contrast <- function(genotype){
    
    ril_contrast <- genotype_contrast(genotype)
    
    res <- ril_contrast - check_contrasts
    
    return(res)
  }
  
  # Get the names for all the ril by check contrasts and then get
  # these contrasts
  contrast_names <- paste(ril_genos, "Checks", sep = " - ")
  
  all_ril_contrasts <- map(ril_genos, genotype_minus_check_contrast) %>% 
    set_names(contrast_names)
  
  res <- list("overall_means" = list("RIL - Check" = ril_contrasts - check_contrasts), 
              "individual_RIL_contrasts" = all_ril_contrasts)
  
  return(res)
}

# Apply the function to the example marginal means
example_ril_contrasts <- check_ril_contrasts(example_emm)

# And use the contrasts function to get the contrasts
contrast(example_emm, example_ril_contrasts$individual_RIL_contrasts)

test_contrast_tible <- tibble(pheno = names(anova_two_years$`Jay Test 1`$joint)) %>% 
  mutate(mar_means = map(anova_two_years$`Jay Test 1`$joint, function(x) x %>% pluck("model") %>% emmeans("GEN")), 
         contrast_vecs = map(mar_means, check_ril_contrasts), 
         contrasts = map2(mar_means, contrast_vecs, function(x, y) contrast(x, y$individual_RIL_contrasts)))

# A function to apply the above function to the nested anova list in the 
# anova_two_years object
make_contrast_tibble <- function(test_list, test_names = names(test_list)){
  
  get_pheno_contrasts <- function(test, test_name){
    
    joint_anova <- pluck(test, "joint")
    
    
    contrast_tibble <- tibble(pheno = names(joint_anova)) %>% 
      mutate(test_name = test_name, 
             mar_means = map(joint_anova, function(x) x %>% pluck("model") %>% emmeans("GEN")), 
             contrast_vecs = map(mar_means, check_ril_contrasts), 
             contrasts = map2(mar_means, contrast_vecs, function(x, y) contrast(x, y$individual_RIL_contrasts))) %>% 
      relocate(test_name, 1) %>% 
      mutate(contrasts = map(contrasts, as_tibble))
    
    return(contrast_tibble)
    
    
  }
  
  res <- map2(test_list, test_names, get_pheno_contrasts) %>% 
    reduce(bind_rows)
  
  return(res)
}

all_contrasts <- make_contrast_tibble(anova_two_years)

all_contrasts %>% 
  select(test_name, pheno, contrasts) %>%  
  unnest(contrasts) %>% 
  View()

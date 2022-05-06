##################################################
## Project: Jay Gillenwater
## Script purpose: Holds multiple functions for perfoming post-hoc comparison
## tests on linear/mixed models by using contrasts to compare RIL vs check means
## Date: 2022-04-29
## Author: Jay Gillenwater
##################################################

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


# A function to apply the above function to the nested anova list in the 
# anova_two_years object
make_contrast_tibble <- function(test_list, test_names = names(test_list), model_type = "linear", check_genos = util_tables$yield_checks){
  
  pluck_item <- ifelse(model_type == "linear", "model", 
                       ifelse(model_type == "mixed", "modellme", stop("model_type should be one of 'linear' or 'mixed'")))
  
  get_pheno_contrasts <- function(test, test_name){
    
    if(model_type == "linear"){
      model_data <- pluck(test, "joint")
    }else if(model_type == "mixed"){
      model_data <- test
    }
    
    
    contrast_tibble <- tibble(pheno = names(model_data)) %>% 
      mutate(test_name = test_name, 
             mar_means = map(model_data, function(x) x %>% pluck(pluck_item) %>% emmeans("GEN")), 
             contrast_vecs = map(mar_means, check_ril_contrasts, check_genos), 
             contrasts = map2(mar_means, contrast_vecs, function(x, y) contrast(x, y$individual_RIL_contrasts, adjust = "sidak"))) %>% 
      relocate(test_name, 1)
    
    return(contrast_tibble)
  }
  
  res <- map2(test_list, test_names, get_pheno_contrasts) %>% 
    reduce(bind_rows)
  
  return(res)
}

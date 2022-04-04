##################################################
## Project: Jay Yield
## Script purpose: Testing out the lsd.test function to 
## get least significant difference values for measured traits
## Date: 2022-04-04
## Author: Jay Gillenwater
##################################################

# Load the yield data
tar_load(all_yield_data)

# Laod packages
source("./packages.R")

# A function to fit a linear model with the yield data for one test
fit_lm <- function(test_data){
  
  # Phenotypes to fit a model on
  measure_vars <- c("md", 
                    "ht", 
                    "lod", 
                    "oil", 
                    "protein", 
                    "yield", 
                    "sdwt", 
                    "sq", 
                    "protein_plus_oil")
  
  # A function to apply to the nested data column to remove any observations
  # with missing data
  clean_data <- function(pheno_data){
    
    pheno_data %<>%
      dplyr::filter(!is.na(value), !genotype %in% c("LMN09-119", "N09-09"))
    return(pheno_data)
    
  }
  
  # Pivot the data by these phenotypes and then nest by each phenotype
  test_data %<>%
    pivot_longer(cols = measure_vars, names_to = "trait") %>%
    group_by(trait) %>% 
    nest() %>% 
    ungroup() %>%
    mutate(data = map(data, clean_data))
  
  model_fn <- function(pheno_data) {
    
    # Fit a linear model on the data, perform an analysis of variance, 
    # and then perform a least significant difference test
    model     <- with(pheno_data, lm(value~ genotype + genotype:ENV + ENV/rep))
    model_aov <- aov(model)
    trait_lsd <- agricolae::LSD.test(model_aov, 
                                     trt   = "genotype", 
                                     p.adj = "bonferroni")
    
    # Group the model, aov, and lsd test into a list and return this list
    res <- list("model"     = model, 
                "model_aov" = model_aov, 
                "lsd"       = trait_lsd)
    
    return(res)
  }
  
  # Apply the model fitting function to the nested data column and return
  # the original data with this a new column added to hold the results
  test_data %<>% 
    mutate(fit_model = map(data, model_fn))
  
  return(test_data)
}

all_models <- map(all_yield_data$two_year, fit_lm)

## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  ## Section: Input files
  ##################################################
  
  # Yield data from 2019
  tar_target(yield_2019_file, 
             here("data", "season_2019", "YieldData_2019.xlsx"), 
             format = "file"),
  
  # 2020 Caswell and Clayton maturity dates
  tar_target(cas_maturity_file, 
             here("data", "season_2020", "maturity_dates_CAS.xlsx"), 
             format = "file"), 
  
  tar_target(cla_maturity_file, 
             here("data", "season_2020", "maturity_dates_CLA.xlsx"), 
             format = "file"), 
  
  # NIR data from 2020
  tar_target(nir_2020_file, 
             list.files(here("data", "season_2020", "nir"), full.names = TRUE), 
             format = "file"),
  
  # Yield data from 2020
  tar_target(yield_2020_file, 
             list.files(here("data", "season_2020", "yield"), full.names = TRUE), 
             format = "file"),
  
  # Yield data from 2021
  tar_target(yield_2021_file, 
             list.files(here("data", "season_2021"), full.names = TRUE), 
             format = "file"), 
  
  ## Section: Data cleaning
  ##################################################
  
  # Cleaning up and merging the data from the three seasons
  tar_target(all_yield_data, 
             merge_yield_data(yield_2019_file, 
                              cas_maturity_file, 
                              cla_maturity_file, 
                              nir_2020_file,
                              yield_2020_file, 
                              yield_2021_file)),
  
  ## Section: Analysis
  ##################################################
  
  # Mixed effects models
  tar_target(mixed_models_two_years, 
             fit_mixed_models(df = all_yield_data$two_year)),
  
  # GGE model
  tar_target(gge_models_two_years, 
             fit_gge_models(df = all_yield_data$two_year)),
  
  # Genotype by trait gge model
  tar_target(gyt_model_two_years, 
             fit_gyt_model(df = all_yield_data$two_year)),
  
  
  ## Section: Analysis summaries
  ##################################################
  
  # A table of genotype BLUPs for each trait/test
  tar_target(genotype_BLUEs, 
             make_blue_table(fit_models = mixed_models_two_years))

  
)

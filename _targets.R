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
             here("data", "season_2020", "nir", "NIRData_2020.xlsx"), 
             format = "file"),
  
  # Yield data from 2020
  tar_target(yield_2020_file, 
             here("data", "season_2020", "yield", "YieldData_2020.xlsx"), 
             format = "file"),
  
  # Yield data from 2021
  tar_target(yield_2021_file, 
             list.files(here("data", "season_2021"), full.names = TRUE), 
             format = "file"), 
  
  # Check genotypes
  tar_target(yield_check_genotypes, 
             here("data", "utils", "check_genotypes.xlsx"), 
             format = "file"),
  
  # A table to convert between genotype names that are coded 
  # differently between years
  tar_target(genotype_conversion, 
             here("data", "utils", "genotype_name_conversion.xlsx"), 
             format = "file"), 
  
  # A table to convert short phenotype names to publication-ready names
  tar_target(trait_conversion, 
             here("data", "utils", "trait_name_lookup.xlsx"), 
             format = "file"),
  
  # A table to convert short phenotype names to longer names that don't 
  # have units included
  tar_target(trait_shortname_conversion, 
             here("data", "utils", "trait_shortname_lookup.xlsx"), 
             format = "file"),
  
  ## Section: Data cleaning
  ##################################################
  
  # Read in the utility tables
  tar_target(util_tables, 
             read_util_tables(yield_check_genotypes, 
                              genotype_conversion, 
                              trait_conversion, 
                              trait_shortname_conversion)),
  
  # Cleaning up and merging the data from the three seasons
  tar_target(all_yield_data, 
             merge_yield_data(yield_2019_file, 
                              cas_maturity_file, 
                              cla_maturity_file, 
                              nir_2020_file,
                              yield_2020_file, 
                              yield_2021_file, 
                              util_tables)),
  
  # Assign a color to each test name to use for plotting
  tar_target(test_name_colors, 
             set_test_colors(color_ids = names(all_yield_data$all))),
  
  ## Section: Analysis
  ##################################################
  
  # ANOVA (fixed effects)
  tar_target(anova_two_years, 
             calc_anova(df = all_yield_data$two_year)),
  
  # Mixed effects models
  tar_target(mixed_models_two_years, 
             fit_mixed_models(df = all_yield_data$two_year)),
  
  # GGE model
  tar_target(gge_models_two_years, 
             fit_gge_models(df = all_yield_data$two_year)),
  
  # Genotype by trait gge model
  tar_target(gyt_model_two_years, 
             fit_gyt_model(df = all_yield_data$two_year)),
  
  # LSD/MSD tests
  tar_target(least_sig_differences, 
             get_lsd_values(df = all_yield_data$two_year)),
  
  
  ## Section: Analysis summaries
  ##################################################
  
  # A table of genotype BLUPs for each trait/test
  tar_target(genotype_BLUEs, 
             make_blue_table(fit_models = mixed_models_two_years)),
  
  # Plots of the genotype blues
  tar_target(BLUE_plots_two_years, 
             make_genotype_blue_plots(fit_models = mixed_models_two_years)),
  
# A example latex table to use for testing targets with the manuscript template
  tar_target(example_table, 
             make_example_table()),
  
  # A example plot to use for testing manuscript markdown rendering
  tar_target(example_plot, 
             make_example_plot(all_yield_data, test_name_colors, dir = here("exports", "plots", "example_plot.pdf")), 
             format = "file"),

  # Cleaner ANOVA tables
  tar_target(anova_tables, 
             make_anova_tables(anova_data = anova_two_years)),

  
  ## Section: Writeup documents
  ##################################################
  tar_render(analysis_writeup, "doc/analysis_writeup.Rmd"), 
  
  tar_render(manuscript_document, here("doc", "manuscript", "manuscript.Rmd"))
  
)

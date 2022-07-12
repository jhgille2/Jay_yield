## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  ## Section: Input files
  ##################################################
  
  # All plant row data collected from the  2018 MPs
  tar_target(plant_rows_2019_file, 
             here("data", "season_2019", "OilMPYieldSelections2019.xlsx"), 
             format = "file"),
  
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
  
  # A table to convert short column names in summary tables to longer, 
  # more descriptive names
  tar_target(column_shortname_conversion, 
             here("data", "utils", "summary_column_names.xlsx"), 
             format = "file"),
  
  # A flowchart to show the population development as a graphviz file
  tar_target(pop_development_flowchart, 
             here("graphviz_docs", "graph.gv"), 
             format = "file"),
  
  # Export this flowchart as a pdf
  tar_target(pop_development_pdf, 
             export_pop_development_pdf(pop_development_flowchart), 
             format = "file"),
  
  ## Section: Data cleaning
  ##################################################
  
  # Read in the utility tables
  tar_target(util_tables, 
             read_util_tables(yield_check_genotypes, 
                              genotype_conversion, 
                              trait_conversion, 
                              trait_shortname_conversion, 
                              column_shortname_conversion)),
  
  # Read in and clean up the plant row data from 2019
  tar_target(plant_row_data, 
             read_plant_row_data(plant_rows_2019_file)),
  
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
  
  # Conrasts of RIL means vs check means
  tar_target(genotype_contrasts_anova, 
             make_contrast_tibble(anova_two_years, 
                                  model_type  = "linear", 
                                  check_genos = util_tables$yield_checks)), 
  
  tar_target(genotype_contrasts_mixed, 
             make_contrast_tibble(mixed_models_two_years,
                                  model_type  = "mixed", 
                                  check_genos = util_tables$yield_checks)),
  
  
  ## Section: Analysis summaries
  ##################################################
  
  # Marginal means derived from linear models
  tar_target(linear_means, 
             map2(anova_two_years, names(anova_two_years), make_emmean_table)),
  
  # A table of genotype BLUPs for each trait/test
  tar_target(genotype_BLUEs, 
             make_blue_table(fit_models = mixed_models_two_years)),
  
  # Plots of the genotype blues
  tar_target(BLUE_plots_two_years, 
             make_genotype_blue_plots(fit_models = mixed_models_two_years, 
                                      all_data   = all_yield_data$two_year, 
                                      util_tables)),
  
  # Make supplementary tables 
  tar_target(supplementary_table_data, 
             make_supplementary_tables(genotype_BLUEs, 
                                       genotype_contrasts_mixed, 
                                       util_tables)),
  
# A example latex table to use for testing targets with the manuscript template
  tar_target(example_table, 
             make_example_table()),
  
  # A example plot to use for testing manuscript markdown rendering
  tar_target(example_plot, 
             make_example_plot(all_yield_data, 
                               test_name_colors, 
                               dir = here("exports", "plots", "example_plot.pdf")), 
             format = "file"),

  # Cleaner ANOVA tables
  tar_target(anova_tables, 
             make_anova_tables(anova_data = anova_two_years)),

  # Scatterplots to show phenotype marginal means and the standard errors on those
  # means 
  tar_target(phenotype_scatterplots, 
             make_phenotype_scatterplots(mixed_models_two_years, util_tables)),

  # Correlation plots
  tar_target(correlation_plots, 
             make_correlation_plots(genotype_BLUEs, 
                                    pheno = c("protein", "oil", "yield", "sdwt", "md", "ht", "protein_meal"), 
                                    util_tables)),

  # Latex tables of summary data for the yield marginal means
  tar_target(yield_summary_tables, 
             make_yield_summary_tables(blue_data      = genotype_BLUEs, 
                                       utility_tables = util_tables, 
                                       lsd_data       = least_sig_differences, 
                                       linear_means   = linear_means, 
                                       summary_phenos = c("yield", "protein", "oil"))),

  # Summary tables for selections of elite genotypes
tar_target(elite_genotype_tables, 
           make_elite_summary_tables(blue_data      = genotype_BLUEs,
                                     utility_tables = util_tables, 
                                     lsd_data       = least_sig_differences)),

# Export all of these tables as word documents
tar_target(elite_genotype_table_exports, 
           export_elite_genotype_tables(elite_genotype_tables, dir = here("exports", "tables", "elite_genos")), 
           format = "file"),
            
  # Histograms for each test with a selection of genotypes labelled for each test
  tar_target(labelled_test_histograms, 
             make_test_histograms(geno_means = genotype_BLUEs$BLUEs, 
                                  util_tables, 
                                  test_name_colors, 
                                  test_1_genos = c("N18-1632-2", "N18-1635", "N18-1855", "N18-1620", "N18-1674"), 
                                  test_2_genos = c("N18-1615", "N18-1796", "N18-1579", "N18-1586", "N18-1761"))),

  # Export plots 
  tar_target(phenotype_summary_plots, 
             export_phenotype_plots(BLUE_plots_two_years, 
                                    phenotype_scatterplots, 
                                    labelled_test_histograms,
                                    correlation_plots,
                                    export_dir = here("exports", "plots")), 
             format = "file"),

# A Export supplementary tables
tar_target(supplementary_tables_export, 
           export_supplementary_tables(supplementary_table_data,
                                       elite_genotype_tables,
                                       exportdir = here("exports", "data", "supplementary")), 
           format = "file"),




  
  ## Section: Writeup documents
  ##################################################
  tar_render(analysis_writeup, here("doc", "analysis_writeup.Rmd")), 

  # tar_render(overview_document, here("doc", "overview_document.Rmd")),
  
  tar_render(manuscript_document, here("doc", "manuscript", "manuscript.Rmd")), 

  tar_render(manuscript_docx_copy, here("doc", "manuscript_docx_copy.Rmd"))
)

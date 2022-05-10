#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit_models
make_genotype_blue_plots <- function(fit_models = mixed_models_two_years, all_data = all_yield_data$two_year, util_tables) {

  
  # A function to relevel the environment factor variable in the yield data
  # so that it is ordered by year and so that 
  reorder_environment <- function(yield_df){
    yield_df %>% 
      mutate(ENV = fct_relevel(ENV, 'CLA - 2020', 'CAS - 2020', 'CAS - 2021', 'PLY - 2021'), 
             genotype = fct_reorder(genotype, yield, .fun = mean, na.rm = TRUE)) %>% 
      rename(Genotype = genotype, 
             Environment = ENV)
  }
  
  all_data <- map(all_data, reorder_environment)
  
  # Get the minimum and maximum yield to set a common scale limit to both plots
  combined_data <- all_data %>% 
    reduce(bind_rows)
  
  yield_range <- range(combined_data$yield, na.rm = TRUE)
  
  yield_scale_min <- yield_range[[1]] - 5
  yield_scale_max <- yield_range[[2]] + 5
  
  # A genotype by environment plot for yield
  test_1_yield <- ge_plot(pluck(all_data, "Jay Test 1"), env = Environment, gen = Genotype, resp = yield) + 
    scale_fill_viridis_c(limits = c(yield_scale_min, yield_scale_max))
  
  test_2_yield <- ge_plot(pluck(all_data, "Jay Test 2"), env = Environment, gen = Genotype, resp = yield) + 
    scale_fill_viridis_c(limits = c(yield_scale_min, yield_scale_max))
  
  both_test_plots <- arrange_ggplot(test_1_yield, test_2_yield, 
                                    tag_levels      = list(c("Test 1", "Test 2")), 
                                    guides          = "collect", 
                                    legend.position = "right")
  
  # A function to make a side-by-side plot of heatmaps for a supplied set of phenotypes
  make_side_by_side_heatmaps <- function(test_data, phenotypes = c("oil", "protein", "yield")){
    
    # New (fancier) trait names for each phenotype
     new_pheno_names <- map_chr(phenotypes, match_from_table, conversiontable = util_tables$trait_lookup) %>% 
       set_names(phenotypes)
    
    # Rename the phenotypes to their fancier names
     test_data %<>%
       rename_with_lookup(util_tables$trait_lookup)
    
    one_pheno_heatmap <- function(pheno_name, pheno_data){
      
      # Get the fancy name of the current phenotype from the short name
      current_pheno_name <- new_pheno_names[pheno_name]
      
      # Remove any NA's from the current phenotype (interferes with the heatmap)
      pheno_data %<>%
        dplyr::filter(!is.na(!!sym(current_pheno_name)))
      
      pheno_data %<>% 
        mutate(Genotype = fct_reorder(Genotype, !!sym(current_pheno_name), .fun = mean, na.rm = TRUE))
      
      # Make the heatmap for the current phenotype
      p <- ge_plot(pheno_data, env = Environment, gen = Genotype, resp = !!sym(current_pheno_name)) + 
        theme(legend.position = "bottom") + 
        guides(fill = guide_colourbar(label = TRUE,
                                      draw.ulim = TRUE,
                                      draw.llim = TRUE,
                                      frame.colour = "black",
                                      ticks = TRUE,
                                      nbin = 10,
                                      label.position = "bottom",
                                      barwidth = 10,
                                      barheight = 1.3,
                                      direction = 'horizontal'))
        
        
      
      return(p)
    }
    
    # Get a heatmap for each phenotype in the current test data
    all_pheno_heatmaps <- map(phenotypes, one_pheno_heatmap, pheno_data = test_data)
    
    # Arrange all the heatmaps in one ggplot
    # all_heatmaps_arranged <- ggarrange(plotlist = all_pheno_heatmaps, ncol = length(all_pheno_heatmaps), nrow = 1, legend = "bottom")
    
    all_heatmaps_arranged <- all_pheno_heatmaps[[1]] | all_pheno_heatmaps[[2]] | all_pheno_heatmaps[[3]]
    
    return(all_heatmaps_arranged)
  }
  
  # Use the function above to make side-by-side heatmaps for each test
  test_heatmaps <- map(all_data, make_side_by_side_heatmaps)
  
  res <- list("ge_plot_both" = both_test_plots, 
              "test_1"       = test_1_yield, 
              "test_2"       = test_2_yield, 
              "side_by_side" = test_heatmaps)
  
  return(res)
}

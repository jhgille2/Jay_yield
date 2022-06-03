#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mixed_models_two_years
#' @param util_tables
make_phenotype_scatterplots <- function(mixed_models_two_years, util_tables, phenos = c("protein", "oil", "yield")) {

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
  
  # A function to make a table of marginal means with genotypes labelled 
  # according to if they had phenotype values greater than or less than 
  # the average value of the checks
  make_se_table <- function(test_data, test_name){
    
    # Get the marginal means for just the checks and get the average value for
    # each phenotype
    check_means <- test_data %>% 
      dplyr::filter(GEN %in% util_tables$yield_checks$genotype) %>% 
      group_by(pheno_name) %>% 
      summarise(check_avg = mean(emmean))
    
    # Add a column to the test marginal means to indicate if the value for a phenotype is
    # above the average of the checks
    se_table <- left_join(test_data, check_means, by = "pheno_name") %>% 
      mutate(above_avg = ifelse(emmean > check_avg, "Above check average", "Below check average")) %>% 
      select(GEN, pheno_name, emmean, SE, above_avg, check_avg) %>% 
      mutate(test_name = test_name) %>% 
      relocate(test_name, .before = GEN)
    
    return(se_table)
  }
  
  # Apply this function to the means list to get a list of means tables
  # that are ready for ggplot
  all_se_tables <- map2(all_means, names(all_means), make_se_table)
  
  # A function to make a dotplot with error bars for some given trait
  pheno_scatterplot <- function(emmean_data = se_table, pheno = "yield", pheno_lookup = util_tables$trait_lookup){
    
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
      theme_few() + 
      ylab("Genotype") + 
      xlab(pheno_longname) + 
      geom_vline(xintercept = pheno_check_avg, lty = 2, size = 1.1) + 
      scale_fill_manual(values = c("blue", "red")) + 
      theme(legend.title = element_blank(), 
            panel.grid.major.y = element_line(colour = "light grey", size = 0.25), 
            legend.text = element_text(size = 20)) + 
      theme_jay_yield_base()
    
    return(p)
  }
  
  # Make a dataframe ready to apply the pheno_scatterplot function to 
  plot_df <- crossing(emmean_data = all_se_tables, pheno = phenos)
  
  # And apply the scatterplot function to the new dataframe
  plot_df %<>% 
    mutate(scatterplot = pmap(., pheno_scatterplot), 
           test_name   = map_chr(emmean_data, function(x) unique(x$test_name))) %>% 
    select(test_name, pheno, scatterplot)
  
  # A function to make a side-by-side plot of all the phenotypes that a plot
  # was made for for each test
  arrange_test_plots <- function(test_data = plot_df){
    
    # Split the plot data by test
    split_data <- test_data %>% 
      metan::split_factors(test_name)
    
    arrange_pheno_plots <- function(split_test_data = split_data, keep_legend = TRUE){
      
      legend_string <- ifelse(keep_legend, "bottom", "none")
      
      # Arrange the scatterplots horizontally beside each other
      p <- ggarrange(plotlist      = split_test_data$scatterplot, 
                     nrow          = 1, 
                     ncol          = nrow(split_test_data), 
                     common.legend = TRUE, 
                     legend        = legend_string)
      
      # Add a title that is just the test name
      # annotate_figure(p, top = text_grob(test_name, face = "bold", size = 16))
      return(p)
    }
    
    # Apply the function to the split data to make a set of side-by-side plots
    # for each phenotype
    split_data_plots <- map(split_data, arrange_pheno_plots) %>% 
      set_names(names(split_data))
    
    # And do this separately for each plot so that the first one has no legend
    test_1_no_legend <- arrange_pheno_plots(split_data[[1]], keep_legend = FALSE)
    test_2           <- arrange_pheno_plots(split_data[[2]])
    
    both_tests <- list(test_1_no_legend, test_2)
    
    combined_plot <- plot_grid(plotlist = both_tests, 
                               labels = list("A", "B"), 
                               ncol = 1, nrow = 2) 
    
    res <- list("side_by_side" = split_data_plots, 
                "combined"     = combined_plot)
    
    return(res)
  }
  
  # Use the function above to make two plots (one for each test) that have all
  # the phenotypes together.
  side_by_side_plots <- arrange_test_plots(plot_df)
  
  # Organize the side-by-side plot list and the full plot dataframe into a list
  # that is then returned
  res <- list("side_by_side" = side_by_side_plots$side_by_side,
              "combined"     = side_by_side_plots$combined,
              "all_plots"    = plot_df)
  
  return(res)
}

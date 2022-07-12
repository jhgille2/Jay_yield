#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param BLUE_plots_two_years
#' @param phenotype_scatterplots
export_phenotype_plots <- function(BLUE_plots_two_years, phenotype_scatterplots, labelled_test_histograms, correlation_plots, export_dir = here("exports", "plots")) {

  
  ## Section: Scatterplots
  ##################################################
  
  # A function to export the scatterplots
  export_scatterplots <- function(scatterplots, test_name, file_ext = "pdf"){
    
    export_one_scatterplot <- function(scatterplot, test_name){
      
      # Get a filepath to save the plot to
      current_filename <- paste0(export_dir, "/", test_name, "_scatterplot.", file_ext)
      
      # Save the plot to the filepath
      ggsave(filename = current_filename, 
             plot = scatterplot, 
             device = file_ext, 
             width = 20, 
             height = 7.5, 
             units = "in")
      
      # And return the path
      return(current_filename)
    }
    
    map2_chr(scatterplots, names(scatterplots), export_one_scatterplot)
    
  }
  
  # All filepaths to the saved scatterplots
  all_scatterplots <- export_scatterplots(phenotype_scatterplots$side_by_side)
  
  both_plots_scatter_filepath     <- paste0(export_dir, "/", "lsmean_scatterplot.pdf")
  both_plots_scatter_filepath_png <- paste0(export_dir, "/", "lsmean_scatterplot.png")
  
  ggsave(filename = both_plots_scatter_filepath, 
         plot = phenotype_scatterplots$combined, 
         device = "pdf", 
         width = 12, 
         height = 10, 
         units = "in", 
         dpi = 800)
  
  ggsave(filename = both_plots_scatter_filepath_png, 
         plot = phenotype_scatterplots$combined, 
         device = "png", 
         width = 12, 
         height = 10, 
         units = "in", 
         dpi = 1800)
  
  
  ## Section: Labelled histograms
  ##################################################
  
  export_histograms <- function(histograms, test_name, file_ext = "pdf"){
    
    export_one_histogram <- function(histogram, test_name){
      
      # Get a filepath to save the plot to
      current_filename <- paste0(export_dir, "/", test_name, ".", file_ext)
      
      # Save the plot to the filepath
      ggsave(filename = current_filename, 
             plot = histogram, 
             device = file_ext, 
             width = 15, 
             height = 10, 
             units = "in")
      
      # And return the path
      return(current_filename)
    }
    
    map2_chr(histograms, names(histograms), export_one_histogram)
    
  }
  
  all_histograms <- export_histograms(labelled_test_histograms)
  
  ## Section: Scatterplots/heatmaps
  ##################################################
  
  # Marginal mean scatterplots over heatmap plots for tests 1 and 2
  scatter_heatmap_test1 <- phenotype_scatterplots$side_by_side$`Jay Test 1` / BLUE_plots_two_years$side_by_side$`Jay Test 1`
  
  scatter_heatmap_test2 <- phenotype_scatterplots$side_by_side$`Jay Test 2` / BLUE_plots_two_years$side_by_side$`Jay Test 2`
  
  scatter_heatmap_all <- list(scatter_heatmap_test1, scatter_heatmap_test2) %>% 
    set_names(c("Test_1", "Test_2"))
  
  # A function to export the scatter heatmaps
  export_scatter_heatmaps <- function(scatter_heatmap, test_name, file_ext = "pdf"){
    
    # Get a filepath to save the plot to
    current_filename <- paste0(export_dir, "/", test_name, "_scatter_heatmap.", file_ext)
    
    # Save the plot to the filepath
    ggsave(filename = current_filename, 
           plot = scatter_heatmap, 
           device = file_ext, 
           width = 20, 
           height = 15, 
           units = "in")
    
    # And return the path
    return(current_filename)
  }
  
  # Correlation plots
  export_correlation_plots <- function(correlation_data, test_name, file_ext = "pdf"){
    
    # Get a filepath to save the plot to
    current_filename <- paste0(export_dir, "/supplemental/", test_name, file_ext)
    
    current_plot <- pluck(correlation_data, "correlation_plot")
    
    # Save the plot to the filepath
    ggsave(filename = current_filename, 
           plot     = current_plot, 
           device   = file_ext, 
           width    = 8, 
           height   = 8, 
           units    = "in")
    
    # And return the path
    return(current_filename)
  }
  
  # all_correlation_plots <- map2_chr(correlation_plots, names(correlation_plots), export_correlation_plots)
  
  Corr_plot_test1 <- export_correlation_plots(correlation_plots$`Jay Test 1`, test_name = "supplemental_figure_1.")
  Corr_plot_test2 <- export_correlation_plots(correlation_plots$`Jay Test 2`, test_name = "supplemental_figure_2.")
  
  # Export all plots
  all_scatter_heatmaps <- map2_chr(scatter_heatmap_all, names(scatter_heatmap_all), export_scatter_heatmaps)

  # Return all the filepaths of the saved plots
  return(c(all_scatter_heatmaps, 
           all_scatterplots, 
           all_histograms, 
           both_plots_scatter_filepath,
           both_plots_scatter_filepath_png,
           Corr_plot_test1, 
           Corr_plot_test2))
}

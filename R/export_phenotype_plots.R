#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param BLUE_plots_two_years
#' @param phenotype_scatterplots
export_phenotype_plots <- function(BLUE_plots_two_years, phenotype_scatterplots, labelled_test_histograms, export_dir = here("exports", "plots")) {

  
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
  
  
  # Export all plots
  all_scatter_heatmaps <- map2_chr(scatter_heatmap_all, names(scatter_heatmap_all), export_scatter_heatmaps)

  # Return all the filepaths of the saved plots
  return(c(all_scatter_heatmaps, 
           all_scatterplots, 
           all_histograms))
}

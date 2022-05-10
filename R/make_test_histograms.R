#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
#' @param util_tables
#' @param test_name_colors
#' @param test_1_genos
#' @param test_2_genos
make_test_histograms <- function(geno_means = genotype_BLUEs$BLUEs, util_tables,
                                 test_name_colors, test_1_genos =
                                 c("N18-1632-2", "N18-1635", "N18-1855",
                                 "N18-1620", "N18-1674"), test_2_genos =
                                 c("N18-1615", "N18-1796", "N18-1579",
                                 "N18-1586", "N18-1761")) {

  # A function to make a labelled histogram
  labelled_histogram <- function(blue_data, test_colors, trait_name, check_genotypes = util_tables$yield_checks, trait_name_lookup, label_genotypes = NULL){
    
    # Use the lookup table to convert the trait name to its fancy counterpart
    fancy_trait_name <- match_from_table(trait_name, trait_name_lookup)
    test_name        <- "test_name"
    
    # Get data for the parents/checks
    CheckParents <- blue_data %>% dplyr::filter(genotype %in% check_genotypes$genotype)
    CheckParents <- CheckParents[, c("genotype", trait_name)]
    colnames(CheckParents) <- c("genotype", "value")  
    
    CheckAvg <- mean(CheckParents$value, na.rm = TRUE)
    TestAvg  <- mean(unlist(blue_data[, trait_name]), na.rm = TRUE)
    
    # The initial plot (need bin heights).
    # Add a black dotted line for check average, a red dashed line for the test average
    Plot.init <- ggplot(blue_data, aes_string(x = trait_name, fill = test_name)) + 
      geom_histogram(bins = 9, col = 'black') + 
      theme_calc() + 
      theme(axis.text  = element_text(size = 20, face = 'bold'),
            axis.title = element_text(size = 30)) + 
      ylab("Count") + 
      xlab(fancy_trait_name) + 
      geom_vline(xintercept = CheckAvg, linetype = 'dotted', colour = 'black', size = 1.25) + 
      geom_vline(xintercept = TestAvg, linetype = 2, colour = 'red', size = 1.25)
    
    # Get data for the other genotypes to label
    LabelGenotypes <- blue_data %>% dplyr::filter(genotype %in% label_genotypes)
    LabelGenotypes <- LabelGenotypes[, c("genotype", trait_name)]
    colnames(LabelGenotypes) <- c("genotype", "value") 
    
    # Data from the initial plot (I want the bin heights)
    PlotData <- ggplot_build(Plot.init)$data[[1]]
    
    # Using the bin counts from the plot, find the y-value where the labels for each
    # check/parent genotype should start
    if(nrow(LabelGenotypes) > 0){
      LabelGenotypes$yval <- NA
      for(i in 1:nrow(LabelGenotypes)){
        LabelGenotypes$yval[[i]] <- PlotData$count[[max(which(PlotData$xmin < LabelGenotypes$value[[i]]))]]
      }
      # CheckParents <- bind_rows(CheckParents, LabelGenotypes)
      CheckParents <- LabelGenotypes
      
      Plot.Final <- Plot.init + 
        ylim(c(0, 12)) + 
        ggrepel::geom_label_repel(data = CheckParents,
                                  aes(x = value, y = yval, label = genotype),
                                  nudge_y = max(PlotData$count)/3,
                                  arrow = arrow(length = unit(0.015, "npc")),
                                  min.segment.length = 0,
                                  size = 5, 
                                  fill = "white") +
        coord_cartesian(xlim = c(min(unlist(blue_data[, trait_name])) - 2, max(unlist(blue_data[, trait_name])) + 2), expand = FALSE) + 
        scale_fill_manual(values = test_colors) + 
        theme(legend.position = "none") + 
        theme(plot.margin = margin(0.5,0.5,0.75,0.6, "cm"))
    }else{
      Plot.Final <- Plot.init + 
        ylim(c(0, 12)) + 
        coord_cartesian(xlim = c(min(unlist(blue_data[, trait_name])) - 2, max(unlist(blue_data[, trait_name])) + 2), expand = FALSE) + 
        scale_fill_manual(values = test_colors) + 
        theme(legend.position = "none") + 
        theme(plot.margin = margin(0.5,0.5,0.75,0.6, "cm"))
    }
    
    Plot.Final
    
  }
  
  # A function to make a patchwork plot with protein, oil, and yield
  make_patchwork_plot <- function(means_data, label_genos){
    
    # Make plots for oil, protein, and yield with the genotypes labelled
    test_oil <- labelled_histogram(means_data, 
                                   test_colors       = test_name_colors, 
                                   trait_name        = "oil", 
                                   check_genotypes   = util_tables$yield_checks, 
                                   trait_name_lookup = util_tables$trait_lookup, 
                                   label_genotypes   = label_genos)
    
    test_pro <- labelled_histogram(means_data, 
                                   test_colors       = test_name_colors, 
                                   trait_name        = "protein", 
                                   check_genotypes   = util_tables$yield_checks, 
                                   trait_name_lookup = util_tables$trait_lookup, 
                                   label_genotypes   = label_genos)
    
    test_yield <- labelled_histogram(means_data, 
                                     test_colors       = test_name_colors, 
                                     trait_name        = "yield", 
                                     check_genotypes   = util_tables$yield_checks, 
                                     trait_name_lookup = util_tables$trait_lookup, 
                                     label_genotypes   = label_genos)
    
    # Patchwork of the three plots with protein and oil on top and yield on the bottom
    p <- (test_oil | test_pro)/(test_yield)
    
    return(p)
  }
  
  # Use the function above to make combined plots for each test
  test_1_patchwork <- make_patchwork_plot(geno_means$`Jay Test 1`, label_genos = test_1_genos)
  test_2_patchwork <- make_patchwork_plot(geno_means$`Jay Test 2`, label_genos = test_2_genos)
  
  # Combine both plots into a list and return the list
  res <- list("test_1_histograms" = test_1_patchwork, 
              "test_2_histograms" = test_2_patchwork)
  
  return(res)
}

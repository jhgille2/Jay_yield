##################################################
## Project: Jay Yield
## Script purpose: Development of function(s) for annotated histograms
## Date: 2022-04-01
## Author: Jay Gillenwater
##################################################

# Load in the data to use in the histograms
tar_load(util_tables)
tar_load(genotype_BLUEs)
tar_load(all_yield_data)
tar_load(test_name_colors)

# Combine all the yield data into a single data frame
all_yield <- reduce(all_yield_data$all, bind_rows)

# A function to replace values in a column with their match from a 
# conversion table. The current value in the data column should be one of 
# the values in the first column of the conversion table and the second
# column should hold what the value should be converted to
convert_from_table <- function(data, var, conversiontable){
  
  all_matches   <- match(unlist(data[, var]), unlist(conversiontable[, 1]))
  match_indices <- which(!is.na(all_matches))
  
  new_var <- as.character(unlist(data[, var]))
  
  new_var[match_indices] <- unlist(conversiontable[, 2])[all_matches[match_indices]]
  
  data[, var] <- new_var
  
  return(data)
}

# Use the genotype conversion utility table to standardize the check genotype names
convert_from_table(all_yield, "genotype", util_tables$genotype_conversion_table)

tar_load(genotype_BLUEs)

# A function to make a labelled histogram
labelled_histogram <- function(blue_data, test_colors, trait_name, check_genotypes, trait_name_lookup, label_genotypes = NULL){
  
  # Use the lookup table to convert the trait name to its fancy counterpart
  fancy_trait_name <- match_from_table(trait_name, trait_name_lookup)
  test_name        <- "test_name"
  
  # Get data for the parents/checks
  CheckParents <- blue_data %>% dplyr::filter(genotype %in% c(check_genotypes$genotype, label_genotypes))
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
  # check/parent genotype shoud start
  # CheckParents$yval <- NA
  # for(i in 1:nrow(CheckParents)){
  #   CheckParents$yval[[i]] <- PlotData$count[[max(which(PlotData$xmin < CheckParents$value[[i]]))]]
  # }
  
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
  
  # Add labels w/arrows for the parents/checks using this new data
  # c(max(PlotData$count) + max(PlotData$count)/5)


  
  # Add additional genotypes if requested
  # if(nrow(LabelGenotypes) > 0){
  #   Plot.Final <- Plot.Final +
  #     ggrepel::geom_label_repel(data = LabelGenotypes,
  #                               aes(x = value, y = yval, label = genotype),
  #                               nudge_y = max(PlotData$count)/3,
  #                               arrow = arrow(length = unit(0.015, "npc")),
  #                               min.segment.length = 0,
  #                               size = 8)
  # }
  
  Plot.Final
  
}

# See the ./R/utils.R file for this function definition.
# Basically uses a table to convert short variable names to more
# publication ready names
match_from_table("md", util_tables$trait_lookup)

# A function to make a patchwork plot with protein, oil, and yield
make_patchwork_plot <- function(means_data, label_genos){
  
  # Make plots for oil, protein, and yield with the genotypes labelled
  test_oil <- labelled_histogram(means_data, 
                                   test_colors       = test_name_colors, 
                                   trait_name        = "oil", 
                                   check_genotypes   = util_tables$check_table, 
                                   trait_name_lookup = util_tables$trait_lookup, 
                                   label_genotypes   = label_genos)
  
  test_pro <- labelled_histogram(means_data, 
                                   test_colors       = test_name_colors, 
                                   trait_name        = "protein", 
                                   check_genotypes   = util_tables$check_table, 
                                   trait_name_lookup = util_tables$trait_lookup, 
                                   label_genotypes   = label_genos)
  
  test_yield <- labelled_histogram(means_data, 
                                     test_colors       = test_name_colors, 
                                     trait_name        = "yield", 
                                     check_genotypes   = util_tables$check_table, 
                                     trait_name_lookup = util_tables$trait_lookup, 
                                     label_genotypes   = label_genos)
  
  # Patchwork of the three plots with protein and oil on top and yield on the bottom
  p <- (test_oil | test_pro)/(test_yield)
  
  return(p)
}


test_1_oil <- labelled_histogram(linear_means$`Jay Test 2`, 
                                 test_colors       = test_name_colors, 
                                 trait_name        = "oil", 
                                 check_genotypes   = util_tables$check_table, 
                                 trait_name_lookup = util_tables$trait_lookup, 
                                 label_genotypes   = c("N18-1627", "N18-1579", "N18-1796"))

test_1_pro <- labelled_histogram(linear_means$`Jay Test 2`, 
                                 test_colors       = test_name_colors, 
                                 trait_name        = "protein", 
                                 check_genotypes   = util_tables$check_table, 
                                 trait_name_lookup = util_tables$trait_lookup, 
                                 label_genotypes   = c("N18-1627", "N18-1579", "N18-1796"))

test_1_yield <- labelled_histogram(linear_means$`Jay Test 2`, 
                                   test_colors       = test_name_colors, 
                                   trait_name        = "yield", 
                                   check_genotypes   = util_tables$check_table, 
                                   trait_name_lookup = util_tables$trait_lookup, 
                                   label_genotypes   = c("N18-1627", "N18-1579", "N18-1796"))

test_1_po <- labelled_histogram(linear_means$`Jay Test 2`, 
                                   test_colors       = test_name_colors, 
                                   trait_name        = "protein_plus_oil", 
                                   check_genotypes   = util_tables$check_table, 
                                   trait_name_lookup = util_tables$trait_lookup, 
                                   label_genotypes   = c("N18-1627", "N18-1579", "N18-1796"))

test_1_yield/test_1_po

(test_1_oil | test_1_pro)/(test_1_po | test_1_yield)

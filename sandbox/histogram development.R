tar_load(util_tables)
tar_load(genotype_BLUEs)
tar_load(all_yield_data)
tar_load(test_name_colors)

all_yield <- reduce(all_yield_data$all, bind_rows)

util_tables$genotype_conversion_table$genotype[match(all_yield$genotype, util_tables$genotype_conversion_table$old_genotype)]


convert_from_table <- function(data, var, conversiontable){
  
  all_matches   <- match(unlist(data[, var]), unlist(conversiontable[, 1]))
  match_indices <- which(!is.na(all_matches))
  
  new_var <- as.character(unlist(data[, var]))
  
  new_var[match_indices] <- unlist(conversiontable[, 2])[all_matches[match_indices]]
  
  data[, var] <- new_var
  
  return(data)
}

convert_from_table(all_yield, "genotype", util_tables$genotype_conversion_table)

tar_load(genotype_BLUEs)

# A function to make a labelled histogram
labelled_histogram <- function(blue_data, test_colors, trait_name, check_genotypes, trait_name_lookup, label_genotypes){
  
  # Use the lookup table to convert the trait name to its fancy counterpart
  fancy_trait_name <- match_from_table(trait_name, trait_name_lookup)
  test_name <- "test_name"
  
  # The initial plot (need bin heights)
  Plot.init <- ggplot(blue_data, aes_string(x = trait_name, fill = test_name)) + 
    geom_histogram(bins = 9, col = 'black') + 
    theme_bw() + 
    theme(axis.text = element_text(size = 20, face = 'bold'),
          axis.title = element_text(size = 30)) + 
    ylab("Count") + 
    xlab(fancy_trait_name)
  
  # + 
  #   geom_vline(xintercept = CheckAvg, linetype = 'dotted', colour = 'black', size = 1.25) + 
  #   geom_vline(xintercept = LSDLine, linetype = 2, colour = 'red', size = 1.25)
  
  # Get data for the parents/checks
  CheckParents <- blue_data %>% dplyr::filter(genotype %in% check_genotypes$genotype)
  CheckParents <- CheckParents[, c("genotype", trait_name)]
  colnames(CheckParents) <- c("genotype", "value")  
  
  # Get data for the other genotypes to label
  LabelGenotypes <- blue_data %>% dplyr::filter(genotype %in% label_genotypes)
  LabelGenotypes <- LabelGenotypes[, c("genotype", trait_name)]
  colnames(LabelGenotypes) <- c("genotype", "value") 
  
  # Data from the initial plot (I want the bin heights)
  PlotData <- ggplot_build(Plot.init)$data[[1]]
  
  # Using the bin counts from the plot, find the y-value where the labels for each
  # check/parent genotype shoud start
  CheckParents$yval <- NA
  for(i in 1:nrow(CheckParents)){
    CheckParents$yval[[i]] <- PlotData$count[[max(which(PlotData$xmin < CheckParents$value[[i]]))]]
  }
  
  if(nrow(LabelGenotypes) > 0){
    LabelGenotypes$yval <- NA
    for(i in 1:nrow(LabelGenotypes)){
      LabelGenotypes$yval[[i]] <- PlotData$count[[max(which(PlotData$xmin < LabelGenotypes$value[[i]]))]]
    }
  }
  
  # Add labels w/arrows for the parents/checks using this new data
  # c(max(PlotData$count) + max(PlotData$count)/5)
  Plot.Final <- Plot.init + 
    ylim(c(0, 12)) + 
    ggrepel::geom_label_repel(data = CheckParents,
                              aes(x = value, y = yval, label = genotype),
                              nudge_y = max(PlotData$count)/3,
                              arrow = arrow(length = unit(0.015, "npc")),
                              min.segment.length = 0,
                              size = 8) +
    coord_cartesian(xlim = c(min(unlist(blue_data[, trait_name])) - 2, max(unlist(blue_data[, trait_name])) + 2), expand = FALSE) + 
    scale_fill_manual(values = test_colors) + 
    theme(legend.position = "none")
  
  # Add additional genotypes if requested
  if(nrow(LabelGenotypes) > 0){
    Plot.Final <- Plot.Final +
      ggrepel::geom_label_repel(data = LabelGenotypes,
                                aes(x = value, y = yval, label = genotype),
                                nudge_y = max(PlotData$count)/3,
                                arrow = arrow(length = unit(0.015, "npc")),
                                min.segment.length = 0,
                                size = 8)
  }
  
  Plot.Final
  
}

match_from_table("md", util_tables$trait_lookup)

ggplot(genotype_BLUEs$BLUEs$`Jay Test 1`, aes_string(x = "yield", fill = "test_name")) + 
  theme_bw() + 
  geom_histogram(color = "black", bins = 10) + 
  scale_fill_manual(values = test_name_colors) + 
  theme(legend.position = "none") 

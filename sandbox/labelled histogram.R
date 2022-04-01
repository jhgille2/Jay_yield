conflict_prefer("filter", "dplyr")

TraitHist.facet <- function(TraitData, Name = names(YieldData_melt_TestTrait)[[1]], FillCol = TestColours[[1]]){
  
  # Get the test and trait name from the name of the list element
  TraitName <- sub("Yield Test \\d{1}\\.", "", Name)
  TestName  <- str_extract(Name, "Yield Test \\d{1}")
  
  # Get the average of checks for a test, and the LSD
  CheckAvg <- CheckParentAvgerage_byTest %>% filter(Test == TestName, Genotype %in% Checks)
  CheckAvg <- mean(CheckAvg$LSMean_Yield)
  
  # Get the LSD for yield for the current test
  CurrentLSD <- AllYieldTests.LSD$Yield[which(AllYieldTests.LSD$Test == TestName)]
  LSDLine    <- CheckAvg - CurrentLSD
  
  # The initial plot (need bin heights)
  Plot.init <- ggplot(TraitData, aes(x = value)) + 
    geom_histogram(bins = 9, fill = FillCol, col = 'black') + 
    theme_bw() + 
    theme(axis.text = element_text(size = 20, face = 'bold'),
          axis.title = element_text(size = 30)) + 
    ylab("Count") + 
    xlab(TraitName) + 
    geom_vline(xintercept = CheckAvg, linetype = 'dotted', colour = 'black', size = 1.25) + 
    geom_vline(xintercept = LSDLine, linetype = 2, colour = 'red', size = 1.25)
  
  # Get data for the parents/checks
  CheckParents <- TraitData %>% filter(Genotype %in% c(Checks, Parents))
  
  # Data from the initial plot (I want the bin heights)
  PlotData <- ggplot_build(Plot.init)$data[[1]]
  
  # Using the bin counts from the plot, find the y-value where the labels for each
  # check/parent genotype shoud start
  CheckParents$yval <- NA
  for(i in 1:nrow(CheckParents)){
    CheckParents$yval[[i]] <- PlotData$count[[max(which(PlotData$xmin < CheckParents$value[[i]]))]]
  }
  
  
  # Add labels w/arrows for the parents/checks using this new data
  # c(max(PlotData$count) + max(PlotData$count)/5)
  Plot.Final <- Plot.init + 
    ylim(c(0, 20)) + 
    ggrepel::geom_label_repel(data = CheckParents,
                              aes(x = value, y = yval, label = Genotype),
                              nudge_y = max(PlotData$count)/6,
                              arrow = arrow(length = unit(0.015, "npc")),
                              min.segment.length = 0,
                              family = "LM Roman 10",
                              size = 8) +
    coord_cartesian(xlim = c(min(YieldLSMeans$value) - 200, max(YieldLSMeans$value) + 200), expand = FALSE)
  
  Plot.Final
}
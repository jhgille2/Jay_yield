##################################################
## Project: Jay Yield
## Script purpose: Development of scatter plots to show pairwise 
## relationships between traits
## Date: 2022-04-01
## Author: Jay Gillenwater
##################################################


# Load in the data to use in the histograms
tar_load(util_tables)
tar_load(genotype_BLUEs)
tar_load(all_yield_data)
tar_load(test_name_colors)

all_data <- reduce(genotype_BLUEs$BLUEs, bind_rows)

test1 <- ge_plot(all_yield_data$two_year$`Jay Test 1`, 
                 env = ENV, 
                 gen = genotype, 
                 resp = yield) + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1))

test2 <- ge_plot(all_yield_data$two_year$`Jay Test 2`, 
                 env = ENV, 
                 gen = genotype, 
                 resp = yield) + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1))

x11()
arrange_ggplot(test1, test2, tag_levels = list("Test 1", "Test 2"))

index_match <- function(vec, convert_table){
  matches     <- match(vec, unlist(convert_table[, 1]))
  matches_ind <- which(!is.na(matches))
  
  replacement <- unlist(convert_table[, 2])[matches[matches_ind]]
  
  return(list("index" = matches_ind, 
              "replacement" = replacement))
}

replacement_names <- index_match(names(all_data), util_tables$trait_shortnames)

names(all_data)[replacement_names$index] <- replacement_names$replacement

ggplot(all_data, aes(x = protein, y = yield, fill = test_name)) + 
  geom_point(color = "black", shape = 21, size = 5) + 
  geom_smooth(method = "lm") + 
  theme_few()

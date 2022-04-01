##################################################
## Project: Jay Yield
## Script purpose: descriptive statistics for two year data
## Date: 2022-04-01
## Author: Jay Gillenwater
##################################################

# Load the yield data
tar_load(all_yield_data)
tar_load(test_name_colors)
tar_load(util_tables)

data_twoyear <- all_yield_data$two_year %>% 
  reduce(bind_rows) %>% 
  rename_with_lookup(., util_tables$trait_shortnames)


desc_stat(data_twoyear, hist = TRUE, by = test)

corr_two_year <- map(all_yield_data$two_year, function(x) rename_with_lookup(x, util_tables$trait_shortnames)) %>% 
  map(., corr_coef) %>% 
  map(., plot)

x11()
arrange_ggplot(corr_two_year, tag_levels = list(names(corr_two_year)))

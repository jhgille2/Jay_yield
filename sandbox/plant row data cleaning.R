##################################################
## Project: Jay Yield
## Script purpose: Development of the function to clean up the plant
## row data from 2018
## Date: 2022-04-25
## Author: Jay Gillenwater
##################################################

tar_load(plant_rows_2019_file)

excel_sheets(plant_rows_2019_file)


# Read in the data from each population
pop_201 <- read_excel(plant_rows_2019_file, sheet = "Pop201")
pop_202 <- read_excel(plant_rows_2019_file, sheet = "Pop202")

both_pops <- bind_rows(pop_201, pop_202) %>% 
  clean_names() %>% 
  rename(sdwt = x100_seed_weight)

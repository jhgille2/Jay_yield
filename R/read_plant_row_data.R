#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param plant_rows_2019_file
read_plant_row_data <- function(plant_rows_2019_file) {

  # Read in the data from each population
  pop_201 <- read_excel(plant_rows_2019_file, sheet = "Pop201")
  pop_202 <- read_excel(plant_rows_2019_file, sheet = "Pop202")
  
  # Combine the two data sets into one, clean up them names, 
  # and rename a clumsy column name
  both_pops <- bind_rows(pop_201, pop_202) %>% 
    clean_names() %>% 
    rename(sdwt = x100_seed_weight)
  
  return(both_pops)
}

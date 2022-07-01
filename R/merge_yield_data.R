#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param yield_2019_file
#' @param cas_maturity_file
#' @param cla_maturity_file
#' @param nir_2020_file
#' @param yield_2020_file
#' @param yield_2021_file
merge_yield_data <- function(yield_2019_file, cas_maturity_file,
                             cla_maturity_file, nir_2020_file, yield_2020_file,
                             yield_2021_file, 
                             util_tables) {
  
  # Read in the 2019 yield data and rename some of the columns
  yield_2019 <- read_excel(yield_2019_file, col_types = "text") %>%
    clean_names() %>% 
    rename(loc              = location, 
           fc               = flower_color, 
           md               = maturity_date, 
           pb               = pubescence, 
           ht               = height, 
           lod              = lodging, 
           protein_plus_oil = oil_protein) %>% 
    mutate(loc = toupper(loc)) # Convert from kg/ha to bu/acre

  # Read in the 2020 maturity date data
  md_cas <- read_excel(cas_maturity_file, col_types = "text")
  md_cla <- read_excel(cla_maturity_file, col_types = "text")
  
  # And merge the maturity date files into one dataframe
  all_md <- bind_rows(md_cas, md_cla) %>% 
    clean_names() %>% 
    select(genotype, loc, test, year, rep, code, plot, md) %>% 
    mutate(loc = toupper(loc))
  
  # Read in the 2020 nir data
  nir_2020 <- read_excel(nir_2020_file, col_types = "text") %>% 
    clean_names()
  
  # Read in the 2020 yield data and join with the maturity date and yield data
  yield_2020 <- read_excel(yield_2020_file, col_types = "text") %>% 
    clean_names() %>% 
    left_join(nir_2020, by = c("genotype", "loc", "test", "code", "rep", "plot", "year")) %>% 
    left_join(all_md, by = c("genotype", "loc", "test", "code", "rep", "plot", "year"))
  
  # Read in the 2021 yield data
  yield_2021 <- yield_2021_file %>% 
    map(read_excel, col_types = "text") %>%
    reduce(bind_rows) %>% 
    clean_names() %>% 
    rename(loc              = location, 
           md               = maturity_date, 
           protein_plus_oil = oil_protein, 
           ht               = height, 
           lod              = lodginig)
  
  # The columns I want to keep fropm all three years
  keep_cols <- c("test", 
                 "loc", 
                 "year",
                 "genotype", 
                 "code", 
                 "rep", 
                 "plot", 
                 "md", 
                 "ht", 
                 "lod", 
                 "oil", 
                 "protein", 
                 "yield", 
                 "sdwt", 
                 "sq")
  
  # The columns to convert to numerics
  num_cols <- c("md", 
                "ht", 
                "oil", 
                "lod",
                "protein", 
                "yield", 
                "sdwt", 
                "sq", 
                "test_weight")
  
  # A function to filter to just the columns I want to keep
  filter_fn <- function(data, cols_to_keep){
    data %>% 
      select(any_of(cols_to_keep))
  }
  
  yield_2019 %>% 
    dplyr::filter(genotype %in% c("N18-1632", unique(c(yield_2020$genotype, yield_2021$genotype))))
  
  # A list of all three years data
  all_yield <- list(yield_2019, 
                    yield_2020, 
                    yield_2021)
  
  # Apply the column select function to each dataframe, 
  # merge the three years data,
  # convert numeric columns to numerics, and convert yield to bu/acre
  all_merged <- map(all_yield, function(x) filter_fn(x, keep_cols)) %>% 
    reduce(bind_rows) %>% 
    mutate(across(any_of(num_cols), as.numeric), 
           protein          = ifelse(as.numeric(rep) > 2, NA, protein), 
           oil              = ifelse(as.numeric(rep) > 2, NA, oil),
           protein_plus_oil = protein + oil, 
           yield            = ifelse(year %in% c("2020"), yield*0.033,                          # Adjust yield from kg/ha to bu/acre
                               ifelse(year == "2019", yield/67.2510693716674,                   # Adjust yield from grams to bu/acre
                                ifelse(loc %in% c("PLY", "SAN"), yield*0.0252, yield*0.0228))), # Adjust from grams to bu/acre
           ENV              = paste(loc, year, sep = " - "), 
           protein_13_pct   = protein*0.87, # Adjust protein/oil to a 13% moisture basis so that protein meal can be calculated
           oil_13_pct       = oil*0.87, 
           protein_meal     = snfR::calc_protein_meal(oil_13_pct, protein_13_pct)) %>% 
    distinct() %>% 
    select(-any_of(c("protein_13_pct", "oil_13_pct")))
  
  # Standardize genotype names so that each genotype is identified with
  # only name. See the function definition in the utils.R script.
  all_merged <- convert_from_table(all_merged, "genotype", util_tables$genotype_conversion_table)
  
  # Split this data into two tables: One with all the data
  # and one with just the data from 2020 and 2021, split up by test
  full_data <- all_merged %>% 
    split(., .$test)
  
  two_years_data <- all_merged %>% 
    dplyr::filter(year %in% c("2020", "2021")) %>% 
    split(., .$test)
  
  test_list <- list("all"      = full_data, 
                    "two_year" = two_years_data)
  
  return(test_list)
}

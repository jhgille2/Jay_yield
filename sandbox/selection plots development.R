##################################################
## Project: Jay Yield
## Script purpose: Making boxplots to show how maturity date was used to assign 
## genotypes to tests
## Date: 2022-04-25
## Author: Jay Gillenwater
##################################################

# Load in the required data
tar_load(c(plant_row_data, all_yield_data))

plant_row_data %<>%
  rename(genotype = row) %>% 
  mutate(test = population)

# Merge the data from 2019 into one dataframe and then just get the 
# table of genotypes that were in each test
selected_genotypes_2019 <- all_yield_data %>%
  pluck("all") %>% 
  reduce(bind_rows) %>% 
  dplyr::filter(year == "2019") %>% 
  select(test, genotype) %>% 
  distinct() %>% 
  split_factors(test, keep_factors = TRUE) %>% 
  map(., function(x) left_join(x, plant_row_data, by = "genotype")) %>% 
  reduce(bind_rows) %>% 
  select(-test.y) %>% 
  rename(test = test.x) %>%
  dplyr::filter(!is.na(md)) %>% 
  bind_rows(plant_row_data) %>%
  mutate(p_o = protein_dry_basis + oil_dry_basis) %>%
  select(test, genotype, population, md, ht, sdwt, bulk_weight, oil_dry_basis, protein_dry_basis, p_o) %>%
  split_factors(population, keep_factors = TRUE)

# The data for one of the population to test plotting functions
pop_201_data <- selected_genotypes_2019$`Oil Mapping Pop 201`

md_plot <- ggplot(pop_201_data, aes(x = test, y = md, fill = test)) + 
  geom_boxplot(color = "black") + 
  coord_flip() + 
  theme_few() + 
  theme(legend.position = "none")

p_o_plot <- ggplot(pop_201_data, aes(x = p_o, fill = test)) + 
  geom_histogram(color = "black") + 
  theme_few()

bulk_weight_plot <- ggplot(pop_201_data, aes(x = bulk_weight, fill = test)) + 
  geom_histogram(color = "black") + 
  theme_few()

x11()
(bulk_weight_plot+ p_o_plot) / md_plot + plot_layout(guides = "collect")




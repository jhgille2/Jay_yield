##################################################
## Project: Jay Yield
## Script purpose: Exploring the mixed model contrasts to identify genotypes
## with similar/superior yield, protein, and oil when compared to the checks
## Date: 2022-05-10
## Author: Jay Gillenwater
##################################################

# Setting up environment
source("./packages.R")
tar_make()

tar_load(genotype_contrasts_mixed)

all_contrasts <- genotype_contrasts_mixed %>% 
  dplyr::filter(pheno %in% c("oil", "protein", "yield", "lod", "sq")) %>% 
  mutate(contrasts = map(contrasts, as_tibble)) %>% 
  select(test_name, pheno, contrasts) %>% 
  unnest(contrasts) %>% 
  group_by(test_name, pheno) %>% 
  mutate(estimate_scaled = as.numeric(scale(estimate))) %>% 
  arrange(test_name, contrast, pheno)

scaled_estimates <- all_contrasts %>% 
  group_by(test_name, contrast) %>% 
  summarise(avg_scaled_estimate = mean(estimate_scaled))
  

all_contrasts_summary <- left_join(all_contrasts, scaled_estimates, by = c("test_name", "contrast"))

# A function that returns a character string to indicate if an observation has a phenotypic value comparable
# to or greater than that of the check average
comparable_pheno <- function(test_name, contrast, pheno, estimate, p.value){
  
  if(p.value < 0.05){
    if(estimate > 0){
      res <- switch(pheno, 
                    "oil"     = "HO",
                    "protein" = "HP", 
                    "yield"   = "HY", 
                    "lod"     = "HL", 
                    "sq"      = "HSQ")
    }else{ 
      res <- switch(pheno, 
                    "oil"     = "LO",
                    "protein" = "LP", 
                    "yield"   = "LY", 
                    "lod"     = "LL", 
                    "sq"      = "LSQ")
      }
  }else{
    res <- switch(pheno, 
                  "oil"     = "SO",
                  "protein" = "SP", 
                  "yield"   = "SY", 
                  "lod"     = "SL", 
                  "sq"      = "SSQ")
  }
  
  return(res)
}


find_elites <- function(contrast, test_name, oil, protein, yield, sq, lod){
  
  res <- ifelse(oil == "SO" & protein == "HP" & yield == 'SY', "yes", "no")

  return(res)  
}

pheno_categories <- all_contrasts %>% 
  ungroup() %>% 
  select(test_name, contrast, pheno, estimate, p.value) %>% 
  mutate(pheno_group = pmap_chr(., comparable_pheno), 
         contrast = str_remove(contrast, " - Checks")) %>% 
  {. ->> pheno_categories_full } %>%
  pivot_wider(names_from = pheno, values_from = pheno_group, id_cols = c(contrast, test_name)) %>% 
  mutate(elite_geno = pmap_chr(., find_elites)) %>% 
  rename_with(~ paste( .x, "category", sep = "_"), .cols = 3:ncol(.))


all_contrasts_summary %>% 
  arrange(test_name, pheno, desc(avg_scaled_estimate)) %>% 
  View()

elite_genos <- c("N18-1635", "N18-1627", "N18-1643", "N18-1783")

genotype_BLUEs$BLUEs %>% 
  reduce(bind_rows) %>% 
  select(genotype, test_name, oil, protein, yield) %>% 
  right_join(pheno_categories, by = c("genotype" = "contrast", "test_name")) %>% 
  dplyr::filter(protein_category == "HP", 
                lod_category != "HL", 
                !genotype %in% elite_genos) %>% 
  arrange(desc(protein)) %>% 
  mutate(p_o = protein + oil) %>% 
  arrange(desc(protein))

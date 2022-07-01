#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param genotype_BLUEs
make_supplementary_tables <- function(genotype_BLUEs, genotype_contrasts_mixed, util_tables) {

  
  # Unnest the contrasts
  contrasts_wide <- genotype_contrasts_mixed %>% 
    mutate(contrasts = map(contrasts, as_tibble)) %>% 
    select(test_name, pheno, contrasts) %>% 
    unnest(contrasts) 
  
  # Rename the variables in the pheno column
  contrasts_wide <- convert_from_table(contrasts_wide, "pheno", util_tables$trait_lookup) %>% 
    rename(`Test name` = test_name)
  
  # Get just the genotypes that had a lodging that was statistically higher than 
  # the checks for its test
  high_lodging <- contrasts_wide %>% 
    dplyr::filter(pheno == "Lodging",
                  p.value < 0.05, 
                  estimate > 0)
  
  # Make a supplementary table with all the lsmeans 
  lsmean_table <- genotype_BLUEs %>%
    pluck("BLUEs") %>% 
    reduce(bind_rows) %>% 
    rename_with_lookup(util_tables$trait_lookup) %>% 
    rename(Genotype = genotype, 
           `Test name` = test_name) %>% 
    arrange(`Test name`, Genotype) %>% 
    mutate_if(is.numeric, round, 2)
  
  # Genotypes with similar oil and yield, high protein
  elite_genos <- c("N18-1635", "N18-1627", "N18-1643", "N18-1783")
  elite_genos_table <- lsmean_table %>% 
    dplyr::filter(Genotype %in% elite_genos)
  
  # Table with just the protein, oil, and protein meal lsmeans for the 
  # genotypes that had 
  
  res <- list("all_lsmeans"         = lsmean_table, 
              "contrasts_table"     = contrasts_wide, 
              "high_lodging"        = high_lodging, 
              "elite_genos_lsmeans" = elite_genos_table)
  
  return(res)
}

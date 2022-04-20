#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df
get_lsd_values <- function(df = all_yield_data$two_year) {

  # A function to fit a linear model with the yield data for one test
  fit_lm <- function(test_data){
    
    # Phenotypes to fit a model on
    measure_vars <- c("md", 
                      "ht", 
                      "lod", 
                      "oil", 
                      "protein", 
                      "yield", 
                      "sdwt", 
                      "sq", 
                      "protein_plus_oil")
    
    # A function to apply to the nested data column to remove any observations
    # with missing data. Also, remove the parent genotypes from the data 
    # because the LSD.test function can only return a value for LSD/MSD if
    # the data is perfectly balanced (as all things should be) and these genotypes
    # were included multiple times in the tests.
    clean_data <- function(pheno_data){

      pheno_data %<>%
        dplyr::filter(!is.na(value), !genotype %in% c("LMN09-119", "N09-09"))

      genotype_reps <- pheno_data %>%
        group_by(genotype) %>%
        count(name = "num_reps")

      rep_counts <- genotype_reps %>%
        group_by(num_reps) %>%
        count(name = "rep_count")

      max_rep_count <- rep_counts$num_reps[[which(rep_counts$rep_count == max(rep_counts$rep_count))]]

      keep_genos <- genotype_reps %>%
        dplyr::filter(num_reps == max_rep_count)

      keep_genos <- as.character(keep_genos$genotype)

      pheno_data %<>%
        dplyr::filter(genotype %in% keep_genos)

      return(pheno_data)

    }
    
    # Pivot the data by these phenotypes and then nest by each phenotype
    test_data %<>%
      pivot_longer(cols = all_of(measure_vars), names_to = "trait") %>%
      group_by(trait) %>% 
      nest() %>% 
      ungroup()  %>%
      mutate(data = map(data, clean_data))
    
    # I took this part out to test using the HSD test function with the unbalanced
    # data
    #
    
    model_fn <- function(pheno_data) {
      
      # Fit a linear model on the data, perform an analysis of variance, 
      # and then perform a least significant difference test
      model     <- with(pheno_data, lm(value~ genotype + genotype:ENV + ENV/rep))
      model_aov <- aov(model)
      trait_lsd <- agricolae::LSD.test(model_aov, 
                                       trt   = "genotype", 
                                       p.adj = "none")
      
      trait_hsd <- agricolae::HSD.test(model_aov, 
                                       trt = "genotype")
      
      trait_hsd_metan <- metan::tukey_hsd(model_aov, 
                                          which = "genotype")
      
      trait_hsd_stats <- TukeyHSD(model_aov, 
                                  which = "genotype")
      
      # Group the model, aov, and lsd test into a list and return this list
      res <- list("model"     = model, 
                  "model_aov" = model_aov, 
                  "lsd"       = trait_lsd, 
                  "hsd"       = trait_hsd)
      
      return(res)
    }
    
    # Apply the model fitting function to the nested data column and return
    # the original data with this a new column added to hold the results
    test_data %<>% 
      mutate(fit_model = map(data, model_fn))
    
    return(test_data)
  }
  
  # Get the model data for both tests
  all_models <- map(df, fit_lm)
  
  return(all_models)
}

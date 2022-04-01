#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df
calc_anova <- function(df = all_yield_data$two_year) {

  # A function to perform by-location
  # and joint analysis of variance for each test
  get_individual_anova <- function(test_data){
    
    # Individual environment ANOVA
    individual_anova <- anova_ind(.data   = test_data, 
                                  env     = ENV, 
                                  gen     = genotype, 
                                  rep     = rep, 
                                  resp    = everything(), 
                                  verbose = FALSE)
    # Joint ANOVA
    joint_anova <- anova_joint(.data   = test_data, 
                               env     = ENV, 
                               gen     = genotype, 
                               rep     = rep, 
                               resp    = everything(), 
                               verbose = FALSE)
    
    # Combine both into a list
    res <- list("individual" = individual_anova, 
                "joint" = joint_anova)
    
    # And return this list
    return(res)
  }
  
  # Map the function to each tests data
  anova_results <- map(df, get_individual_anova)

  return(anova_results)
}

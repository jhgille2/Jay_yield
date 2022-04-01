#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

make_example_table <- function() {

  data(mtcars)
  
  ex_kable <- head(mtcars) %>% 
    kable(format = "latex", caption = "This is a table caption. Tables should be placed in the main text near to the first time they are cited.", booktabs = TRUE) %>% row_spec(0,bold=TRUE) %>% 
    kable_styling()
  
  return(ex_kable)
}

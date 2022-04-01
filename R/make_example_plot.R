#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param all_yield_data
#' @param test_name_colors
#' @param dir
make_example_plot <- function(all_yield_data, test_name_colors, dir =
                              here("exports", "plots", "example_plot.pdf")) {

  # Get just the data from 2019
  data_2019 <- all_yield_data$all %>% 
    reduce(bind_rows) %>%
    dplyr::filter(str_detect(test, "Yield Test"))
  
  test_colors <- test_name_colors[names(test_name_colors) %in% unique(data_2019$test)]
  
 p <-  ggplot(data_2019, aes(y = yield, fill = test)) + 
   geom_boxplot() + 
   theme_calc() + 
   scale_fill_manual(values = test_colors)
 
 ggsave(dir, plot = p, device = "pdf", width = 5, height = 5, units = "in")
 
 return(dir)
}

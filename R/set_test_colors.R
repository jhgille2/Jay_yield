#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param color_ids
set_test_colors <- function(color_ids = names(all_yield_data$all)) {

  # Use the gg_color_hue function (see utils.R) to assign a 
  # hexcode color to each test name
  gg_color_hue(length(color_ids)) %>% 
    set_names(color_ids)

}

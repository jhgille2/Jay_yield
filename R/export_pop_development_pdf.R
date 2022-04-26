#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pop_development_flowchart
export_pop_development_pdf <- function(pop_development_flowchart) {

  DiagrammeR::grViz(pop_development_flowchart) %>% 
    export_svg() %>% 
    charToRaw() %>% 
    rsvg_png(file = here("exports", "plots", "pop_development_flowchart.png"))
  
  return(here("exports", "plots", "pop_development_flowchart.png"))
}

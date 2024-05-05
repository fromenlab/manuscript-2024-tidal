plot_interactive <- function(core_plot, plotly_width = 800, plotly_height = 500, ...) {
  if (knitr::is_html_output() | interactive()){
    core_plot %>% ggplotly(width = plotly_width, height = plotly_height, ...)  
  } else {
    core_plot
  }
}

report_list <- function(x) {
  if (knitr::is_html_output()) {
    x %>% htmltools::tagList()
  } else {
    x
  }
}
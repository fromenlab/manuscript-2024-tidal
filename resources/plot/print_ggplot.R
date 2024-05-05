# Modified from https://gist.github.com/debruine/baa61e561935d5e0df1ea0f294399c6c
# See relevant issue here: https://github.com/rstudio/rmarkdown/issues/1827

# 20240212 - get chunk options for width, height, and dpi

print.ggplot <- function(plot, ...){
  args <- list(...)
  
  if (interactive() && 
      !isTRUE(getOption("knitr.in.progress"))) {
    filename <- tempfile(fileext = ".png")
    
    if (is.null(args$width) && is.null(args$height)) {
      width = knitr::opts_chunk$get("fig.width")
      height = knitr::opts_chunk$get("fig.height")
      
      if (is.null(args$dpi)) {
        dpi = knitr::opts_chunk$get("dpi")
      } else {
        dpi = args$dpi
      }
      
      suppressMessages(ggplot2::ggsave(filename, plot, width = width, height = height, dpi = dpi, ...))
    } else {
      suppressMessages(ggplot2::ggsave(filename, plot, ...))
    }
    
    print(knitr::include_graphics(filename))
    
  } else {
    ggplot2:::print.ggplot(plot)
  }
  invisible(NULL)
}
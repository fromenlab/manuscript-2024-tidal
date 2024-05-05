# Based on https://doi.org/10.1083/jcb.202001064

geom_prismbars <- function(mapping = NULL, data = NULL, geom = NULL,
                           position = "identity", ..., fun.data = NULL, fun = NULL,
                           fun.max = NULL, fun.min = NULL, fun.args = list(), na.rm = FALSE,
                           orientation = NA, show.legend = NA, inherit.aes = TRUE,
                           fun.y, fun.ymin, fun.ymax) {
  if (!missing(fun.y)) {
    warn("`fun.y` is deprecated. Use `fun` instead.")
    fun <- fun %||% fun.y
  }
  if (!missing(fun.ymin)) {
    warn("`fun.ymin` is deprecated. Use `fun.min` instead.")
    fun.min <- fun.min %||% fun.ymin
  }
  if (!missing(fun.ymax)) {
    warn("`fun.ymax` is deprecated. Use `fun.max` instead.")
    fun.max <- fun.max %||% fun.ymax
  }
  list(
    layer(
      data = data, mapping = mapping, stat = StatSummary,
      geom = GeomErrorbar, position = position, show.legend = show.legend,
      inherit.aes = inherit.aes, params = list(
        width = 0.1, fun.data = function(x) {
          mean <- mean(x)
          sd <- sd(x)
          sem <- sd / sqrt(length(x))
          vals <- c(mean - sd, mean, mean + sd)
          names(vals) <- c("ymin", "y", "ymax")
          vals
        },
        fun = fun, fun.max = fun.max, fun.min = fun.min,
        fun.args = fun.args, na.rm = na.rm, orientation = orientation,
        ...
      )
    ),
    layer(
      data = data, mapping = mapping, stat = StatSummary,
      geom = GeomErrorbar, position = position, show.legend = show.legend,
      inherit.aes = inherit.aes, params = list(
        width = 0.3, fun.data = function(x) {
          mean <- mean(x)
          vals <- c(mean, mean, mean)
          names(vals) <- c("ymin", "y", "ymax")
          vals
        },
        fun = fun, fun.max = fun.max, fun.min = fun.min,
        fun.args = fun.args, na.rm = na.rm, orientation = orientation,
        ...
      )
    )
  )
}
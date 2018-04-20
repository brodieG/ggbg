#' Waterfall Charts
#'
#' Waterfall charts are a variation on [ggplot2::geom_col] charts, where the
#' base of each bar starts where the top of the previous bar ended.  This is
#' effectively equivalent to a stacked bar chart that has been spread on the x
#' axis.
#'
#' @inheritParams ggplot2::geom_bar

geom_waterfall <- function(
  mapping = NULL, data = NULL,
  stat = "identity", position = "stack",
  ...,
  width = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomWaterfall,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbg-ggproto
#' @export

GeomWaterfall <- ggproto("GeomWaterfall", GeomRect,

  required_aes = c("x", "y"),

  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    transform(data,
      ymin = pmin(y, 0), ymax = pmax(y, 0),
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
  },

  draw_panel = function(self, data, panel_params, coord, width = NULL) {
    # width=NULL is hack to ensure that width is detected as a parameter as
    # otherwise it gets trimmed by param trimming process in `draw_layer`

    # In order for our waterfall to work, we need to compute the cumulative
    # values; one question is whether the data is correctly sorted.

    browser()
    ggproto_parent(GeomRect, self)$draw_panel(data, panel_params, coord)
  }
)

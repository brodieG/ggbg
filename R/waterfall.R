## Copyright (C) 2018  Brodie Gaslam
##
## This file is part of "ggbg - Assorted Ggplot Extensions"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

#' Waterfall Charts
#'
#' Waterfall charts are a variation on [ggplot2::geom_col] charts, where the
#' base of each bar starts where the top of the previous bar ended.  This is
#' effectively equivalent to a stacked bar chart that has been spread on the x
#' axis.
#'
#' @inheritParams ggplot2::geom_bar
#' @export

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

  setup_data = function(self, data, params) {
    if(!all(c("x", "y") %in% names(data)))
      stop(
        "geom_waterfall requires aesthetics ",
        paste0(setdiff(c("x", "y"), names(data)), sep=", ")
      )

    # Compute the x offsets, start by ordering by x values

    if(nrow(data)) {
      x.o <- order(
        data[["x"]], sign(data[["y"]]), data[["PANEL"]], data["group"]
      )
      data.o <- data[x.o, ]
      y.cum <- cumsum(data.o[["y"]])
      y.cum.lag <- c(0, head(y.cum, -1L))
      data.o[["ymin"]] <- pmin(y.cum, y.cum.lag)
      data.o[["ymax"]] <- pmax(y.cum, y.cum.lag)
      data <- data.o[rank(x.o),]
    }
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)

    data <- transform(data,
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
    # not strictly necessary, but seems good form to call the parent panel
    # setup

    res <- ggproto_parent(GeomRect, self)$setup_data(data, params)
    browser()
    res
  },

  draw_panel = function(self, data, panel_params, coord, width = NULL) {
    check_req_aes(self, data)
    # width=NULL is hack to ensure that width is detected as a parameter as
    # otherwise it gets trimmed by param trimming process in `draw_layer`

    # In order for our waterfall to work, we need to compute the cumulative
    # values; one question is whether the data is correctly sorted.

    ggproto_parent(GeomRect, self)$draw_panel(data, panel_params, coord)
  }
)

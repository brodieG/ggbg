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

# Generate a car 'grob' using a baseline PNG; we don't store the actual PNG with
# the package, instead we have the pre-stored car.raster variable in sysdata.rda
# car.raster <- png::readPNG("~/Downloads/car2.png")

# The `grid` grob actually responsible for rendering our car

# nocov start
# covr doesn't seem to correctly handle this

carGrob <- function(x, y, length, width, gp) {
  grobTree(
    rectGrob(
      x, y, hjust=.5, height=width, width=length,
      gp = gp
    ),
    rasterGrob(
      car.raster, x=x, y=y, hjust=.5, height=width, width=length
) ) }
# nocov end

#' Renders the `geom_car` Layer
#'
#' `GeomCar` renders cars on a layer.  It is quite limited, but a useful example
#' for understanding how to create custom geoms with custom grobs.
#'
#' @rdname ggbg-ggproto
#' @importFrom grid grobTree rasterGrob rectGrob gpar

GeomCar <- ggproto("GeomCar", Geom,
  # Generate grobs from the data, we have to reconvert length/width so
  # that the transformations persist

  draw_panel=function(self, data, panel_params, coords) {
    with(
      coords$transform(data, panel_params),
      carGrob(
        x, y, length=xmax-xmin, width=ymax-ymin,
        gp=gpar(
          col = colour, fill = alpha(fill, alpha),
          lwd = size * .pt, lty = linetype, lineend = "butt"
  ) ) ) },
  # Convert data to coordinates that will get transformed (length/width don't
  # normally).

  setup_data=function(self, data, params) {
    transform(data,
      xmin = x - length / 2, xmax = x + length / 2,
      ymin = y - width / 2, ymax = y + width / 2
  ) },
  # Required and default aesthetics

  required_aes=c("x", "y", "length", "width"),
  default_aes = aes(
    colour = NA, fill = "grey35", size = 0.5, linetype = 1, alpha = NA
  ),
  # Use the car grob in the legend

  draw_key = function(data, params, size) {
    with(
      data,
      carGrob(
        0.5, 0.5, length=.75, width=.5,
        gp = gpar(
          col = colour, fill = alpha(fill, alpha),
          lwd = size * .pt, lty = linetype, lineend = "butt"
  ) ) ) }
)
#' Cars
#'
#' A geom that displays cars.  This geom was implemented on a lark as an answer
#' to a [question on SO](https://stackoverflow.com/questions/22159087/is-it-possible-to-draw-diagrams-in-r/22207979#22207979).
#'
#' @eval ggplot2:::rd_aesthetics("geom", "car")
#' @inheritParams ggplot2::layer
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(
#'   geom.car.data,  # ggbg data set
#'   aes(x=x, y=y, length=length, width=width, fill=label)
#' ) +
#' geom_hline(yintercept=seq(5, 35, by=10), color="white", size=2, linetype=2) +
#' geom_car() +
#' coord_equal() +
#' theme(panel.background = element_rect(fill="#555555"), 
#'   panel.grid.major = element_blank(),
#'   panel.grid.minor = element_blank())

geom_car <- function(
  mapping=NULL, data=NULL, ..., inherit.aes=TRUE, show.legend=NA
) {
  layer(
    data=data, mapping=mapping, geom=GeomCar, position="identity",
    stat="identity", show.legend = show.legend, inherit.aes = inherit.aes,
    params=list(...)
  )
}

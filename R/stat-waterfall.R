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

#' Computes the `ycum` Aesthetic
#'
#' `StatWaterfall` is intended for use only via the `stat` parameter to other
#' layers (e.g. `geom_...(stat='waterfall')`).  It will add a `ycum` aesthetic
#' to the data which can then be used by the layer.  Unlike other `Stat`
#' objects, it does not exist as a stand-alone layer (i.e. `stat_waterfall` is
#' not defined).  Its main purpose is to generate the text for cumulative
#' labels.  See [`position_waterfall`] for usage examples.
#'
#' @export
#' @rdname ggbg-ggproto
#' @format NULL
#' @seealso [`position_waterfall`] for usage examples.

StatWaterfall <- ggproto("StatWaterfall", Stat,
  required_aes = c("x", "y"),
  reverse = FALSE,

  compute_panel = function(data, scales, reverse=FALSE) {
    if("ycum" %in% names(data)) {
      warning(
        "`ycum` aesthetic aesthetic already exists, `stat_waterfall` ",
        "will not recompute it."
      )
    } else {
      dat.ord <-
        order(data[["x"]], data[["group"]] * if(reverse) -1 else 1)

      data.o <- data[dat.ord, , drop=FALSE]
      data.o[["ycum"]] <- cumsum(data.o[["y"]])
      data <- data.o[rank(dat.ord, ties.method='first'), , drop=FALSE]
    }
    data
  }
)

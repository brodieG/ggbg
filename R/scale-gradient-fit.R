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



jet <- t(
  matrix(
    c(
      0.0, 0.0, 0.5,
      0.0, 0.0, 1.0,
      0.0, 1.0, 1.0,
      0.0, 1.0, 0.0,
      1.0, 1.0, 0.0,
      1.0, 0.0, 0.0,
      0.5, 0.0, 0.0
    ),
    nrow=3
  )
)
#' @importFrom ggplot2 ScaleContinuous ggproto

ScaleContinuousFit <- ggproto("ScaleContinuousFit", ggplot2::ScaleContinuous,
  map_data = numeric(),
  map = function() NULL,

  ## We need to collect the totality of the data so that we can do shaped
  ## mappings, this is why we add the `data` field

  train = function(self, x) {
    if(!is.numeric(x))
      stop(
        "Internal Error: Attempting to train ", class(self),
        " with non-numeric data."
      )
    self$data <- c(self$data, c)

    ## call parent method as well in case other methods used the range data
    ggproto_parent(ScaleContinuous, self)$train(x)
  },
  map = function(self, x, limits = self$get_limits()) {
    x <- self$rescaler(self$oob(x, range = limits), from = limits)

    uniq <- unique(x)
    pal <- self$palette(uniq)
    scaled <- pal[match(x, uniq)]

    ifelse(!is.na(scaled), scaled, self$na.value)
  },
  ## Seems like `palette` is not really used outside of `map`, so TBD whether we
  ## want to allow independent spec.  Argument for is that is more consistent.
  palette = NULL
)
#' Create a Palette Function with Equally Spaced Colors
#'
#' @colours character a vector of colours to base the palette on, must have at
#'   least two colours, and the colours should be in a format understood by
#'   [`grDevices::col2rgb`].
#' @values numeric vector of the same length as `colours`, with strictly
#'   increasing values between 0 and 1 where each element corresponds to the
#'   rescaled value associated with the colour in the same spot in the `colours`
#'   vector.  The default is a vector of `NA_real_` values, which frees the
#'   palette to choose the most evenly spaced out colors across the range.  You
#'   can add constraints by specifying values, which will break up the color
#'   range into `n + 1` pieces, where `n` is the number of values you specify.
#'
#' @seealso [`ggplot2::scale_fill_gradientn`], [`grDevices::col2rgb`]
#' @param dist.fun a function that computes distances between colors in the
#'   L*a*b* color space.  The function should have two parameters, which will
#'   be 3 column matrices containing the L, a, and b coordinates or the colours.

equidist_palette <- function(colours, values, na.value, dist.fun=deltaE2000) {

}
colors <- c("blue", "yellow", "red", "orange")
values <- c(0, rep(NA_real_, length(colours) - 1), 1)

values[match('white', colors)] <- 0

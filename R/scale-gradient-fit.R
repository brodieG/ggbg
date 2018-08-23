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
  limits_quantile = NULL,

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
#' Create a Palette Function That Produces Uniform Colours
#'
#' The resulting palette function converts values between 0 and 1 into colours
#' along the piecewise path through `colours` in a three dimensional colour
#' space, L*a*b* by default.  Pairs of points that are equidistant in the input
#' domain should be approximately equidistant along the `colours` path as per
#' `dist_fun`, although if the path is not straight they are unlikely to be
#' equidistant in the colour space itself.
#'
#' Distance along the path is measured as the cumulative sum of the
#' `dist_fun` distance between small segments of the path.  In order to get
#' the expected outcomes `dist_fun` should return the same distance for any
#' straight line segment whether it is computed as the distance from one end to
#' the other, or as the sum of the lengths of sub-segments that constitute
#' the same path.  Additionally as we move from one point to the other along the
#' shortest Euclidean path in the three dimensional colour space, the value
#' `dist_fun` returns should decrease, though it need not do so in proportion to
#' the Euclidean distance traveled.
#'
#' Upon creation of the function, each segment in `colours` is subdivided into
#' enough pieces such that the distance spanned by each piece is less than the
#' tolerance specified in `dist_ctl`.  The value-to-colour map is then carried
#' out by finding the segment that causes the cumulative accrued `dist_fun`
#' distance as a fraction of the total cumulative `dist_fun` distance of the
#' path to exceed the value.  The actual `colour` is then interpolated linearly
#' in that segment in the provided colour space.
#'
#' The complexity of this function and the resulting palette function is
#' designed to accommodate the CIEDE2000 colour distance function, implemented
#' in this package as [`deltaE2000`].
#'
#' @seealso [`ggplot2::scale_fill_gradientn`], [`grDevices::col2rgb`]
#' @param colours character a vector of colours to base the palette on, must
#'   have at least two colours, and the colours should be in a format understood
#'   by [`grDevices::col2rgb`].
#' @param mid integer(1L) in `seq_along(colours)`, indicates which element in
#'   `colours` is to be treated as the midpoint colour.
#' @param dist_fun a function that computes distances between colors in a three
#'   dimensional color space.  The function should have two parameters, which
#'   will be 3 column matrices containing the coordinates of the colours to
#'   compute the distances between.  The function should compute the distances
#'   between matching rows across the two matrices.
#' @param na.value character(1L) a colour in a format understood by
#'   [`grDevices::col2rgb`] to use to represent NA values.
#' @param col_to_space a function that converts colors of the type accepted by
#'   [`col2rgb`] into a 3 column matrix representing coordinates of the same
#'   colour in the target colour space.  The default colour space is L*a*b*.
#' @param space_to_col a function that carries out the reverse of the
#'   that `col_to_space` transformation.
#' @param dist_ctl a list containing the control values used in the
#'   approximation of the `dist_fun` distance along the path.  It must include
#'   the following named elements:
#'
#'   * `max.adj.dist`: numeric(1L) the maximum allowed distance between two
#'     adjacent computed distance points.  The smaller this number the more
#'     accurate the approximation will be, but the trade off is a slower palette
#'     function.
#'   * `tol`: passed to [`equalize_dists2`] as the value of the `tol` parameter.
#'   * `iters`: passed to [`equalize_dists2`] as the value of the `iters`
#'     parameter.
#'   * Other parameters to pass on to `dist_fun`; if `dist_fun` uses one of the
#'     previous three parameter names you will need to write a wrapper function
#'     that uses different names and forwards them to the distance function.

palette_equi_div <- function(colours, mid, na.value, dist_fun=deltaE2000) {
}
palette_equi <- function(
  colours, na.value, dist_fun=deltaE2000, dist_ctl,
  col_to_space=color_to_lab, space_to_col=lab_to_color, from=0, to=1
) {
  col.lab <- color_to_lab(colours)
  f_lab <- make_coord_funs(col.lab)

  # We want some number of distinct colors to do our look

  col.lab.e <- do.call(equalize_dists2, c(list(col.lab, f_lab), dist_ctl))

}

colors <- c("blue", "yellow", "red", "orange")
values <- c(0, rep(NA_real_, length(colours) - 1), 1)

values[match('white', colors)] <- 0

## Somewhere we need to check that limit and limit_quantile are not both
## specified.

continuous_scale_fit <- function(
  aesthetics, scale_name, palette, name = waiver(),
  breaks = waiver(), minor_breaks = waiver(), labels = waiver(), limits = NULL,
  limits_quantile = NULL, rescaler = rescale, oob = censor, expand = waiver(),
  na.value = NA_real_, trans = "identity", guide = "legend", position = "left",
  super = ScaleContinuousFit
) {
  if(!is.null(limits) && !is.null(limts_quantile))
    stop("Scale may only specify one of `limits` and `limits_quantile`.")

}

scale_gradientx <- function(
  aesthetics, mid=NULL, midpoint=NULL, colours, dist_fun=deltaE2000, ...
) {

}
scale_fill_gradientx <- function(..., aesthetics='fill') {
  scale_gradientx(..., aesthetics=aesthetics)
}
scale_colour_gradientx <- function(..., aesthetics='colour') {
  scale_gradientx(..., aesthetics=aesthetics)
}
scale_color_gradientx <- scale_colour_gradientx



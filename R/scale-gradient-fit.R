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
#' along the piecewise path through `colours` in the L*a*b* space.  Pairs of
#' points that are equidistant in the input domain should be approximately
#' equidistant along the `colour` path as per `dist.fun`.
#'
#' Distance along the path is measured as the cumulative sum of the
#' `dist.fun` distance between small segments of the path.
#'
#' TEST WHETHER WE GET SAME DISTANCE WITH dist.fun IF WE MEASURE SUM OF PIECES.
#' This seems to be true for deltaE2000.
#'
#' To the extent the
#' `colour` path used to build the palette function is not straight, it is
#' possible for shortest distance between two points to be
#'
#' that are equal between input points should be approximately equal
#'
#' Uniformity is measured as approximately as per `dist.fun` along the piecewise
#' path through the `colours` vector in L*a*b* space.
#'
#' The resulting `palette` function will convert values between 0 and 1 into the
#' colour that corresponds to the location along the colour path that is that
#' fraction of the full length of the colour path as measured by the cumulative
#' sum of the distances between adjacent colours.
#' attempt to approximate the colour that
#' corresponds to
#'
#' A number of colours is interpolated along the shortest Euclidean path in
#' L*a*b* space that passes through all the colours specified in `colours`, in
#' the order they are specified.  The number will be such that it is more than
#' `min.colors` and large enough that the distance measured with `dist.fun`
#' between any two adjacent colours is less than `min.dist`.  This allows us to
#' easily compute cumulative distances as per `dist.fun` along the colour path.
#' If we need to compute a cumulative distance that lies between two of the
#' computed colours, we interpolate that partial distance using the full
#' distance as per `dist.fun` between the two nearest known colours multiplied
#' by the ratio of the partial Euclidean distance to the full Euclidean distance
#' measured *along* the colour path (i.e., not in 3D space).
#'
#' The palette function can then be given a value between `from` and `to` and
#' the colour that is approximately at the same point along the colour path as
#' measured by `dist.fun` will be returned.
#'
#' @param colours character a vector of colours to base the palette on, must
#'   have at least two colours, and the colours should be in a format understood
#'   by [`grDevices::col2rgb`].
#' @param mid integer(1L) in `seq_along(colours)`, indicates which element in
#'   `colours` is to be treated as the midpoint colour.
#' @seealso [`ggplot2::scale_fill_gradientn`], [`grDevices::col2rgb`]
#' @param dist.fun a function that computes distances between colors in the
#'   L*a*b* color space.  The function should have two parameters, which will
#'   be 3 column matrices containing the L, a, and b coordinates or the colours.

palette_equi_div <- function(colours, mid, na.value, dist.fun=deltaE2000) {
}
palette_equi <- function(
  colours, na.value, from=0, to=1, dist.ctl,
) {
  vetr(colours=character() && length(.) > 1)
  col.spc <- color_to_space(colours)

  # For each input segment, compute the total distance, determine how many
  # segments we need, interpolate them, and equalize them.  This may not be the
  # most efficient since we wrote `make_coord_funs` to deal with the entire
  # path, not each segment, so there should be much simpler calculations we
  # could use.

  equal_sub_seg <- function(i) {
    seg.dist <- try(dist_fun(col.spc[i,], col.spc[i + 1L,], ...))
    dist.err.warn <- FALSE
    if(inherits(seg.dist, "try-error")) {
      stop(
        "Argument `dist_fun` produced an error when computing the distance ",
        "between '", colours[i],"' and '", colours[i + 1L], "'.  See previous ",
        "error."
      )
    }
    if(seg.dist > min_dist) {
      f_spc <- make_coord_funs(col.spc[i + 0:1, ])
      sub.segs <- ceil(seg.dist / min_dist)
      nc <- sub.segs + 1
      sub.pos <- seq(0, 1, length.out=nc)
      seg.coords <- f_spc$pos_to_coords(sub.pos, ...)
      seg.coords.e <- equalize_dists2(seg.coords, dist_fun, f_spc, ...)

      dist.tot <-
        dist_fun(seg.coords.e[1L, , drop=FALSE], seg.coords.e[nc, , drop=FALSE])
      dist.pieces <- sum(
        dist_fun(
          seg.coords.e[-nc, , drop=FALSE], seg.coords.e[-1L, , drop=FALSE]
      ) )
      if(
        !dist.err.warn &&
        abs(dist.tot - dist.pieces) >
        dist_ctl[['warn.if.sub.dist.mismatch.tol']]
      ) {
        dist.err.warn <- TRUE
        warning(
          "Argument `dist_fun` produces different lengths when computing ",
          "the length for a segment vs the sum of the lengths of subsegments ",
          "it comprises.  This will cause distance calculations along the ",
          "palette to be suspect."
        )
      }
    } else {
      col.spc[i + 0:1, ]
    }
  }
  sub.segs <-
    lapply(seq(from=1L, to=length(colours) - 1L, by=1L), equal_sub_seg)

  col.spc.fin <- do.call(rbind, sub.segs)
  nr.fin <- nrow(col.spc.fin)


  #

    !=
    sum(
    )

      if(
        sum(
          dist.fun(seg.coords.e[-1L, ], seg.coords[-nrow(seg.coords.e), ])
        ) != seg.dist
      )

  # We want some number of distinct colors to do our look

  col.lab.e <- do.call(equalize_dists2, c(list(col.lab, f_lab), dist.ctl))

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



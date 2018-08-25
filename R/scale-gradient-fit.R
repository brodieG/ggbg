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
#' Create a Colour Ramp Function That Produces Uniform Colours
#'
#' The colour ramp function converts values between 0 and 1 into colours
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
#' the other, or as the sum of the lengths of sub-segments that end to end
#' correspond to the same segment (i.e., the "Whole is Equal to the Sum Of Its
#' Parts").  Additionally as we move from one point to the other along the
#' shortest Euclidean path in the three dimensional colour space, the value
#' `dist_fun` returns should decrease, though it need not do so in proportion
#' to the Euclidean distance traveled.
#'
#' Upon creation of the function, each segment in `colours` is subdivided into
#' enough pieces such that the distance spanned by each piece is less than the
#' `jnd`, or the "Just-Noticeable Difference".  The value-to-colour map is then
#' carried out by finding the segment that causes the cumulative accrued
#' `dist_fun` distance as a fraction of the total cumulative `dist_fun` distance
#' of the path to exceed the value.  The actual colour is then interpolated
#' linearly in that segment in the provided colour space.
#'
#' The complexity of this function and the resulting colour ramp function is
#' designed to accommodate the CIEDE2000 colour distance function, implemented
#' in this package as [`deltaE2000`], while also allowing users to specify
#' custom colour spaces and distance functions.
#'
#' @export
#' @seealso [`ggplot2::scale_fill_gradientn`], [`grDevices::col2rgb`],
#' [`grDevices::colorRamp`].
#' @param colours character a vector of colours to base the ramp on, must
#'   have at least two colours, and the colours should be in a format understood
#'   by [`grDevices::col2rgb`].
#' @param mid integer(1L) in `seq_along(colours)`, indicates which element in
#'   `colours` is to be treated as the midpoint colour.
#' @param dist_fun a function that computes distances between colors in a three
#'   dimensional colour space.  The function should have two parameters, which
#'   will be 3 column matrices containing the coordinates of the colours to
#'   compute the distances between.  The function should compute the distances
#'   between matching rows across the two matrices.
#' @param na.value character(1L) a colour in a format understood by
#'   [`grDevices::col2rgb`] to use to represent NA values, named to be
#'   consistent with `ggplot2` conventions.
#' @param jnd numeric(1L) strictly positive, indicates the `dist_fun` distance
#'   that is considered a "Just-noticeable Difference".
#' @param ... arguments to pass on to `dist_fun`.
#' @param col_to_space a function that converts colors of the type accepted by
#'   [`col2rgb`] into a 3 column matrix representing coordinates of the same
#'   colour in the target colour space.  The default colour space is L*a*b*.
#' @param space_to_col a function that carries out the reverse of the
#'   that `col_to_space` transformation.
#' @param eq_ctl list, advanced usage only, arguments to pass on to
#'   [`equalize_dists2`].  [`equalize_dists2`] is used to make the sub-segments
#'   we create interpolate out of the `colours` path equi-distance.  You may
#'   only pass `tol`, `iters`, and `quiet` as the other arguments are derived
#'   from arguments to this function.
#' @param wesoip_tol numeric(1L) how closely the "Whole Equal to Sum Of Its
#'   Parts" must hold to suppress warnings about this condition not being met
#'   when we breakdown segments of the `colours` path into sub-segments.  Set to
#'   `Inf` to suppress warnings.  Tolerance is measured against the absolute
#'   difference of the sum of the parts and the whole divided by the whole.
#' @return a function that accepts a numeric parameter as an input, and for the
#'   values between `from` and `to` returns a corresponding colour along the
#'   `colours` path.

colour_ramp_equi <- function(
  colours, na.value, dist_fun=deltaE2000, ..., jnd=1,
  col_to_space=color_to_lab,
  space_to_col=lab_to_color, from=0, to=1,
  eq_ctl=list(tol=1e-3, iters=1e4, quiet=TRUE),
  wesoip_tol=sqrt(.Machine$double.eps)
) {
  vetr(
    colours=character() && length(.) > 1, na.value=CHR.1,
    jnd=NUM.1 && . > 0,
    col_to_space=is.function(.),
    space_to_col=is.function(.),
    eq_ctl=list(
      tol=numeric(1L), iters=numeric(1L), quiet=logical(1L)
    ),
    from=NUM.1 && all_bw(., 0, 1),
    to=NUM.1 && all_bw(., 0, 1) && . > from,
    wesoip_tol=numeric(1L) && . >= 0
  )
  col.spc <- col_to_space(colours)

  # For each input segment, compute the total distance, determine how many
  # segments we need, interpolate them, and equalize them.  This may not be the
  # most efficient since we wrote `make_coord_funs` to deal with the entire
  # path, not each segment, so there should be much simpler calculations we
  # could use.

  sub.segs <- lapply(
    seq(from=1L, to=length(colours) - 1L, by=1L),
    equalize_segments, colours=colours, col.spc=col.spc, dist_fun=dist_fun,
    eq_ctl=eq_ctl, jnd=jnd, wesoip_tol, ...
  )
  col.spc.fin <- do.call(rbind, sub.segs)

  # Compute the distances between adjacent colours and normalize

  dists <- dist_fun(
    col.spc.fin[-nrow(col.spc.fin), , drop=F],
    col.spc.fin[-1L, , drop=F], ...
  )
  dists.c <- cumsum(c(0, dists))

  dists.norm <- dists.c / max(dists.c) * (to - from) + from

  function(x) {
    vetr(is.numeric(.))
    x.na <- is.na(x)
    x.no.na <- x[!is.na(x)]
    bins <- findInterval(x.no.na, dists.norm, rightmost.closed=TRUE)
    offset <- (x.no.na - dists.norm[bins]) /
      (dists.norm[bins + 1] - dists.norm[bins])

    col.res <- try(
      space_to_col(
        col.spc.fin[bins, , drop=FALSE] + offset * (
          col.spc.fin[bins + 1, , drop=FALSE] - col.spc.fin[bins, , drop=FALSE]
        )
    ) )
    if(inherits(col.res, "try-error"))
      stop(
        "The 3D colourspace to string colour function this ramp function ",
        "was built with caused an error when converting from the colourspace ",
        "to the colours; see `col_to_space` parameter in `?colour_ramp_equi`."
      )
    if(!is.character(col.res) || length(col.res) != length(x.no.na))
      stop(
        "The 3D colourspace to string colour function this ramp function ",
        "did not return a character of the same length as the number of ",
        "rows in the 3D colourspace matrix input; see `col_to_space` ",
        "parameter in `?colour_ramp_equi`."
      )
    col.res.fin <- character(length(x))
    col.res.fin[!x.na] <- col.res
    col.res.fin[x.na] <- na.value
    col.res.fin
  }
}
ramp_equi_div <- function(colours, mid, na.value, dist_fun=deltaE2000) {
}

## Helper Function to Break Down Segments
##
## Breaks down segments into equal-length sub-segments

equalize_segments <- function(
  i, colours, col.spc, jnd, dist_fun, eq_ctl, wesoip_tol, ...
) {
  seg.dist <- try(
    dist_fun(col.spc[i, , drop=FALSE], col.spc[i + 1L, , drop=FALSE], ...)
  )
  if(inherits(seg.dist, "try-error")) {
    stop(
      "Argument `dist_fun` produced an error when computing the distance ",
      "between '", colours[i],"' and '", colours[i + 1L], "'.  See previous ",
      "error."
    )
  }
  if(seg.dist > jnd) {
    f_spc <- make_coord_funs(col.spc[i + 0:1, ])
    sub.segs <- ceiling(seg.dist / jnd)
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
      abs(dist.tot - dist.pieces) >
      wesoip_tol
    ) {
      browser()
      warning(
        "Argument `dist_fun` produces different lengths when computing ",
        "the length for a segment vs the sum of the lengths of subsegments ",
        "it comprises.  This will cause distance calculations along the ",
        "ramp to be suspect."
      )
    }
    seg.coords.e
  } else {
    col.spc[i + 0:1, ]
  }
}


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



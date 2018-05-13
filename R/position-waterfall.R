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

#' Cumulative Stacking of Objects
#'
#' A waterfall chart is a bar chart where each segment starts where the prior
#' segment left off.  This is similar to a stacked bar chart, except that
#' elements are offset along the `x` axis so that positive and negative values
#' do not overlap.  Another similar type of chart is the candle stick plot,
#' except those have "whiskers" and typically require you to manually specify
#' the `ymin` and `ymax` values.
#'
#' `position_waterfall` creates waterfall charts when it is applied to
#' `geom_col` or `geom_bar`.  You can apply it to any geom, although the results
#' may not always make sense, especially with geoms that explicitly specify
#' the `height`, `ymin`, and or `ymax` aesthetics.  The stacking is always
#' computed from the `y` values.  Since stat layers are computed prior to
#' position adjustments, you can also use `position_waterfall` with stats (e.g
#' `stat_bin`, see examples).
#'
#' Most position adjustments modify positions of groups that otherwise
#' would occupy the same space, leaving relative positions of groups unchanged.
#' `position_waterfall` also changes relative positions of groups that would
#' normally not occupy the same space.
#'
#' @inheritParams ggplot2::position_dodge
#' @inheritParams ggplot2::position_stack
#' @param dodge TRUE (default) or FALSE, controls how to resolve
#'   groups that overlap on the `x` axis.  The default is to dodge them
#'   to form mini-waterfalls within each `x` value, but you can chose to stack
#'   them instead by setting `dodge=FALSE`.  Negative and positive values are
#'   segregated prior to stacking so they do not overlap.  Interpreting
#'   waterfall charts with stacked sub-groups is difficult, so we recommend
#'   you use the default setting instead.  Observations within a group that
#'   have the same `x` value are always stacked.
#' @param vjust like the `vjust` parameter for [ggplot2::position_stack], except
#'   that by default the direction of justification follows the direction of the
#'   bar (see `vjust.mode`).  This only has an effect on geoms with positions
#'   like text, points, or lines.  The default position is `0.5`, which places
#'   elements midway through the height of the corresponding waterfall step.
#'   The default value is convenient for labeling `geom_col` waterfalls.  Use
#'   `1` to position at the "end" of each waterfall step.
#' @param vjust.mode character(1L), one of "end" (default), or "top" where "top"
#'   results in the same behavior as in [ggplot2::position_stack].  "end" means
#'   the justification is relative to the "end" of the waterfall bar.  So if a
#'   waterfall bar is heading down (i.e. negative `y` value), the "end" is at
#'   the bottom.  If it heading up (i.e. positive `y` value), the "end" is at
#'   the top.  For positive `y` values "end" and "top" do the same thing.
#' @export
#' @param dodge TRUE or FALSE (default), whether to dodge waterfall bars when
#'   there are multiple bars for a single x value.
#' @examples
#' ## These examples are best run via `example(position_waterfall)`
#' library(ggplot2)
#' dat <- data.frame(x=1:3, y=1:3)
#' p1 <- ggplot(dat, aes(x=x, y=y)) + geom_col(position='waterfall')
#'
#' ## Add text or labels; defaults to middle waterfall position
#' ## which can be modified with `vjust`
#' p1 + geom_label(aes(label=x), position='waterfall')
#'
#' ## We use the `geom` `vjust` value AND the waterfall
#' ## `vjust` value to get our labels to the top of the bar
#' p1 + geom_label(
#'   aes(label=x),
#'   position=position_waterfall(vjust=1), # position vjust
#'   vjust=0                               # geom vjust
#' )
#'
#' ## We can use arbitrary geoms
#' ggplot(dat, aes(x=x, y=y)) +
#'   geom_point() +
#'   geom_point(position=position_waterfall(vjust=1), color='red')
#'
#' ## Or stats; here we turn a histogram into an ecdf plot
#' dat.norm <- data.frame(x=rnorm(1000))
#' ggplot(dat.norm, aes(x=x)) + geom_histogram(position='waterfall')
#' ggplot(dat.norm, aes(x=x)) + stat_bin(position='waterfall')
#'
#' ## Data with groups
#' dat3 <- data.frame(
#'   x=c(3, 2, 2, 2, 1, 1), y=c(-3, 1, 4, -6, -1, 10),
#'   grp=rep(c("A", "B", "C"), lenght.out=6)
#' )
#' p2 <- ggplot(dat3, aes(x=x, y=y, fill=grp))
#' p2 + geom_col(position="waterfall")
#'
#' ## Equal width columns
#' p2 + geom_col(position=position_waterfall(preserve='single'))
#'
#' ## Stacking groups is possible, bug hard to interpret when
#' ## negative values present
#' p2 + geom_col(position=position_waterfall(dodge=FALSE))





position_waterfall <- function(
  width = NULL,
  preserve = c("total", "single"),
  reverse = FALSE,
  dodge = TRUE,
  vjust = 0.5,
  vjust.mode = "end"
) {
  vetr(dodge=LGL.1, vjust=NUM.1, vjust.mode=CHR.1 && . %in% c("top", "end"))

  ggproto(NULL, PositionWaterfall,
    width = width,
    preserve = match.arg(preserve),
    reverse = reverse,
    vjust=vjust,
    vjust.mode=vjust.mode,
    dodge = dodge
  )
}

## Much of this code is lifted from ggplot2/R/position-dodge.R

#' @rdname ggbg-ggproto
#' @format NULL
#' @usage NULL
#' @export

PositionWaterfall <- ggproto(
  "PositionWaterfall", Position,
  name = "position_waterfall",
  width = NULL,
  reverse = FALSE,
  preserve = "total",
  dodge = TRUE,
  vjust = 0.5,
  vjust.mode="end",

  setup_params = function(self, data) {
    if (identical(self$preserve, "total")) {
      n <- NULL
    } else {
      n <- max(table(data$xmin))
    }
    list(
      width = self$width,
      n = n,
      dodge = self$dodge,
      reverse = self$reverse,
      vjust = self$vjust,
      vjust.mode = self$vjust.mode
    )
  },
  # try to make sure that data has x, xmin, xmax, and ymin and ymax

  setup_data = function(self, data, params) {
    # Deal with `x` vals

    if (
      !"x" %in% names(data) & isTRUE(all(c("xmin", "xmax") %in% names(data)))
    ) {
      if(isTRUE(any(data[["xmin"]] > data[["xmax"]])))
        warning(
          "Some `xmin` values are greater than `xmax` values for `", self$name,
          "`, this may cause unexpected outcomes."
        )
      data$x <- (data$xmin + data$xmax) / 2
    } else if (
      "x" %in% names(data) && !isTRUE(all(c("xmin", "xmax") %in% names(data)))
    ) {
      if(sum(c("xmin", "xmax") %in% names(data)) == 1)
        warning(
          "Only one of `xmin` and `xmax` available to `", self$name, "`, ",
          "this may cause unexpected outcomes."
        )

      data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)

      if(!is.numeric(data[["width"]]) || isTRUE(any(data[["width"]] < 0)))
        warning(
          "Non-numeric or negative `width` values provided to `", self$name,
          "`, this may cause unexpected outcomes."
        )

      data[["xmin"]] <- data[["x"]] - data[["width"]] / 2
      data[["xmax"]] <- data[["x"]] + data[["width"]] / 2
    }
    # Deal with `y` vals

    if(!"y" %in% names(data)) {
      # compute panel will issue a warning if "y" is missing, so no need to do
      # it here
      if(isTRUE(all(c("ymin", "ymax") %in% names(data)))) {
        data[["y"]] <- (data[["ymin"]] + data[["ymax"]]) / 2
      }
    } else {
      data[["ymin"]] <- pmin(data[["y"]], 0)
      data[["ymax"]] <- pmax(data[["y"]], 0)
    }
    data
  },
  # as per setup_params, params should have width and n, though n could be null
  # if null, we want to take up the entire width

  compute_panel = function(self, data, params, scales) {
    if(!all(c("xmin", "xmax", "x", "ymin", "ymax", "y") %in% names(data))) {
      warning(
        "Computed data for `", self$name,
        "` must contain `x`, `xmin`, `xmax`, `y`, `ymin`, and `ymax` ",
        "columns, and since it doesn't we cannot apply `", self$name, "`."
      )
      data
    } else {
      # group by xmin, and then stack / dodge, we also need to track the
      # cumulative height of the previous bars

      data <- data[
        order(data[["xmin"]], data[["group"]] * if(params$reverse) -1 else 1),
        , drop=FALSE
      ]
      y.cum <- cumsum(data[["y"]])
      y.cum.last <- tapply(y.cum, data[["xmin"]], tail, 1L)
      prev.last <- c(0, head(y.cum.last, -1))

      d.s <- split(data, data[["xmin"]])

      d.s.proc <- Map(
        pos_waterfall, df=d.s, width=list(params$width), dodge=params$dodge,
        y.start=prev.last, n=list(params$n), vjust=params$vjust,
        vjust.mode=params$vjust.mode
      )
      do.call(rbind, d.s.proc)
    }
  }
)

# Dodge overlapping interval.
# Assumes that each set has the same horizontal position.

pos_waterfall <- function(
  df, width, dodge, y.start, vjust, vjust.mode, n = NULL
) {
  if (is.null(n)) {
    group.u <- unique(df[["group"]])
    n <- length(unique(df[["group"]]))
    group.map <- match(df[["group"]], group.u)
  } else {
    group.map <- df[["group"]]
  }
  if(!is.numeric(n) || length(n) != 1L || is.na(n) || n < 0) {
    warning(
      "Internal error: failed computing 'position_waterfall' due to ",
      "unexpected `n`, contact maintainer."
    )
  } else {
    df <- if(dodge) {
      # dodge mode; lifted directly from ggplot2/R/position-dodge.R
      #
      # Find the center for each group, then use that to calculate xmin and xmax

      if(n > 1) {
        if(!all(group.map > 0 & group.map <= n)) {
          warning(
            "Internal Error: unexpected group numbers in 'position_waterfall'.",
            "  Dodging disabled, contact maintainer."
          )
        } else {
          d_width <- max(df$xmax - df$xmin)
          if(is.null(width)) width <- d_width
          df$x <- df$x + width * ((group.map - 0.5) / n - .5)
          df$xmin <- df$x - d_width / n / 2
          df$xmax <- df$x + d_width / n / 2
        }
      }
      stack_waterfall(df, y.start, vjust, vjust.mode)
    } else {
      # stack mode, need to segregate positives and negatives

      df.pos <- df[df[["y"]] >= 0, , drop=FALSE]
      df.neg <- df[df[["y"]] < 0, , drop=FALSE]
      rbind(
        stack_waterfall(df.pos, y.start, vjust, vjust.mode),
        stack_waterfall(df.neg, y.start, vjust, vjust.mode)
      )
    }
  }
  df
}
stack_waterfall <- function(df, y.start, vjust, vjust.mode) {
  y.all <- c(y.start, df[["y"]])
  y.cum <- cumsum(y.all)
  y.lead <- head(y.cum, -1L)
  y.lag <- tail(y.cum, -1L)

  y.orig <- df[["y"]]
  df[["y"]] <- y.lag
  df[["ymin"]] <- pmin(y.lead, df[["y"]])
  df[["ymax"]] <- pmax(y.lead, df[["y"]])
  # adjust v position
  df[["y"]] <- ifelse(
    y.orig < 0 & identical(vjust.mode, "end"),
    df[["ymax"]] - vjust * (df[["ymax"]] - df[["ymin"]]),
    df[["ymin"]] + vjust * (df[["ymax"]] - df[["ymin"]])
  )
  df
}

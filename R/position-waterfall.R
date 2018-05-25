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

#' Stack Chart Elements on Cumulative Value
#'
#' A waterfall chart is a bar chart where each segment starts where the prior
#' segment left off.  This is similar to a stacked bar chart, except that
#' the stacking does not reset across `x` values.  In effect, it is the
#' visualization of a cumulative sum.  Another similar type of chart is the
#' candle stick plot, except those have "whiskers" and typically require you to
#' manually specify the `ymin` and `ymax` values.
#'
#' `position_waterfall` creates waterfall charts when it is applied to
#' `geom_col` or `geom_bar`.  You can apply it to any geom, so long as the
#' geom specifies a `y` aesthetic, although there is no guarantee the result
#' will make sense for arbitrary geoms.  The stacking is always
#' computed from the `y` aesthetic.  The order of the stacking is determined by
#' the `x` aesthetic.
#'
#' If only `xmin` and `xmax` aesthetics are present the `x` value will be
#' inferred as the midpoint of those two.  If the `xmin` and `xmax` aesthetics
#' are missing `position_waterfall` will attempt to infer them from `x` and the
#' provided or implicit `width`.
#'
#' Since stat layers are computed prior to position adjustments, you can also
#' use `position_waterfall` with stats (e.g `stat_bin`, see examples).
#'
#' We also implement a [StatWaterfall][ggbg-ggproto] `ggproto` object that
#' can be accessed within `geom_*` calls by specifying `stat='waterfall'`.
#' Unlike typical stat `ggproto` object, this one does not have a layer
#' instantiation function (i.e. `stat_waterfall` does not exist).  The sole
#' purpose of the stat is to compute the `ycum` aesthetic that can then be used
#' by the `geom` layer (see the labeling examples).
#'
#' Most `position_*` adjustments modify positions of groups that otherwise
#' would occupy the same space, leaving relative positions of groups unchanged.
#' `position_waterfall` is different in as much as it also changes relative
#' positions of groups that would normally not occupy the same space.
#'
#' @inheritParams ggplot2::position_dodge
#' @inheritParams ggplot2::position_stack
#' @param dodge TRUE (default) or FALSE, controls how to resolve
#'   groups that overlap on the `x` axis.  The default is to dodge them
#'   to form mini-waterfalls within each `x` value, but you can chose to stack
#'   them instead by setting `dodge=FALSE`.  Negative and positive values are
#'   segregated prior to stacking so they do not overlap.  Interpreting
#'   waterfall charts with stacked sub-groups is difficult when they contain
#'   negative values, so we recommend you use the default setting instead.
#'   Observations within a group that have the same `x` value are always
#'   stacked, so if you have both positive and negative values for any given `x`
#'   value you may want to consider segregating the positives and negatives in
#'   different groups.
#' @param vjust like the `vjust` parameter for [`ggplot2::position_stack`],
#'   except that by default the direction of justification follows the direction
#'   of the bar (see `vjust.mode`), and the default value is `0.5` instead of
#'   `1`.  This only has an effect on geoms with positions like text, points, or
#'   lines.  The default setting places elements midway through the height of
#'   the corresponding waterfall step.  The default value is convenient for
#'   labeling `geom_col` waterfalls.  Use `1` to position at the "end" of each
#'   waterfall step.  This is different to the `vjust` for geoms like
#'   `geom_text` where `vjust=1` shift the text down, but it is consistent with
#'   what [`gggplot2::position_stack`] does.
#' @param vjust.mode character(1L), one of "end" (default), or "top" where "top"
#'   results in the same behavior as in [`ggplot2::position_stack`].  "end"
#'   means the justification is relative to the "end" of the waterfall bar.  So
#'   if a waterfall bar is heading down (i.e. negative `y` value), the "end" is
#'   at the bottom.  If it heading up (i.e. positive `y` value), the "end" is at
#'   the top.  For positive `y` values "end" and "top" do the same thing.
#' @export
#' @param dodge TRUE or FALSE (default), whether to dodge waterfall bars when
#'   there are multiple bars for a single x value.
#' @examples
#' ## These examples are best run via `example(position_waterfall)`
#' library(ggplot2)
#' dat <- data.frame(x=3:1, y=1:3)
#' p1 <- ggplot(dat, aes(x=x, y=y)) + geom_col(position='waterfall')
#'
#' ## Add text or labels; defaults to middle waterfall position
#' ## which can be modified with `vjust`
#' p1 + geom_label(aes(label=x), position='waterfall')
#'
#' ## We can also add the cumulative running to the top of
#' ## the bars with `stat='waterfall'` and position adjustments
#' p1 + geom_label(aes(label=x), position='waterfall') +
#'  geom_label(
#'    stat="waterfall",        # adds `ycum` computed variable
#'    aes(label=stat(ycum)),   # which we can use for label
#'    position=position_waterfall(vjust=1), # text to end of column
#'    vjust=0,                              # tweak so it's on top
#' )
#' ## A poor person's candlestick chart:
#' dat.r.walk <- data.frame(x=1:20, y=rnorm(20))
#' ggplot(dat.r.walk, aes(x=x, y=y, fill=y > 0)) +
#'   geom_col(position='waterfall')
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

#' Compute Position Adjustments Based on Cumulative Value
#'
#' `PositionWaterfall` is the `ggproto` object used to generate the position
#' adjustments that correspond to [position_waterfall].
#'
#' @rdname ggbg-ggproto
#' @format NULL
#' @usage NULL
#' @export

PositionWaterfall <- ggproto(
  "PositionWaterfall", Position,
  name = "position_waterfall",
  width = NULL,
  width.geom = NULL,
  width.default = 1,
  reverse = FALSE,
  preserve = "total",
  dodge = TRUE,
  vjust = 0.5,
  vjust.mode="end",
  has.x.width=FALSE,
  groups=integer(),

  setup_params = function(self, data) {
    x.check <- all(c("xmin", "xmax") %in% names(data))
    self$has.x.width <-
      is.numeric(data[["xmin"]]) && is.numeric(data[["xmax"]]) &&
      !anyNA(data[['xmin']]) && !anyNA(data[['xmax']]) &&
      all(data[['xmax']] >= data[['xmin']])

    if(!self$has.x.width && any(x.check)) {
      warning(
        "Cannot interpret 'xmin' and 'xmax' aesthetics either because ",
        "one of them is missing, either contains missing values, either ",
        "is not numeric, or some 'xmax' values are less than the corresponding ",
        "'xmin' values.  Geom width adjustments will not be applied by `",
        self$name, "`."
      )
    }
    if(is.null(width.geom)) {
      # We want to know if the geom comes with a consistent width on `xmin` and
      # `xmax` (those should have been created by Geom$setup_data by this point)

      if(self$has.x.width) {
        width.tmp <- unique(data[["xmax"]] - data[["xmin"]])
        if(length(width.tmp) == 1 && !is.na(width.tmp) && width.tmp > 0)
          width.geom <- width.tmp
      }
    }
    if(isTRUE(preserve == 'single') && is.null(width.geom)) {
      warning(
        "`preserve='single'` for `", self$name, "` only works if all widths ",
        "for a layer as implied by 'xmin' and 'xmax' are the same, positive, ",
        "and not NA.  Proceeding with `preserve='total'`."
      )
      preserve <- "total"
    }
    if("x" %in% names(data)) {
      self$width.default = resolution(data[['x']], FALSE) * 0.9
    }
    list(
      width = self$width,
      preserve = self$preserve,
      dodge = self$dodge,
      reverse = self$reverse,
      vjust = self$vjust,
      vjust.mode = self$vjust.mode,
      width.geom = width.geom,
      width.default = self$width.default,
      has.x.with = self$has.x.width,
      groups=sort(unique(data[['group']]))
    )
  },
  # We don't want to modify the data at this point because we don't want to add
  # aesthetics to the data frame, so we'll do everything at the compute panel
  # step

  setup_data = function(self, data, params) {
    data
  },

  compute_panel = function(self, data, params, scales) {
    # Cases are:
    # * preserve == 'single' && width.geom is not null
    # * width.geom is not null
    # * width.geom is null, but there are defined xmin/xmax
    # * no xmin/xmax but dodge width specified
    # * no xmin/xmax and dodge width not specified
    #
    # The dodging does two things: changes the xmin/xmax distance
    # Shifts the x values around.
    #
    # If preserve == 'single' && width.geom availablejjjjjjj

    stop("x may not be available")

    check.x <- c("xmin", "xmax") %in% names(data)

    if(isTRUE(params$preserve == 'single')) {
      g.width <- params$geom.width

    }
    if(!all(check.x) && any(check.x)) {
      warning(
        "One of 'xmin' and 'xmax' aesthetic is missing and we do not know ",
        "how to infer the other one; `", self$name, "` will not be applied."
      )
    } else if (!all(check.x)) {
      warning(
        "'xmin' and 'xmax' aesthetics are missing and could not be inferred; `",
        self$name, "` will not be applied."
      )
    } else if (!"x" %in% names(data)) {
      warning(
        "'x' aesthetic is missing and could not be inferred; `", self$name,
        "` will not be applied."
      )
    } else if (!"y" %in% names(data)) {
      warning(
        "'y' aesthetic is missing and could not be inferred; `", self$name,
        "` will not be applied."
      )
    } else {
      # group by x, and then stack / dodge, we also need to track the
      # cumulative height of the previous bars

      data <- data[
        order(data[["x"]], data[["group"]] * if(params$reverse) -1 else 1),
        , drop=FALSE
      ]
      y.cum <- cumsum(data[["y"]])
      y.cum.last <- tapply(y.cum, data[["x"]], tail, 1L)
      prev.last <- c(0, head(y.cum.last, -1))

      d.s <- split(data, data[["x"]])

      d.s.proc <- Map(
        pos_waterfall, df=d.s, width=list(params$width), dodge=params$dodge,
        y.start=prev.last, n=list(params$n), vjust=params$vjust,
        vjust.mode=params$vjust.mode, groups=params$groups
      )
      data <- do.call(rbind, d.s.proc)
    }
    data
  }
)
# Dodge overlapping interval.
# Assumes that each set has the same horizontal position.

pos_waterfall <- function(
  df, width, width.geom, width.default, dodge, y.start, vjust, vjust.mode,
  n, has.x.width, groups
) {
  group.map <- match(df[['group']], groups)

  df <- if(dodge) {
    d.widths <- if(!is.null(width)) rep(width, length.out=nrow(df))
      else if (!is.null(width.geom)) rep(width.geom, length.out=nrow(df))
      else if (!identical(preserve, single) && has.x.width)
        df[['xmax']] - df[['xmin']]
      else rep(width.default, length.out=nrow(df))

    # If the geoms have defined xmin/xmax then adjust them for the dodging
    # effect (which will make them narrower)

    if(has.x.width) {
      widths <- df[['xmax']] - df[['xmin']]
      # possible for there to be multiple values within a group; take the max
      # width

      group.widths <- tapply(widths, group.map, max)

      if(identical(preserve, 'single')) {
        width.scale <- width.geom * n
      } else {
        width.scale <- sum(group.widths)
        width.geom <- max(group.widths)
      }
      widths.fin <- widths / width.scale * width.geom
      df.mid <- df[['xmin']] + widths / 2
      df[['xmin']] <- df.mid - widths.fin / 2
      df[['xmax']] <- df.mid + widths.fin / 2
    }
    # Now compute dodge widths, which may be done with a different width than
    # the `xmin` / `xmax` one if one was specified

    if(identical(preserve, 'single')) {
      d.width.geom <- d.widths
      d.width.scale <- width.geom * n
    } else {
      d.width.scale <- sum(d.widths)
      d.width.geom <- max(d.widths)
    }
    d.widths.fin <- d.widths / d.width.scale * d.width.geom

    # and the dodge offsets.

    grp.ord <- match(df[['group']], unique(df[['group']]))




    g.width <- if(!is.null(width.geom)) width.geom
      else if (
        is.numeric(data[['xmin']]) && is.numeric(data[['xmax']])

    if(is.null(width
    if(!is.null(width.geom))

    #
    # Find the center for each group, then use that to calculate xmin and xmax

    if(n > 1) {
      if(!all(group.map > 0 & group.map <= n)) {
        warning(
          "Internal Error: unexpected group numbers in 'position_waterfall'.",
          "  Dodging disabled, contact maintainer."
        )
      } else {
        # Need to deal with:
        # - non-specified width
        # - different dodge width relative to actual width
        # - what

        d_width <- if(!is.null(width)) {
          width
        } else if(!is.null(df[["width"]])) {
          df[["width"]]
        } else if(all(c("xmin", "xmax") %in% names(df))) {
          df[["xmax"]] - df[["xmin"]]
        } else {
          resolution(data[["x"]], FALSE) * 0.9
        }
        d_width <- rep(d_width, length.out=nrow(df))

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
if (is.null(n)) {
    group.u <- unique(df[["group"]])
    n <- length(unique(df[["group"]]))
    group.map <- match(df[["group"]], group.u)
  } else {
    group.map <- df[["group"]]
  }

  } else {
  }
  df
}
stack_waterfall <- function(df, y.start, vjust, vjust.mode) {
  y.all <- c(y.start, df[["y"]])
  y.cum <- cumsum(y.all)
  y.lead <- head(y.cum, -1L)
  y.lag <- tail(y.cum, -1L)

  y.orig <- df[["y"]]
  y.min <- pmin(y.lead, y.lag)
  y.max <- pmax(y.lead, y.lag)

  df[["y"]] <- y.lag

  # adjust v position

  df[["y"]] <- ifelse(
    y.orig < 0 & identical(vjust.mode, "end"),
    y.max - vjust * (y.max - y.min),
    y.min + vjust * (y.max - y.min)
  )
  if("ymin" %in% names(df)) df[["ymin"]] <- y.min
  if("ymax" %in% names(df)) df[["ymax"]] <- y.max
  df
}

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
#' candlestick plot, except those have "whiskers" and typically require you to
#' manually specify the `ymin` and `ymax` values.
#'
#' `position_waterfall` creates waterfall charts when it is applied to
#' `geom_col` or `geom_bar`.  You can apply it to any geom, so long as the
#' geom specifies a `y` aesthetic, although there is no guarantee the result
#' will make sense for arbitrary geoms.  The stacking is always
#' computed from the `y` aesthetic.  The order of the stacking is determined by
#' the `x` aesthetic.  The actual position of the objects are also affected by
#' `vjust`, and you may need to change the value of `vjust` if you are using
#' `position_waterfall` with geoms other than columns.
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
#' Unlike typical stat `ggproto` objects, this one does not have a layer
#' instantiation function (i.e. `stat_waterfall` does not exist).  The sole
#' purpose of the stat is to compute the `ycum` aesthetic that can then be used
#' by the `geom` layer (see the labeling examples).
#'
#' @section Dodging:
#'
#' Unlike most `position_*` adjustments, `position_waterfall` adjust positions
#' across different `x` values.  However, we still need to resolve `x`
#' value overlaps.  The default approach is to apply the same type of adjustment
#' across groups within any given `x` value.  This stacks and dodges elements.
#'
#' Dodging involves changing the `width` of the geom and also shifting the
#' `geom` horizontally.  Width adjustments will always be made based on the
#' `xmin`/`xmax`/`width` aesthetic, irrespective of whether you specify the
#' dodge `position_waterfall(width=...)`.  Horizontal shifts can be
#' controlled separately by using `position_waterfall(width=...)`.  Unlike the
#' `width` parameter specified via geom/stat `mapping` that affects the display
#' width of graphical elements (i.e. `aes(width=...)`),
#' `position_waterfall(width=...)` affects only the horizontal shifting of
#' groups with equal `x` values.
#'
#' Alternatively you can turn off dodging within `x` values by setting
#' `position_waterfall(dodge=FALSE)` which will result in stacking within each
#' `x` value.
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
#' @param signif integer(1L) between 1 and 22, defaults to 11, corresponds to
#'   the `digits` parameter for [`signif`] and is used to reduce the precision
#'   of numeric `x` aesthetic values so that stacking is not foiled by double
#'   precision imprecision.
#' @param y.start numeric(1L), defaults to 0, will be starting point for the
#'   cumulative sum of `y` values.  This could be useful if you want to combine
#'   waterfalls with other layers and need the waterfall to start at a specific
#'   value.
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
#'   geom_point(position='waterfall', color='blue') + # default vjust=0.5
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
  vjust.mode = "end",
  signif = getOption('ggbg.signif'),
  y.start = 0
) {
  vetr(
    dodge=LGL.1, vjust=NUM.1, vjust.mode=CHR.1 && . %in% c("top", "end"),
    signif=INT.1 && . >= 1 && . <= 22, y.start=NUM.1
  )
  ggproto(NULL, PositionWaterfall,
    width = width,
    preserve = match.arg(preserve),
    reverse = reverse,
    vjust=vjust,
    vjust.mode=vjust.mode,
    dodge = dodge,
    signif = signif,
    y.start = y.start
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
  width = NULL,              # dodge width
  width.geom.unique = NULL,  # all geoms have same width
  width.default = 1,
  reverse = FALSE,
  preserve = "total",
  dodge = TRUE,
  vjust = 0.5,
  vjust.mode="end",
  has.x.width=FALSE,         # xmin and xmax are present and reasonable
  has.width=FALSE,           # width aesthetic present and reasonable
  groups=integer(),          # all possible groups,
  # significance for x values to compute, width, overlap, etc.
  signif=11,
  y.start=0,

  setup_params = function(self, data) {

    signif <- self[['signif']]

    x.check <- all(c("xmin", "xmax") %in% names(data))
    self[['has.x.width']] <-
      is.numeric(data[["xmin"]]) && is.numeric(data[["xmax"]]) &&
      !anyNA(data[['xmin']]) && !anyNA(data[['xmax']]) &&
      all(data[['xmax']] >= data[['xmin']])

    if(!self[['has.x.width']] && any(x.check)) {
      warning(
        "Cannot interpret 'xmin' and 'xmax' aesthetics either because ",
        "one of them is missing, either contains missing values, either ",
        "is not numeric, or some 'xmax' values are less than the corresponding ",
        "'xmin' values.  Geom width adjustments will not be applied by `",
        self[['name']], "`."
      )
    }
    width.geom.unique <- self[['width.geom.unique']]
    if(is.null(width.geom.unique)) {
      # We want to know if the geom comes with a consistent width on `xmin` and
      # `xmax` (those should have been created by Geom$setup_data by this point)

      if(self[['has.x.width']]) {
        width.tmp <-
          unique(signif(data[["xmax"]] - data[["xmin"]], digits=signif))
        if(length(width.tmp) == 1 && !is.na(width.tmp) && width.tmp > 0)
          width.geom.unique <- width.tmp
      }
    }
    preserve <- self[['preserve']]
    if(isTRUE(preserve == 'single') && is.null(width.geom.unique)) {
      warning(
        "`preserve='single'` for `", self$name, "` only works if all widths ",
        "for a layer as implied by 'xmin' and 'xmax' are the same, positive, ",
        "and not NA.  Proceeding with `preserve='total'`."
      )
      preserve <- "total"
    }
    if("x" %in% names(data)) {
      self[['width.default']] =
        signif(resolution(data[['x']], FALSE) * 0.9, digits=signif)
    }
    if(
      "width" %in% names(data) && is.numeric(data[['width']]) &&
      all(data[['width']] > 0)
    ) {
      self[['has.width']] <- TRUE
    } else if('width' %in% names(data) && !has.x.width && is.null(width)) {
      warning(
        "'width' aesthetic contains values that are not strictly positive ",
        "numerics, so it cannot be used for dodging."
      )
    }
    list(
      width = self[['width']],
      preserve = self[['preserve']],
      dodge = self[['dodge']],
      reverse = self[['reverse']],
      vjust = self[['vjust']],
      vjust.mode = self[['vjust.mode']],
      width.geom.unique = width.geom.unique,
      width.default = self[['width.default']],
      has.x.width = self[['has.x.width']],
      has.width = self[['has.width']],
      groups=sort(unique(data[['group']])),
      signif=signif,
      y.start=self[['y.start']]
    )
  },
  # We don't want to modify the data at this point because we don't want to add
  # aesthetics to the data frame, so we'll do everything at the compute panel
  # step.  This is out of an abundance of caution to avoid messing with
  # downstream rendering of geoms.

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

    # We need a geom width, and we need a dodge width.


    check.x <- c("xmin", "xmax") %in% names(data)
    x <- if("x" %in% names(data)) signif(data[['x']], digits=params[['signif']])
    else if (params[['has.x.width']])
      signif(
        (data[['xmax']] - data[['xmin']]) / 2 + data[['xmin']],
        digits=params[['signif']]
      )
    else {
      warning(
        "Either 'x', or 'xmin' and 'xmax' must be specified; `",
        self$name, "` will not be applied."
      )
      NULL
    }
    if(!is.null(x)) {
      if(!"y" %in% names(data)) {
        warning(
          "'y' aesthetic is missing; `", self$name, "` will not be applied."
        )
      } else {
        # group by x, and then stack / dodge, we also need to track the
        # cumulative height of the previous bars

        ord.idx <- order(x, data[['group']] * if(params[['reverse']]) -1 else 1)
        data <- data[ord.idx , , drop=FALSE]
        x <- x[ord.idx]

        y.cum <- cumsum(c(params[['y.start']], data[["y"]]))
        y.cum.last <- tapply(tail(y.cum, -1L), x, tail, 1L)
        prev.last <- c(params[['y.start']], head(y.cum.last, -1))

        # For each `x` value, compute stacking and dodging

        d.s <- split(data, x)

        d.s.proc <- mapply(
          pos_waterfall,
          df=d.s,
          y.start=prev.last,
          MoreArgs=list(
            width=params[['width']],
            width.geom.unique=params[['width.geom.unique']],
            width.default=params[['width.default']],
            dodge=params[['dodge']],
            has.x.width=params[['has.x.width']],
            has.width=params[['has.width']],
            vjust=params[['vjust']],
            vjust.mode=params[['vjust.mode']],
            groups=params[['groups']],
            preserve=params[['preserve']],
            signif=params[['signif']],
            reverse=params[['reverse']]
          ),
          SIMPLIFY=FALSE
        )
        # Re-assemble and restore order of data

        data <- do.call(rbind, d.s.proc)[
          order(seq(length.out=length(ord.idx))[ord.idx]), , drop=FALSE
        ]
      }
    }
    data
  }
)
## Recompute widths based on group size

calc_width <- function(widths, width.geom.unique, group.map, groups, preserve) {
  # possible for there to be multiple values within a group, so pick max,
  # and make sure everything sorted by integer value of group map
  group.widths <- tapply(
    widths, factor(group.map, levels=seq_along(groups)), max
  )
  group.widths <- group.widths[order(as.integer(names(group.widths)))]

  if(identical(preserve, 'single')) {
    width.scale <- width.geom.unique * length(groups)
    group.widths[is.na(group.widths)] <- width.geom.unique
  } else {
    group.widths[is.na(group.widths)] <- 0
    width.scale <- sum(group.widths)
    width.geom.unique <- max(group.widths)
  }
  widths.fin <- widths / width.scale * width.geom.unique
  list(
    width=widths.fin, group.widths=group.widths,
    scale=width.geom.unique / width.scale
  )
}
# Dodging can handle different width as well as overlapping intervals.  Stacking
# is done relative to the `x` value (or midpoint of `xmin`/`xmax`)

pos_waterfall <- function(
  df, width, width.geom.unique, width.default, dodge, y.start,
  vjust, vjust.mode, has.x.width, has.width, groups, preserve, signif,
  reverse
) {
  group.map <- match(df[['group']], sort(groups, decreasing=reverse))

  df <- if(dodge) {
    geom.widths.raw <- if(has.x.width)
      df[['xmax']] - df[['xmin']] else if(has.width) df[['width']]

    dodge.widths.raw <- if(!is.null(width)) rep(width, length.out=nrow(df))
      else if (!is.null(geom.widths.raw)) geom.widths.raw
      else rep(width.default, length.out=nrow(df))

    geom.widths <- if(!is.null(geom.widths.raw))
      calc_width(
        geom.widths.raw, width.geom.unique, group.map, groups, preserve
      )
    dodge.widths <- calc_width(
      dodge.widths.raw, width.geom.unique, group.map, groups, preserve
    )
    # Adjust the width sizes based on geom.widths

    if(!is.null(geom.widths.raw)) {
      if(has.x.width) {
        width.ratio <- geom.widths[['width']] / geom.widths.raw
        xmid <- df[['xmin']] + geom.widths.raw / 2
        df[['xmin']] <- xmid - geom.widths[['width']] / 2
        df[['xmax']] <- xmid + geom.widths[['width']] / 2
      }
      if(has.width) {
        df[['width']] <- geom.widths[['width']]
      }
    }
    # Adjust positions based on dodge width.  For each group the dodge amount is
    # the difference between its middle and the middle of the total width.

    dodge.width.total <- sum(dodge.widths[['group.widths']])
    dodge.width.mid <- dodge.width.total / 2
    dodge.width.cum <- cumsum(dodge.widths[['group.widths']])
    dodge.width.lead <- c(0, head(dodge.width.cum, -1))
    dodge.width.offset.group <-
      ((dodge.width.cum + dodge.width.lead) / 2) - dodge.width.mid

    dodge.width.offset <- dodge.width.offset.group[group.map] *
      dodge.widths[['scale']]

    if(has.x.width) {
      df[['xmin']] <- df[['xmin']] + dodge.width.offset
      df[['xmax']] <- df[['xmax']] + dodge.width.offset
    }
    if("x" %in% names(df) && is.numeric(df[['x']])) {
      df[['x']] <- df[['x']] + dodge.width.offset
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
  if("ymin" %in% names(df)) df[["ymin"]] <- df[["ymin"]] + y.lag - y.orig
  if("ymax" %in% names(df)) df[["ymax"]] <- df[["ymax"]] + y.lag - y.orig
  df
}

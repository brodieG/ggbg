
#' Stack objects in a Waterfall
#'
#'
#' @inheritParams ggplot2::position_dodge
#' @inheritParams ggplot2::position_stack
#' @export
#' @param dodge TRUE or FALSE (default), whether to dodge waterfall bars when
#'   there are multiple bars for a single x value.

position_waterfall <- function(
  width = NULL,
  preserve = c("total", "single"),
  reverse = FALSE,
  dodge = TRUE
) {
  if(!is.logical(dodge) || !length(dodge) == 1 || is.na(dodge))
    stop("`dodge` should be TRUE or FALSE")

  ggproto(NULL, PositionWaterfall,
    width = width,
    preserve = match.arg(preserve),
    reverse = reverse,
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

  setup_params = function(self, data) {
    if(
      (is.null(data$xmin) || is.null(data$xmax)) &&
      is.null(data$width) &&
      is.null(self$width)
    ) {
      warning("Width not defined. Set with `position_waterfall(width = ?)`",
        call. = FALSE)
    }
    if (identical(self$preserve, "total")) {
      n <- NULL
    } else {
      n <- max(table(data$xmin))
    }
    list(
      width = self$width,
      n = n,
      dodge = self$dodge,
      reverse = self$reverse
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
        y.start=prev.last, n=list(params$n)
      )
      do.call(rbind, d.s.proc)
    }
  }
)

# Dodge overlapping interval.
# Assumes that each set has the same horizontal position.

pos_waterfall <- function(df, width, dodge, y.start, n = NULL) {
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
      stack_waterfall(df, y.start)
    } else {
      # stack mode, need to segregate positives and negatives

      rbind(
        stack_waterfall(df[df[["y"]] >= 0, , drop=FALSE], y.start),
        stack_waterfall(df[df[["y"]] < 0, , drop=FALSE], y.start)
      )
    }
  }
  df
}
stack_waterfall <- function(df, y.start) {
  y.all <- c(y.start, df[["y"]])
  y.cum <- cumsum(y.all)
  y.lead <- head(y.cum, -1L)
  y.lag <- tail(y.cum, -1L)
  df[["y"]] <- y.lag
  df[["ymin"]] <- pmin(y.lead, df[["y"]])
  df[["ymax"]] <- pmax(y.lead, df[["y"]])
  df
}

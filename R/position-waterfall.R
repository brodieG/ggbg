
#' Stack objects in a Waterfall
#'
#'
#' @inheritParams position_identity
#' @param dodge TRUE or FALSE (default), whether to dodge waterfall bars when
#'   there are multiple bars for a single x value.

position_waterfall <- function(
  width = NULL,
  preserve = c("total", "single"),
  dodge = FALSE
) {
  if(!is.logical(dodge) || !length(dodge) == 1 || is.na(dodge))
    stop("`dodge` should be TRUE or FALSE")

  ggproto(NULL, PositionWaterfall,
    width = width,
    preserve = match.arg(preserve),
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
  preserve = "total",
  dodge = FALSE,

  setup_params = function(self, data) {
    if (is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
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
      n = n
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
      if(sum(c("xmin", "xmax") %in% names(data)) != 2)
        warning(
          "Only one of `xmin` and `xmax` available to `", self$name, "`, ",
          "this may cause unexpected outcomes."
        )

      data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)

      if(!is.numeric(data[["width"]]) || isTRUE(any(data[["width"]]) < 0))
        warning(
          "Non-numeric or negative `width` values provided to `", self$name,
          "`, this may cause unexpected outcomes."
        )

      data[["xmin"]] <- data[["x"]] - data[["width"]] / 2
      data[["xmax"]] <- data[["x"]] + data[["width"]] / 2
    }
    # Deal with `y` vals

    if(
      "y" %in% names(data) && !isTRUE(all(c("ymin", "ymax") %in% names(data)))
    ) {
      if(sum(c("ymin", "ymax") %in% names(data)) != 2)
        warning(
          "Only one of `ymin` and `ymax` available to `", self$name, "`, ",
          "this may cause unexpected outcomes."
        )
      if("height" %in% names(data) || "height" %in% names(params)) {
        # height provided, so center on height; this doesn't really make that
        # much sense in the context of a waterfall chart, but we provide the
        # handling in the hopes that it does something reasonable to layers that
        # provide it
        data[["height"]] <- data[["height"]] %||% params[["height"]]
        if(!is.numeric(data[["height"]]) || !isTRUE(all(data[["height"]] >= 0)))
          warning(
            "`height` values are not positive numeric in `", self$name,
            "`, this may cause unexpected outcomes."
          )
        data[["ymin"]] <- data[["y"]] - data[["height"]] / 2
        data[["ymax"]] <- data[["y"]] + data[["height"]] / 2
      } else {
        # no height, so use y value as top/bottom of bar, anchored at zero
        data[["ymin"]] <- pmin(data[["y"]], 0)
        data[["ymax"]] <- pmax(data[["y"]], 0)
      }
    } else if (
      !"y" %in% names(data) && isTRUE(all(c("ymin", "ymax") %in% names(data)))
    ) {
      if(isTRUE(any(data[["ymin"]] > data[["ymax"]])))
        warning(
          "Some `ymin` values are greater than `ymax` values for `", self$name,
          "`, this may cause unexpected outcomes."
        )
      data[["y"]] <- (data[["ymin"]] + data[["ymax"]]) / 2
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
      # group by xmin, and then stack / dodge

      d.s <- split(data, data[["xmin"]])


    }
  }
)

# Dodge overlapping interval.
# Assumes that each set has the same horizontal position.

pos_waterfall <- function(df, width, dodge, y.start, n = NULL) {
  if (is.null(n)) n <- length(unique(df[["group"]]))
  if(!is.numeric(n) || length(n) != 1L || is.na(n) || n < 0) {
    warning(
      "Internal error: failed computing 'position_waterfall' due to ",
      "unexpected `n`, contact maintainer."
    )
    df
  } else {
    # For stacking, we need negatives and positives segregated.  We also need
    # the baseline y


  }

  d_width <- max(df$xmax - df$xmin)

  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidx <- match(df$group, sort(unique(df$group)))

  # Find the center for each group, then use that to calculate xmin and xmax
  df$x <- df$x + width * ((groupidx - 0.5) / n - .5)
  df$xmin <- df$x - d_width / n / 2
  df$xmax <- df$x + d_width / n / 2

  df
}
